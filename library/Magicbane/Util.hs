{-# LANGUAGE OverloadedStrings, UnicodeSyntax, DataKinds, FlexibleContexts #-}

-- | Various useful functions and type definitions.
module Magicbane.Util where

import           RIO (MonadThrow, throwM, MonadIO, liftIO)
import           Control.Arrow
import           Control.Monad
import           Control.Error (hush)
import qualified System.IO
import qualified Data.ByteString.Lazy as L (ByteString)
import qualified Data.HashMap.Strict as HMS
import qualified Data.Sequences as S
import qualified Data.Text as T
import           Data.Text (Text)
import           Data.Char (isSpace)
import           Data.Foldable
import           Data.HashMap.Strict (insertWith)
import           Data.Maybe
import           Data.MonoTraversable
import           Data.String.Conversions
import           Data.String.Conversions.Monomorphic
import           Data.Attoparsec.Text as AP
import           Data.Aeson
import           Network.URI
import           Network.HTTP.Types (hContentType)
import qualified Network.HTTP.Link
import           Web.FormUrlEncoded
import           Servant

type Host = Header "Host" Text
type Form = ReqBody '[FormUrlEncoded] [(Text, Text)]

type HTTPLink = Network.HTTP.Link.Link
type WithLink α = (Headers '[Header "Link" [HTTPLink]] α)

hPutStrLn ∷ MonadIO μ ⇒ System.IO.Handle → String → μ ()
hPutStrLn h s = liftIO $ System.IO.hPutStrLn h s

-- | Merges two JSON objects recursively. When the values are not objects, just returns the left one.
mergeVal ∷ Value → Value → Value
mergeVal (Object x) (Object y) = Object $ HMS.unionWith mergeVal x y
mergeVal x _ = x

-- | Encodes key-value data as application/x-www-form-urlencoded.
writeForm ∷ (ConvertibleStrings α Text, ConvertibleStrings β Text, ConvertibleStrings L.ByteString γ) ⇒ [(α, β)] → γ
writeForm = fromLBS . mimeRender (Proxy ∷ Proxy FormUrlEncoded) . map (toST *** toST)

-- | Decodes key-value data from application/x-www-form-urlencoded.
readForm ∷ (ConvertibleStrings Text α, ConvertibleStrings Text β, ConvertibleStrings γ L.ByteString) ⇒ γ → Maybe [(α, β)]
readForm x = map (fromST *** fromST) <$> hush (mimeUnrender (Proxy ∷ Proxy FormUrlEncoded) $ toLBS x)

-- | Reads a Servant incoming form as a list of key-value pairs (for use in FromForm instances).
formList ∷ Web.FormUrlEncoded.Form → [(Text, Text)]
formList = fromMaybe [] . hush . fromForm

-- | Converts a flat key-value form with keys in typical nesting syntax (e.g. "one[two][three]") to an Aeson Value with nesting (for use in FromForm instances).
formToObject ∷ [(Text, Text)] → Value
formToObject f = foldl' assignProp (object []) $ (map . first) parseKey f
  where parseKey x = fromMaybe [ x ] $ hush $ parseOnly formKey x
        assignProp (Object o) ([k], v) = Object $ insertWith concatJSON k (toJSON [ v ]) o
        assignProp (Object o) (k : k' : ks, v) = Object $ insertWith (\_ o' → assignProp o' (k' : ks, v)) k (assignProp (object []) (k' : ks, v)) o
        assignProp x _ = x
        concatJSON (Array v1) (Array v2) = Array $ v1 <> v2
        concatJSON (Array v1) _ = Array v1
        concatJSON _ (Array v2) = Array v2
        concatJSON _ _ = Null

formKey ∷ Parser [Text]
formKey = do
  firstKey ← AP.takeWhile (/= '[')
  restKeys ← many' $ do
    void $ char '['
    s ← AP.takeWhile (/= ']')
    void $ char ']'
    return s
  void $ option '_' $ char '[' >> char ']'
  return $ firstKey : S.filter (not . T.null) restKeys

-- | Parses any string into a URI.
parseUri ∷ ConvertibleStrings α String ⇒ α → URI
parseUri = fromJust . parseURI . cs

-- | Prepares text for inclusion in a URL.
--
-- >>> :set -XOverloadedStrings
-- >>> slugify "Hello & World!"
-- "hello-and-world"
slugify ∷ Text → Text
slugify = T.filter (not . isSpace) . T.intercalate "-" . T.words .
          T.replace "&" "and"  . T.replace "+" "plus" . T.replace "%" "percent" .
          T.replace "<" "lt"   . T.replace ">" "gt"   . T.replace "=" "eq" .
          T.replace "#" "hash" . T.replace "@" "at"   . T.replace "$" "dollar" .
          T.filter (`onotElem` ("!^*?()[]{}`./\\'\"~|"::String)) .
          T.toLower . T.strip

-- | Creates a simple text/plain ServerError
errText ∷ ServerError → L.ByteString → ServerError
errText e t = e { errHeaders = [ (hContentType, "text/plain; charset=utf-8") ]
                , errBody    = t }

-- | Creates and throws a simple text/plain ServerError.
throwErrText ∷ MonadThrow μ ⇒ ServerError → L.ByteString → μ α
throwErrText e t = throwM $ errText e t
