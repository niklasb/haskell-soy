{-# OPTIONS_GHC -fwarn-unused-imports -fwarn-incomplete-patterns #-}
module Soy.Sanitization where

import qualified Data.ByteString as BS
import Data.Char
import Data.Monoid
import qualified Data.Text as T
import Data.Text.Encoding
import Text.Printf

applyAll :: [a -> a] -> a -> a
applyAll fs = appEndo $ mconcat $ map Endo fs

escapeHtml :: T.Text -> T.Text
escapeHtml = applyAll $ map replaceWithEntity "<>\"'/`@ %*+-=^|&"
    where replaceWithEntity c =
                T.replace (T.singleton c) (T.pack $ printf "&#x%02X;" $ fromEnum c)

escapeUri :: T.Text -> T.Text
escapeUri = T.concatMap (T.pack . escapeChar)
    where escapeChar c
            | isAsciiUpper c || isAsciiLower c || isDigit c = [c]
            | otherwise = concat $ map (printf "%%%02X" . fromEnum)
                                 $ BS.unpack $ encodeUtf8 $ T.singleton c

escapeJs :: T.Text -> T.Text
escapeJs = T.concatMap (T.pack . escapeChar)
    where escapeChar c
            | c `elem` [' '..'~'] && c `notElem` "'\"\\" = [c]
            | c `elem` ['\0'..'\xff'] = printf "\\x%02X" $ fromEnum c
            | otherwise = printf "\\u%04X" $ fromEnum c
