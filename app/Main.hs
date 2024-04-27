module Main where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text.IO.Utf8 as TIO
import Data.Text.Lazy.Encoding (encodeUtf8)
import QCL

main :: IO ()
main = do
  text <- TIO.getContents
  case evalQCL text of
    Right value -> BL.putStrLn (Aeson.encode value)
    Left e -> BL.putStrLn (encodeUtf8 e)
