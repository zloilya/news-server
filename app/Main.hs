module Main where

import Lib (startSever)
import Config (Config (..))
import qualified Data.ByteString as B
import qualified Data.Yaml as Y

main :: IO ()
main = do
  config <- Y.decodeThrow =<< B.readFile "src/Config.yaml"
  startSever config
