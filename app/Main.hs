{-# LANGUAGE OverloadedStrings #-}

module Main where

import Options.Applicative
import Mixpanel.Convert


main :: IO ()
main = execParser (info (helper <*> options) fullDesc) >>= run
  where
    options = (\input output -> (input, output)) -- can be replaced by (,)
              <$> strOption (long "input"  <> help "Input file")
              <*> strOption (long "output" <> help "Output file")
