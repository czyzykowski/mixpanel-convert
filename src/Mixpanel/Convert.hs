{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

module Mixpanel.Convert where

import           Prelude hiding (readFile, writeFile)
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.HashMap.Strict        as M
import qualified Data.HashSet               as S
import qualified Data.Text                  as T


class Monad m => MonadFS m where
  readFile  :: FilePath -> m BL.ByteString
  writeFile :: FilePath -> BL.ByteString -> m ()


instance MonadFS IO where
  readFile  = BL.readFile
  writeFile = BL.writeFile


type Properties = M.HashMap T.Text Value


toLowerKeys :: Properties -> Properties
toLowerKeys m = M.fromListWith merge (map (\(k, v) -> (T.toLower k, v)) $ M.toList m)
  where merge Null v = v
        merge v _    = v


convert' :: Properties -> Maybe Value -> Maybe Value
convert' empty' (Just (Object o)) = do
  (Object p) <- M.lookup "properties" o
  event      <- M.lookup "event" o
  return $ Object $ toLowerKeys (M.insert "event" event (M.union p empty'))
convert' _ _                      = Nothing


convert :: Properties -> BL.ByteString -> BL.ByteString
convert empty' bytes = maybe "" encode (convert' empty' decoded)
  where decoded = decode bytes


toKeys :: Maybe Value -> [T.Text]
toKeys (Just (Object o)) =
  case M.lookup "properties" o of
    Just (Object p) -> ("event" : M.keys p)
    _               -> []
toKeys _                 = []


lines' :: BL.ByteString -> [BL.ByteString]
lines' = BL.split '\n'


unlines' :: [BL.ByteString] -> BL.ByteString
unlines' = BL.intercalate "\n"


gatherKeys' :: BL.ByteString -> S.HashSet T.Text
gatherKeys' content = foldr (S.union . S.fromList) S.empty listOfKeys
  where
    inputLines = lines' content
    listOfKeys = map (toKeys . decode) inputLines


gatherKeys :: MonadFS m => FilePath -> m (S.HashSet T.Text)
gatherKeys path = do
  content <- readFile path
  return $ gatherKeys' content


run :: MonadFS m => (FilePath, FilePath) -> m ()
run (inputPath, outputPath) = do
  keys         <- gatherKeys inputPath
  inputContent <- readFile inputPath

  let empty'      = M.fromList $ zip (S.toList keys) (repeat Null)
  let inputLines  = lines' inputContent
  let outputLines = map (convert empty') inputLines

  writeFile outputPath (unlines' outputLines)
