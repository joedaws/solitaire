module Game.Solitaire.Persist (saveState, loadState, getSavePath) where

import Data.Binary (Binary (..), decodeFile, encodeFile)
import System.Directory (XdgDirectory (..), createDirectoryIfMissing, getXdgDirectory)
import System.FilePath ((</>))

import Game.Solitaire.State (Solitaire (..))

saveState :: (Binary c) => Solitaire c -> IO ()
saveState game = getSavePath >>= \p -> encodeFile p game

loadState :: (Binary c) => IO (Solitaire c)
loadState = getSavePath >>= decodeFile

getSavePath :: IO FilePath
getSavePath = do
    dir <- getXdgDirectory XdgData "solitaire"
    createDirectoryIfMissing True dir
    pure $ dir </> "solitaire.save"
