module AudioDB where

import ADB
import Foreign
import System.FilePath (FilePath)

openDB :: FilePath -> (Ptr ADB)
openDB = undefined

closeDB :: (Ptr ADB) -> IO ()
closeDB = undefined

createDB :: FilePath -> (Ptr ADB)
createDB = undefined
