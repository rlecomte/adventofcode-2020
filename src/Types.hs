{-# LANGUAGE NoImplicitPrelude #-}

module Types where

import RIO
import RIO.Process

--import RIO.Text.Lazy (Text)

-- | Command line arguments
data Options = Options
  { optionsDay :: !Day,
    optionsDirEntry :: !FilePath
  }

data App = App
  { appLogFunc :: !LogFunc,
    appProcessContext :: !ProcessContext,
    appOptions :: !Options
    -- Add other app-specific configuration information here
  }

newtype Day = Day Int

newtype Input = Input Text

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x {appLogFunc = y})

instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x {appProcessContext = y})
