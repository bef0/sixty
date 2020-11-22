{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Command.Compile where

import Protolude hiding (withAsync, wait)

import Control.Concurrent.Async.Lifted.Safe
import qualified Data.HashSet as HashSet
import qualified Data.Text as Text
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text (putDoc)
import Data.Time.Clock
import LLVM.Pretty ()
import Rock

import qualified Core.Pretty as Pretty
import qualified Core.Syntax as Syntax
import qualified Driver
import qualified Error.Hydrated
import qualified Name
import qualified Project
import qualified Query

compile :: [FilePath] -> IO ()
compile argumentFiles = do
  startTime <- getCurrentTime
  (sourceDirectories, filePaths) <- Project.filesFromArguments argumentFiles
  ((), errs) <- Driver.runTask sourceDirectories filePaths Error.Hydrated.pretty $
    forM_ filePaths $ \filePath -> do
      (module_, _, defs) <- fetch $ Query.ParsedFile filePath
      let
        names =
          HashSet.fromList $
            Name.Qualified module_ . fst . snd <$> defs
      liftIO $ putDoc $ "module" <+> pretty module_ <> line <> line
      forM_ names $ \name -> do
        cc <- fetch $ Query.ClosureConverted $ Name.Lifted name 0
        liftIO $ print cc
        assembly <- fetch $ Query.Assembly $ Name.Lifted name 0
        cpsAssembly <- fetch $ Query.CPSAssembly $ Name.Lifted name 0
        -- llvm <- fetch $ Query.LLVM $ Name.Lifted name 0
        -- liftIO $ putDoc $ pretty assembly
        liftIO $ putDoc $ pretty cpsAssembly
        -- liftIO $ putDoc $ pretty llvm
        putText ""
  endTime <- getCurrentTime
  forM_ errs $ \err ->
    putDoc $ err <> line
  let
    errorCount =
      length errs
  putText $ Text.unwords
    [ "Found"
    , show errorCount
    , case errorCount of
      1 -> "error"
      _ -> "errors"
    , "in"
    , show (diffUTCTime endTime startTime) <> "."
    ]
  unless (null errs)
    exitFailure
