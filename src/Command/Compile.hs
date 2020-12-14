{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Command.Compile where

import Protolude hiding (withAsync, wait, moduleName)

import Control.Concurrent.Async.Lifted.Safe
import qualified Data.ByteString.Short as ShortByteString
import qualified Data.HashSet as HashSet
import qualified Data.Text as Text
import qualified Data.Text.Lazy.IO as Lazy
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text (putDoc)
import Data.Time.Clock
import qualified LLVM.AST
import LLVM.Pretty (ppllvm)
import Rock
import System.FilePath

import qualified Core.Pretty as Pretty
import qualified Paths_sixty as Paths
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
  ((), errs) <- Driver.runTask sourceDirectories filePaths Error.Hydrated.pretty $ do
    forwardDeclarationsFile <- liftIO $ Paths.getDataFileName "rts/forward_declarations.ll"
    forwardDeclarations <- liftIO $ Lazy.readFile forwardDeclarationsFile
    forM_ filePaths $ \filePath -> do
      (moduleName@(Name.Module moduleNameText), _, defs) <- fetch $ Query.ParsedFile filePath
      let
        names =
          HashSet.fromList $
            Name.Qualified moduleName . fst . snd <$> defs
      liftIO $ putDoc $ "module" <+> pretty moduleName <> line <> line
      llvmDefinitions <- forM (toList names) $ \name -> do
        cc <- fetch $ Query.ClosureConverted $ Name.Lifted name 0
        liftIO $ print cc
        assembly <- fetch $ Query.Assembly $ Name.Lifted name 0
        cpsAssembly <- fetch $ Query.CPSAssembly $ Name.Lifted name 0
        llvm <- fetch $ Query.LLVM $ Name.Lifted name 0
        liftIO $ putDoc $ pretty assembly <> line
        putText ""
        liftIO $ putDoc $ pretty cpsAssembly <> line
        putText ""
        liftIO $ putDoc $ pretty llvm
        putText ""
        return llvm
      let
        outputFileName =
          replaceBaseName (replaceExtension filePath "ll") ("out" </> takeBaseName filePath)
        llvmModule =
          ppllvm
            LLVM.AST.Module
              { moduleName = ShortByteString.toShort $ toUtf8 moduleNameText
              , moduleSourceFileName = ShortByteString.toShort $ toUtf8 outputFileName
              , moduleDataLayout = Nothing
              , moduleTargetTriple = Nothing
              , moduleDefinitions = concat llvmDefinitions
              }

      putStrLn llvmModule
      liftIO $ Lazy.writeFile outputFileName $ forwardDeclarations <> llvmModule
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
