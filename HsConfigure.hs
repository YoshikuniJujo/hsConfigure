{-# LANGUAGE ScopedTypeVariables #-}

module HsConfigure (
  runUsersEx
) where

import Control.Exception

import System.Environment   (getArgs)
import System.IO            (openFile, hClose, IOMode(WriteMode, AppendMode), hPrint)
import System.Directory     (doesDirectoryExist, createDirectory, copyFile, doesFileExist,
                             getAppUserDataDirectory, getModificationTime)
import System.FilePath      (splitPath)
import System.Process       (runProcess, waitForProcess)
import System.Posix.Process (executeFile)
import System.Info          (os, arch)
import Control.Monad        (when, unless)
import Control.Exception    (bracket)
import Control.Applicative  ((<$>))
import Data.Version         (Version, showVersion)

runUsersEx :: String -> Maybe Version -> Maybe FilePath -> IO () -> IO ()
runUsersEx prjName ver src defaultAction
  = do errFile <- getAppUserDataDirectory prjName >>= return . addErrSuffix . mkBaseFile prjName
       catch (buildLaunch prjName ver src) $
         \(err :: IOException) -> bracket (openFile errFile AppendMode) hClose (flip hPrint err)
       defaultAction

buildLaunch :: String -> Maybe Version -> Maybe FilePath -> IO ()
buildLaunch prjName ver src = do
  whenMaybe src $ initAppUserDataDirectory prjName ver
  recompile prjName ver
  exec <- getAppUserDataDirectory prjName >>= return . addExecuteSuffix . mkBaseFile prjName
  args <- getArgs
  executeFile exec False args Nothing

initAppUserDataDirectory :: String -> Maybe Version -> FilePath -> IO ()
initAppUserDataDirectory prjName ver defSrc = do
  dir     <- getAppUserDataDirectory prjName
  let src  = addSrcSuffix ver $ mkBaseFile prjName dir
  unlessM (doesDirectoryExist dir) $ createDirectory dir
  unlessM (doesFileExist src)      $ copyFile defSrc src

recompile :: String -> Maybe Version -> IO ()
recompile prjName ver = do
  dir  <- getAppUserDataDirectory prjName
  let base = mkBaseFile prjName dir
      bin  = addExecuteSuffix base
      src  = addSrcSuffix ver base
      err  = addErrSuffix base
  srcT <- getModTime src
  binT <- getModTime bin
  when (srcT > binT) $ do
    bracket (openFile err WriteMode) hClose $ \h -> do
      waitForProcess =<<
        runProcess "ghc" ["--make", basename src, "-i", "-o", bin]
                   (Just dir) Nothing Nothing Nothing (Just h)
    return ()
  where getModTime f = catch (Just <$> getModificationTime f) (\(e :: IOException) -> return Nothing)
        basename     = last . splitPath

mkBaseFile :: String -> String -> String
mkBaseFile prjName = (++ "/" ++ prjName)

addExecuteSuffix, addErrSuffix :: String -> String
addSrcSuffix                   :: Maybe Version -> String -> String
addExecuteSuffix      = (++ "-" ++ arch ++ "-" ++ os)
addErrSuffix          = (++ ".error")
addSrcSuffix Nothing  = (++ ".hs")
addSrcSuffix (Just v) = (++ "-" ++ showVersion v ++ ".hs")

whenMaybe :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenMaybe Nothing  _ = return ()
whenMaybe (Just x) f = f x

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM mp s = mp >>= flip unless s
