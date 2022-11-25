{-# LANGUAGE FlexibleContexts #-}

module Main where

import System.Directory
import Control.Monad
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Except


(//) :: Maybe a -> a -> a
Just x // _ = x
Nothing // y = y


type Env = [(String, Integer, Integer)]


main :: IO ()
main = proj1Impl "." (Just 2)


proj1Impl :: FilePath -> Maybe Int -> IO ()
proj1Impl path depth = do
  envs <- run path (depth // maxBound :: Int) []
  case envs of
      Left s -> print s
      Right (_,env) -> do
          putStrLn "Count:"
          forM_ env $ \(path, _, count) -> do
              putStrLn $ path ++ " " ++ show count
          putStrLn "Size:"
          forM_ env $ \(path, size, _) -> do
              putStrLn $ path ++ " " ++ show size

  
run :: FilePath -> Int -> Env -> IO (Either String (Integer, Env))
run path depth env = do
        runExceptT $ runStateT ev env
  where ev :: StateT Env (ExceptT String IO) Integer
        ev = findCountAndSizeOfContentInPath path  depth


findCountAndSizeOfContentInPath :: ( MonadError String m
       , MonadState Env m, MonadIO m )
      => FilePath -> Int -> m Integer
findCountAndSizeOfContentInPath path depth = do
    isDir <- liftIO $ doesDirectoryExist path
    size <- case isDir of
                True -> do
                    paths <- liftIO $ listDirectory path
                    fileAndDirsSizes <- forM paths $ \p -> do
                                            let fromStartPath = path ++ "/" ++ p
                                            pure $ if depth == 0
                                                then liftIO $ getFileSize fromStartPath
                                                else findCountAndSizeOfContentInPath fromStartPath (depth - 1)
                    size <- fmap sum (sequence fileAndDirsSizes)
                    let count = fromIntegral $ length paths
                    modify (\st -> (path, size, count) : st)
                    pure size
                False -> do
                    size <- liftIO $ getFileSize path
                    pure size
    pure size