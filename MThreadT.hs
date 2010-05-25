{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module MThreadT (MThreadT, initMThreadT, mForkIO) where

import Control.Monad.State(StateT,runStateT,liftIO,MonadFix,get,MonadIO,MonadState,put,MonadPlus)
import Control.Concurrent(putMVar,forkIO,MVar,newEmptyMVar,ThreadId,takeMVar)
import Control.Exception(finally)

newtype MThreadT m a = MThreadT { runMThreadT :: StateT [MVar ()] m a } deriving (Monad, Functor, MonadFix, MonadPlus, MonadIO, MonadState [MVar()])

initMThreadT ::  (Monad m, MonadIO m) => MThreadT m a -> m a
initMThreadT m = do
    (a, threadlist) <- runStateT (runMThreadT m) []
    liftIO . waitOnMVarList $ threadlist
    return a

waitOnMVarList ::  [MVar b] -> IO ()
waitOnMVarList = mapM_ takeMVar

mForkIO ::  (Monad m, MonadIO m) => IO () -> MThreadT m ThreadId
mForkIO io = do
    m <- liftIO newEmptyMVar
    threadlist <- get
    put (m:threadlist)
    liftIO . forkIO $ (io `finally` putMVar m ())
