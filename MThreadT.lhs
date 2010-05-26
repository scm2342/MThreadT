\documentclass{article}
%include lhs2TeX.fmt
\begin{document}
Monad Transformer for synchronizing Threads wenn leave the Transformer

\begin{code}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module MThreadT (MThreadT, runMThreadT, mForkIO) where

import Control.Monad.State(StateT,runStateT,liftIO,MonadFix,get,MonadIO,MonadState,put,MonadPlus)
import Control.Concurrent(putMVar,forkIO,MVar,newEmptyMVar,ThreadId,takeMVar)
import Control.Exception(finally)
\end{code}

We just warp the StateT Transformer. Conceptionelly we do not need a standalone Monad since we nee IO anyway...

\begin{code}
newtype MThreadT m a = MThreadT { unpackMThreadT :: StateT [MVar ()] m a } deriving (Monad, Functor, MonadFix, MonadPlus, MonadIO, MonadState [MVar()])
\end{code}

Run Managed Threads...

\begin{code}
runMThreadT ::  (Monad m, MonadIO m) => MThreadT m a -> m a
runMThreadT m = do
    (a, threadlist) <- runStateT (unpackMThreadT m) []
    liftIO . waitOnMVarList $ threadlist
    return a

waitOnMVarList ::  [MVar b] -> IO ()
waitOnMVarList = mapM_ takeMVar
\end{code}

Start a Managed Thread - other than that the same as forkIO is...

\begin{code}
mForkIO ::  (Monad m, MonadIO m) => IO () -> MThreadT m ThreadId
mForkIO io = do
    m <- liftIO newEmptyMVar
    threadlist <- get
    put (m:threadlist)
    liftIO . forkIO $ (io `finally` putMVar m ())
\end{code}

\end{document}
