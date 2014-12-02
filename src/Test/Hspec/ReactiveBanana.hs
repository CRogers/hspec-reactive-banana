{-# LANGUAGE Rank2Types #-}

module Test.Hspec.ReactiveBanana where

import Test.Hspec
import Reactive.Banana
import Reactive.Banana.Frameworks

import Data.IORef
import Control.Monad (forM)

interpretEvent :: (forall t. Event t a -> Event t b) -> [a] -> IO [[b]]
interpretEvent = interpretFrameworks

interpretBehavior :: (forall t. Event t a -> Behavior t b) -> [a] -> IO (b, [[b]])
interpretBehavior f as = do
    output <- newIORef []
    init <- newIORef undefined
    (addHandler, runHandlers) <- newAddHandler

    network <- compile $ do
        e <- fromAddHandler addHandler
        o <- changes $ f e
        i <- initial $ f e
        liftIO $ writeIORef init i
        reactimate' $ (fmap . fmap) (\b -> modifyIORef output (++[b])) o

    actuate network
    bs <- forM as $ \a -> do
        runHandlers a
        bs <- readIORef output
        writeIORef output []
        return bs
    i <- readIORef init
    return (i, bs)