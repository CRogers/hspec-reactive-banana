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

shouldHaveInitialState :: (Show b, Eq b) => (forall t. Event t a -> Behavior t b) -> b -> Expectation
shouldHaveInitialState behavior expected = do
    result <- interpretBehavior behavior []
    fst result `shouldBe` expected

class WithEvents w where
    withEvents :: (forall t. Event t a -> w t b) -> [a] -> IO [[b]]

instance WithEvents Event where
    withEvents = interpretEvent

instance WithEvents Behavior where
    withEvents f = fmap snd . interpretBehavior f

shouldProduce :: (Show b, Eq b) => IO [[b]] -> [[b]] -> Expectation
shouldProduce mactual expected = do
    actual <- mactual
    actual `shouldBe` expected

test = diamond `withEvents` [1] `shouldProduce` [[2,2]]

count :: Event t a -> Behavior t Int
count = accumB 0 . fmap (const (+1))

diamond :: Event t a -> Event t a
diamond e = union e e