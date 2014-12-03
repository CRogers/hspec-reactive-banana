{-# LANGUAGE Rank2Types, TypeFamilies #-}

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
    type Out w b
    withEvents :: (forall t. Event t a -> w t b) -> [a] -> IO (Out w b)

instance WithEvents Event where
    type Out Event b = [[b]]
    withEvents = interpretEvent

instance WithEvents Behavior where
    type Out Behavior b = [b]
    withEvents f = fmap (map head . snd) . interpretBehavior f

shouldProduce :: (Show b, Eq b) => IO [b] -> [b] -> Expectation
shouldProduce mactual expected = mactual >>= (`shouldBe` expected)

shouldHaveEndState :: (Show b, Eq b) => IO [b] -> b -> Expectation
shouldHaveEndState mactual expected = do
    actual <- mactual
    last actual `shouldBe` expected

test = count `withEvents` [()] `shouldHaveEndState` 11

count :: Event t a -> Behavior t Int
count = accumB 0 . fmap (const (+1))

dup :: Event t a -> Event t a
dup e = union e e