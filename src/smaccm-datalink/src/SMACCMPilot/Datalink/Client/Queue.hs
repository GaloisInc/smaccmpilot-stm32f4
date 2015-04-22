module SMACCMPilot.Datalink.Client.Queue
  ( Poppable
  , unPoppable
  , Pushable
  , unPushable
  , newQueue
  , queuePop
  , queueTryPop
  , queuePush
  , forkPop
  , popProducer
  , pushConsumer
  ) where

import Control.Monad
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Pipes

newtype Poppable a = Poppable { unPoppable :: TQueue a }
newtype Pushable a = Pushable { unPushable :: TQueue a }

newQueue :: IO (Pushable a, Poppable a)
newQueue = newTQueueIO >>= \q -> return (Pushable q, Poppable q)

forkPop :: Poppable a -> IO (Poppable a, Poppable a)
forkPop q = do
  (q1o, q1i) <- newQueue
  (q2o, q2i) <- newQueue
  void $ forkIO $ forever $ do
    v <- queuePop q
    queuePush q1o v
    queuePush q2o v
  return (q1i, q2i)

queuePop :: Poppable a -> IO a
queuePop q = atomically (readTQueue (unPoppable q))

queueTryPop :: Poppable a -> IO (Maybe a)
queueTryPop q = atomically $ do
  e <- isEmptyTQueue tq
  case e of
    True -> return Nothing
    False -> fmap Just (readTQueue tq)
  where tq = unPoppable q

queuePush :: Pushable a -> a -> IO ()
queuePush q v = void (atomically (writeTQueue (unPushable q) v))

popProducer :: (MonadIO m) => Poppable a -> Producer a m ()
popProducer q = do
  v <- liftIO (queuePop q)
  yield v
  popProducer q

pushConsumer :: (MonadIO m) => Pushable a -> Consumer a m ()
pushConsumer q = do
  v <- await
  liftIO (queuePush q v)
  pushConsumer q
