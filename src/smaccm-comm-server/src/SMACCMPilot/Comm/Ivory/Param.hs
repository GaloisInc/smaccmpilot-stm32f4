{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module SMACCMPilot.Comm.Ivory.Param where

import Ivory.Language
import Ivory.Tower

data ParamWriter a =
  ParamWriter
    { pw_chan :: ChanInput a
    , pw_name :: String
    }

data ParamReader a =
  ParamReader
    { pr_chan :: ChanOutput a
    , pr_name :: String
    , pr_ival :: Init a
    }

data Param a =
  Param
    { param_writer :: ParamWriter a
    , param_reader :: ParamReader a
    }

paramReaderState :: (IvoryArea a, IvoryZero a)
                 => ParamReader a -> Monitor e (Ref Global a)
paramReaderState pr@ParamReader{..} = do
  s <- stateInit pr_name pr_ival
  paramReaderHandler pr $ do
    callback $ \v -> refCopy s v
  return s

paramReaderHandler :: (IvoryArea a, IvoryZero a)
                   => ParamReader a -> Handler a e () -> Monitor e ()
paramReaderHandler ParamReader{..} k =
  handler pr_chan (pr_name ++ "_update") k

paramWriterEmitter :: (IvoryArea a, IvoryZero a) 
                   => ParamWriter a -> Handler b e (Emitter a)
paramWriterEmitter ParamWriter{..} = emitter pw_chan 1

towerParam :: (IvoryArea a) => String -> Init a -> Tower e (Param a)
towerParam n i = do
  c <- channel
  return Param
    { param_writer = ParamWriter
      { pw_chan = fst c
      , pw_name = n
      }
    , param_reader = ParamReader
      { pr_chan = snd c
      , pr_name = n
      , pr_ival = i
      }
    }


class ParamReadable p where
  paramState   :: (IvoryArea a, IvoryZero a) => p a -> Monitor e (Ref Global a)
  paramHandler :: (IvoryArea a, IvoryZero a) => p a -> Handler a e () -> Monitor e ()

instance ParamReadable ParamReader where
  paramState = paramReaderState
  paramHandler = paramReaderHandler

instance ParamReadable Param where
  paramState = paramReaderState . param_reader
  paramHandler p k = paramReaderHandler (param_reader p) k

class ParamWritable p where
  paramEmitter :: (IvoryArea a, IvoryZero a) => p a -> Handler b e (Emitter a)

instance ParamWritable ParamWriter where
  paramEmitter = paramWriterEmitter

instance ParamWritable Param where
  paramEmitter = paramWriterEmitter . param_writer
