module StatefulUnsafeMonad where
import Control.Monad(ap)

data Unsafe a = Error String | Ok a deriving (Show, Eq)

data StatefulUnsafe s a  = StatefulUnsafe (s -> (Unsafe a,s))

app :: StatefulUnsafe s a -> (s ->(Unsafe a,s))
app (StatefulUnsafe stateful) = stateful

err :: String -> StatefulUnsafe e a
err s = StatefulUnsafe $ \ state -> (Error s, state)

instance Functor (StatefulUnsafe s) where
  fmap f (StatefulUnsafe sa) =  StatefulUnsafe $ \ state -> case sa state of
                                                              (Ok a, output) -> (Ok $f a, output)
                                                              (Error e, output) -> (Error e, output)

instance Applicative (StatefulUnsafe s) where
  pure = return
  (<*>) = ap

instance Monad (StatefulUnsafe s) where
  return a = StatefulUnsafe $ \ state -> (Ok a, state)
  (StatefulUnsafe sa) >>= f = StatefulUnsafe $ \ state -> case sa state of
                                                           (Ok a, output) -> app (f a) output
                                                           (Error e, output) -> (Error e, output)

get :: StatefulUnsafe s s
get = StatefulUnsafe $ \ s ->  (Ok s,s)

put :: s -> StatefulUnsafe s ()
put s = StatefulUnsafe $ \ _ ->  (Ok (),s)
