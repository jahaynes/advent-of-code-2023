module State ( State (..)
             , evalState
             , execState
             , get
             , modify
             , put
             ) where

newtype State s a =
    State { runState :: s -> (s, a) }

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put x = State $ \_ -> (x, ())

modify :: (s -> s) -> State s ()
modify f = get >>= \x -> put (f x)

evalState :: State s a -> s -> a
evalState s = snd . runState s

execState :: State s a -> s -> s
execState s = fst . runState s

instance Functor (State s) where

    fmap f (State run) = State $ \s ->
        let (s', x) = run s
        in (s', f x)

instance Applicative (State s) where

    pure x = State $ \s -> (s, x)

    sf <*> sx = State $ \s ->
        let (s',  f) = runState sf s  in
        let (s'', x) = runState sx s' in
        (s'', f x)

instance Monad (State s) where

    return = pure

    ma >>= f = State $ \s ->
        let (s', a)  = runState ma s
        in runState (f a) s'
