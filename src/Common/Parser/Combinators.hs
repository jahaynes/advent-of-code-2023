module Parser.Combinators where

import Parser (Parser (..))

import Data.Functor ((<&>), void)

many :: Parser s a -> Parser s [a]
many p = Parser (go [])
  where
  go acc s =
    case runParser p s of
      Left _ -> Right (s, reverse acc)
      Right (s', a) -> go (a:acc) s'

many_ :: Parser s a -> Parser s ()
many_ = void . many

many1 :: Parser s a -> Parser s [a]
many1 p = do
  xs <- many p
  if null xs
    then fail "none found for many1"
    else pure xs

one :: Parser [a] a
one = Parser f
  where
  f     [] = Left "no one available"
  f (x:xs) = Right (xs, x)

opt :: (a -> Bool) -> Parser [a] (Maybe a)
opt c = Parser f
    where
    f     []             = Right (  [], Nothing)
    f (x:xs) | c x       = Right (  xs, Just x)
             | otherwise = Right (x:xs, Nothing)

such :: (a -> Bool) -> Parser [a] a
such f = Parser go
  where
  go     []             = Left "out of input"
  go (x:xs) | f x       = Right (xs, x)
  go     _  | otherwise = Left "mismatch"

suchnt :: (a -> Bool) -> Parser [a] a
suchnt f = such (not . f)

pTakeWhile :: (a -> Bool) -> Parser [a] [a]
pTakeWhile p = many (such p)

pTakeWhile1 :: (a -> Bool) -> Parser [a] [a]
pTakeWhile1 p = many1 (such p)

pDropWhile :: (a -> Bool) -> Parser [a] ()
pDropWhile p = many (such p) <&> \_ -> ()

pDropWhile1 :: (a -> Bool) -> Parser [a] ()
pDropWhile1 p = many1 (such p) <&> \_ -> ()

list :: Eq a => [a] -> Parser [a] [a]
list xs = Parser $ \s ->
  let len = length xs
  in
  if xs == take len s
    then Right (drop len s, xs)
    else Left "string mismatch"

remainder :: Parser [a] [a]
remainder = Parser $ \s -> Right ([], s)

eof :: Parser [a] ()
eof = Parser go
  where
  go [] = Right ([], ())
  go  _ = Left "not eof"

ok :: Parser a ()
ok = pure ()

sepBy :: Eq c => [c] -> Parser [c] a -> Parser [c] [a]
sepBy sep p = do
    x  <- p
    xs <- many (list sep *> p)
    pure (x:xs)
