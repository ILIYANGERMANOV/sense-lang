{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}

module Parser where

import Control.Monad

-- Motivated by FUNCTIONAL PEARL
-- Monadic parsing in Haskell
-- by Graham Hutton & Erik Meijer

newtype Parser a = Parser (String -> [(a, String)])
  deriving (Functor)

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure a = Parser (\cs -> [(a, cs)])

  -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  -- TODO: may not be implemented properly
  pf <*> p =
    Parser
      ( \cs ->
          [ (f a, cs'')
            | (a, cs') <- parse p cs,
              (f, cs'') <- parse pf cs'
          ]
      )

instance Monad Parser where
  -- return :: a -> Parser a
  return a = Parser (\cs -> [(a, cs)])

  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f =
    Parser
      ( \cs ->
          concat
            [ parse (f a) cs'
              | (a, cs') <- parse p cs
            ]
      )

(<|>) :: Parser a -> Parser a -> Parser a
p <|> q =
  Parser
    ( \cs -> do
        case parse p cs of
          [] -> parse q cs
          res -> res
    )

combine :: Parser a -> Parser a -> Parser a
p `combine` q =
  Parser
    ( \cs -> do
        let ps = parse p cs
        let qs = parse q cs
        ps ++ qs
    )

(+++) :: Parser a -> Parser a -> Parser a
p +++ q =
  Parser
    ( \cs -> case parse (p `combine` q) cs of
        [] -> []
        (x : xs) -> [x]
    )

empty :: Parser a
empty = Parser (const [])

parse :: Parser a -> (String -> [(a, String)])
parse (Parser p) = p

-- END DEFINITON

item :: Parser Char
item =
  Parser
    ( \cs -> case cs of
        "" -> []
        (c : cs) -> [(c, cs)]
    )

-- satisfies the given predicate
sat :: (Char -> Bool) -> Parser Char
sat p = do
  c <- item
  if p c then return c else empty

char :: Char -> Parser Char
char c = sat (c ==)

string :: String -> Parser String
string "" = return ""
string (c : cs) = do
  char c
  string cs
  return (c : cs)

consecutive :: Parser a -> Parser [a]
consecutive p = consecutive' p +++ return []

consecutive' :: Parser a -> Parser [a]
consecutive' p = do a <- p; as <- consecutive p; return (a : as)