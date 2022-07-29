{-# LANGUAGE DeriveFunctor #-}

module Parser where

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

parse :: Parser a -> (String -> [(a, String)])
parse (Parser p) = p