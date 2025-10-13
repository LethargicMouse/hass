module Enclosed (
  enclosed,
) where

enclosed :: String -> String -> String
enclosed c s = c ++ s ++ reverse (inverse <$> c)

inverse :: Char -> Char
inverse c = case c of
  '(' -> ')'
  ')' -> '('
  '[' -> ']'
  ']' -> '['
  '{' -> '}'
  '}' -> '{'
  _ -> c
