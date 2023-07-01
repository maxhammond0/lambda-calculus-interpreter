module Main where

import Data.Maybe
import System.Environment
import Text.Parsec
import Text.Parsec.String
import Control.Monad

type Name = String

-- Lambda calculus definitions
data Expression
  = Application Expression Expression
  | Function Name Expression
  | Variable Name
  deriving (Eq, Show)

evaluate :: Expression -> Expression
evaluate = undefined

parseExpression :: Parser Expression
parseExpression =
  try parseFunction
    <|> try parseApplication
    <|> try parseVariable

parseApplication :: Parser Expression
parseApplication =
  Application
    <$> parseExpression
    <*> parseExpression

-- \x.x
parseFunction :: Parser Expression
parseFunction =
  Function
    <$> ( string "\\"
            >> many spaces
            >> parseVariable
            >> many spaces
            >> string "."
        )
    <*> parseExpression

parseVariable :: Parser Expression
parseVariable =
  Variable
    <$> 


-- IO --
encode :: Expression -> String
encode = go where
  go (Variable n) = n
  go (Function n b@(Function _ _)) = "\\ " ++ n ++ tail (go b)
  go (Function n b) = "\\ " ++ n ++ " . " ++ go b
  go (Application e@(Application _ _) f) = go e ++ " " ++ wrap f
  go (Application e f) = wrap e ++ " " ++ wrap f

  wrap x@(Variable _) = go x
  wrap x@(Function _ _) = "(" ++ go x ++ ")"
  wrap x@(Application _ _) = "(" ++ go x ++ ")"

putExpr :: Expression -> IO ()
putExpr = putStr . encode

putExprLn :: Expression -> IO ()
putExprLn = putStrLn . encode

main :: IO ()
main = do
  args <- getArgs
  case args of
    [src] -> undefined -- TODO
    _ -> putStrLn "Incorrect usage"
