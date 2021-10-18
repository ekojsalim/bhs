module Main where

import LispVal
import Eval
import Control.Monad (forever)
import qualified Data.Text as T

c :: LispVal
c = Number 2

main :: IO ()
main = forever $ do
  putStr "> "
  a <- getLine
  res <- runASTinEnv basicEnv $ textToEvalForm $ T.pack a
  print res