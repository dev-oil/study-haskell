module Main where

import qualified Domain.Todo.UseCase as TaskUseCase

main :: IO ()
main = do
  result <- TaskUseCase.createTask "Hello"
  print result