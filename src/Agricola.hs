module Main where

-- import Haste

data Color = Red | Blue



data Piece = Worker Color | Border | Animal | Resource | Trough

main :: IO()
main = do
  putStrLn "Hello, baby"
