module Main where

import Blackjack
import Control.Monad.State (execStateT, liftIO)

azino777 :: BJ ()
azino777 = do
  newGame
  round1
  advance

main :: IO ()
main = do
  putStrLn "Hi, I'm callmecabman and welcome to whitejack"
  liftIO $ execStateT azino777 undefined
  return ()
