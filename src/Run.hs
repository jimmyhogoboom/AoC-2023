{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import Import
import Day1

day1 :: RIO App (String, String)
day1 = do
  p1 <- liftIO Day1.part1
  p2 <- liftIO Day1.part2
  return (p1, p2)

run :: RIO App ()
run = do
  (p1, p2) <- day1
  logInfo $ displayShow [("1.1", p1), ("1.2", p2)]
