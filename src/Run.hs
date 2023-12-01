{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import Import
import Day1

run :: RIO App ()
run = do
  logInfo "We're inside the application!"
  day1 <- liftIO Day1.part1
  logInfo $ displayShow day1
  -- putStrLn day1
