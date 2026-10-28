-- | Terminal view of the engine API explorer (uses the real HostBindings):
--
--   stack runghc -- -isrc -itest test/ApiDemo.hs                 -- whole catalog
--   stack runghc -- -isrc -itest test/ApiDemo.hs jump            -- search
--   stack runghc -- -isrc -itest test/ApiDemo.hs --complete player.ju
module Main (main) where

import System.Environment (getArgs)

import ApiCatalog
import Simulation.GridWorld (GridWorld, initGridWorld)

main :: IO ()
main = do
  args <- getArgs
  let world = initGridWorld 10 8
  case args of
    ["--complete", t] -> do
      putStrLn ("completions for \"" ++ t ++ "\":")
      mapM_ (\(l, d) -> putStrLn ("  " ++ l ++ "    " ++ d)) (completions t)
    [q] -> printObjects world (searchApi q)
    _   -> printObjects world apiCatalog

printObjects :: GridWorld -> [ApiObject] -> IO ()
printObjects _ [] = putStrLn "(no API members match)"
printObjects world os = mapM_ obj os
  where
    obj o = do
      putStrLn ("== " ++ objectTitle o ++ " ==  " ++ aoDoc o)
      mapM_ member (aoMembers o)
      putStrLn ""
    member m = do
      putStrLn ("  [" ++ kindLabel (amKind m) ++ "] " ++ signature m
                ++ maybe "" (\v -> "    = " ++ v ++ " (live)") (liveValue world m))
      putStrLn ("      " ++ amDoc m)
      putStrLn ("      insert: " ++ insertText m)