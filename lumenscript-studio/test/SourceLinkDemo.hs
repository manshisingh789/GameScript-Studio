-- | stack runghc -- -isrc -itest test/SourceLinkDemo.hs [click-kind] [id|line col]
--   kinds: token N | ast N | code N | cursor LINE COL
module Main (main) where

import System.Environment (getArgs)
import SourceLink
import SourceLinkFixture

main :: IO ()
main = do
  args <- getArgs
  let sel = case args of
        ["token", n]        -> SelToken (read n)
        ["ast",   n]        -> SelAst   (read n)
        ["code",  n]        -> SelCode  (read n)
        ["cursor", l, c]    -> let o = offsetFromLineCol demoSource (read l) (read c)
                               in SelSource (Span o o)
        _                   -> SelToken 3
  putStrLn ("Selection: " ++ show sel ++ "\n")
  putStr (renderReport demoTable (resolve demoTable sel))