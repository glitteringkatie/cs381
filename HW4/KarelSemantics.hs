-- Katie Hughes
-- Andrea Baldwin
-- Mikky Cecil

module KarelSemantics where

import Prelude hiding (Either(..))
import Data.Function (fix)

import KarelSyntax
import KarelState


-- | Valuation function for Test.
test :: Test -> World -> Robot -> Bool
test (Not t)    w r = not (test t w r)
test (Facing c) _ r = getFacing r == c
test (Clear d)  w r = isClear (relativePos d r) w
test Beeper     w r = hasBeeper (getPos r) w
test Empty      _ r = isEmpty r

-- | Valuation function for Stmt.
stmt :: Stmt -> Defs -> World -> Robot -> Result
stmt Shutdown   _ _ r = Done r
stmt Move       _ w r = let f = getFacing r; p = getPos r; n = (neighbor f p)
                        in case isClear n w of
                          True  -> OK w (setPos n r)
                          False -> Error ("Blocked at: " ++ show n)
stmt PickBeeper _ w r = let p = getPos r
                        in if hasBeeper p w
                          then OK (decBeeper p w) (incBag r)
                          else Error ("No beeper to pick at: " ++ show p)
--Note: there can be more than one beeper in a spot.
--We don't need to check if it's a wall because the robot can't walk to a wall spot.
stmt PutBeeper     _ w r = let p = getPos r
                           in case isEmpty r of
                             True  -> Error ("No beeper to put.")
                             False -> OK (incBeeper p w) (decBag r)
stmt (Turn d)   _ w r = let f = getFacing r
                        in OK w (setFacing (cardTurn d f) r)
stmt (Block ss) d w r = stmts ss d w r
stmt (If c x y) d w r = if test c w r == True 
                        then stmt x d w r
                        else stmt y d w r
stmt (Call m)   d w r = case lookup m d of
                          Just s  -> stmt s d w r
                          Nothing -> Error ("Undefined macro: " ++ m)
stmt (While c x) d w r = if test c w r == True 
                         then case stmt x d w r of
                           OK nw nr -> stmt (While c x) d nw nr
                           Done  nr -> Done nr
                           Error s  -> Error s
                         else OK w r
stmt (Iterate i s) d w r = if i > 0
                           then case stmt s d w r of
                                  OK nw nr -> stmt (Iterate (i - 1) s) d nw nr
                                  Done  nr -> Done nr
                                  Error s  -> Error s
                           else OK w r

stmts :: [Stmt] -> Defs -> World -> Robot -> Result
stmts []     d w r = OK w r
stmts (s:ss) d w r = case stmt s d w r of
                       OK  nw nr -> stmts ss d nw nr
                       Done   nr -> Done nr
                       Error  s  -> Error s

-- | Run a Karel program.
prog :: Prog -> World -> Robot -> Result
prog (m,s) w r = stmt s m w r
