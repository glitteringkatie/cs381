module HW3 where

import MiniMiniLogo
import Render


--
-- * Semantics of MiniMiniLogo
--

-- NOTE:
--   * MiniMiniLogo.hs defines the abstract syntax of MiniMiniLogo and some
--     functions for generating MiniMiniLogo programs. It contains the type
--     definitions for Mode, Cmd, and Prog.
--   * Render.hs contains code for rendering the output of a MiniMiniLogo
--     program in HTML5. It contains the types definitions for Point and Line.

-- | A type to represent the current state of the pen.
type State = (Mode,Point)

-- | The initial state of the pen.
start :: State
start = (Up,(0,0))

-- | A function that renders the image to HTML. Only works after you have
--   implemented `prog`. Applying `draw` to a MiniMiniLogo program will
--   produce an HTML file named MiniMiniLogo.html, which you can load in
--   your browswer to view the rendered image.
draw :: Prog -> IO ()
draw p = let (_,ls) = prog p start in toHTML ls


-- Semantic domains:
--   * Cmd:  State -> (State, Maybe Line)
--   * Prog: State -> (State, [Line])


-- | Semantic function for Cmd.
--
--   >>> cmd (Pen Down) (Up,(2,3))
--   ((Down,(2,3)),Nothing)
--
--   >>> cmd (Pen Up) (Down,(2,3))
--   ((Up,(2,3)),Nothing)
--
--   >>> cmd (Move 4 5) (Up,(2,3))
--   ((Up,(4,5)),Nothing)
--
--   >>> cmd (Move 4 5) (Down,(2,3))
--   ((Down,(4,5)),Just ((2,3),(4,5)))
--
cmd :: Cmd -> State -> (State, Maybe Line)
cmd (Pen i)    (m, p)     = ((i,p), Nothing)
cmd (Move i j) (m, (x,y)) = case m of
                              Up   -> ((m, (i, j)), Nothing)
                              Down -> ((m, (i, j)), Just ((x, y), (i, j)))


-- | Semantic function for Prog.
--
--   >>> prog (nix 10 10 5 7) start
--   ((Down,(15,10)),[((10,10),(15,17)),((10,17),(15,10))])
--
--   >>> prog (steps 2 0 0) start
--   ((Down,(2,2)),[((0,0),(0,1)),((0,1),(1,1)),((1,1),(1,2)),((1,2),(2,2))])
addLine :: (State, [Line]) -> Line -> (State, [Line])
addLine (s, l) newl = (s, newl:l)

prog :: Prog -> State -> (State, [Line])
prog []     s = (s,[])
prog (p:ps) s = let res = (cmd p s); line = snd res; state = fst res in
                  case line of
                    Just l  -> addLine (prog ps state) l
                    Nothing -> prog ps state


--
-- * Extra credit
--

-- | This should be a MiniMiniLogo program that draws an amazing picture.
--   Add as many helper functions as you want.
amazing :: Prog
amazing = [Pen Up, Move (14,24), Pen Down, Move (22,26), Move (23,26),
              Move (21,25), Move (21,24), Move (22,24), Move (21,23),
              Move (20,24), Move (14,21), Move (18,11), Move (18,9),
              Move (16,6), Move (16,2), Move (12,2),
              Move (14,3), Move (12,5), Move (12,6), Move (13,9), Move (10,6),
              Move (9,3), Move (9,2), Move (5,2), Move (7,3), Move (5,6),
              Move (5,7), Move (6,9), Move (5,11), Move (5,12), Move (2,15),
              Move (2,18), Move (3,21), Move (2,23), Move (4,25), Move (4,24),
              Move (3,23), Move (4,22), Move (4,23), Move (5,22), Move (4,21),
              Move (5,20), Move (6,21), Move (7,21), Move (14,23), Move (14,25),
              Move (13,25), Move (7,22), Move (7,14), Move (6,12), Move (6,22),
              Move (7,22), Move (4,28), Move (9,28), Move (9,29), Move (8,28),
              Move (8,29), Move (7,29), Move (6,30), Move (8,30), Move (7,31),
              Move (13,32), Move (13,31), Move (14,31), Move (13,30),
              Move (13,29), Move (14,30), Move (15,29), Move (15,27),
              Move (14,26), Move (13,27), Move (13,25),
            Pen Up, Move (8,29), Pen Down, Move (13,30),
            Pen Up, Move (6,24), Pen Down, Move (8,24), Move (9,25),
            Pen Up, Move (13,28), Pen Down, Move (14,29), Move (14,27),
                Move (13,28)]
