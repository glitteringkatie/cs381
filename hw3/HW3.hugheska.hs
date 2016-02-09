import MiniMiniLogo
import Render

type State  = Point
type Domain = State -> (State, Maybe Line)

cmd :: Cmd -> State -> (State, Maybe Line)
cmd (Pen m)    (x,y) = undefined
cmd (Move i j) (x,y) =

prog :: Prog -> State -> (State, [Line])
prog []     s = (s,[])
prog (p:ps) s = move ps (cmd s p)


-- somewhere we will have to use toHTML to render this all
-- I think this will be the `draw` command referenced in the assignment
-- description. It will take the [Line] from prog since toHTML takes an array
-- of lines
