import PA1Helper
import System.Environment (getArgs)
import Data.Set(Set)
import Data.List
import qualified Data.Set as Set
-- Haskell representation of lambda expression
-- data Lexp = Atom String | Lambda String Lexp | Apply Lexp  Lexp 

-- Given a filename and function for reducing lambda expressions,
-- reduce all valid lambda expressions in the file and output results.
-- runProgram :: String -> (Lexp -> Lexp) -> IO ()

-- This is the identity function for the Lexp datatype, which is
-- used to illustrate pattern matching with the datatype. "_" was
-- used since I did not need to use bound variable. For your code,
-- however, you can replace "_" with an actual variable name so you
-- can use the bound variable. The "@" allows you to retain a variable
-- that represents the entire structure, while pattern matching on
-- components of the structure.
id' :: Lexp -> Lexp
id' v@(Atom _) = v
-- final result

id' lexp@(Lambda str lex) = lex

id' lexp@(Apply e1 _)  = lexp
--not direct for pattern match

--CAN lambda internally appear free variable?
getFree :: Lexp -> [Lexp] -> [Lexp] -> [Lexp]
getFree lexp@(Atom _) boundSet freeSet = 
    if findX lexp boundSet then freeSet
    else freeSet ++ [lexp]
getFree lexp@(Lambda str e) boundSet freeSet = []{--getFree e ([Atom(str)]++boundSet) freeSet--}
getFree lexp@(Apply e1 e2) boundSet freeSet = getFree e1 boundSet freeSet ++ getFree e2 boundSet freeSet

findX :: Lexp -> [Lexp] -> Bool
findX ele [] = False
findX ele (x:xs) 
   | ele == x = True
   | otherwise = findX ele xs
                     
--Assuming input most be free-less
--eta conversion /x.(E x) -> E
eta :: Lexp -> [Lexp] -> Lexp 
eta lexp@(Atom _) bound = lexp
   --if findX lexp bound then lexp
eta lexp@(Lambda str e) bound = eta e (bound++[Atom(str)])
eta lexp@(Apply e1 e2) bound  
   | findX fe2 bound = fe1
   | otherwise = Apply fe1 fe2
  where
    fe1 = eta e1 bound
    fe2 = eta e2 bound
 
--alpha rename /x.x x -> /xx.xx x
callAlpha :: Lexp -> Lexp
callAlpha lexp = alpha lexp free []
  where free = getFree lexp [] []

alpha :: Lexp -> [Lexp] -> [String] -> Lexp
alpha lexp@(Atom v) free needChange 
   | findStringX lexp needChange  = reName lexp name
   | otherwise =  lexp
  where name = getStringX lexp needChange

alpha lexp@(Lambda str e) free needChange
   | findX bstr free =  Lambda nstr be
   | otherwise = Lambda str ne 
  where 
    bstr = Atom(str)
    nstr = str++str --same rule applied
    ne = alpha e free needChange
    newchanges = needChange ++ [str] 
    be = alpha e free newchanges

alpha lexp@(Apply e1 e2) free needChange= Apply fe1 fe2
  where
    fe1 = alpha e1 free needChange
    fe2 = alpha e2 free needChange

reName :: Lexp -> String -> Lexp 
reName lexp@(Atom _) name = Atom v 
   where v = name++name 
reName lexp@(Lambda _ _) name = lexp
reName lexp@(Apply _ _) name = lexp

findStringX :: Lexp -> [String] -> Bool
findStringX ele [] = False
findStringX ele (x:xs)
   | ele == Atom(x) = True
   | otherwise = findStringX ele xs

getStringX :: Lexp -> [String] -> String
getStringX ele [] = "Shouldneverbereached" --no string
getStringX ele (x:xs)
  |  ele == Atom(x) = x
  |  otherwise = getStringX ele xs

-- You will need to write a reducer that does something more than
-- return whatever it was given, of course!

--Beta reduction (\x.E M) -> E{M/x}
--Check results
callBeta :: Lexp -> Lexp 
callBeta lexp 
   | isFinish lexp = lexp
   | otherwise = callBeta var
  where var = beta lexp
 
beta :: Lexp -> Lexp 
beta lexp@(Atom _) = lexp
beta lexp@(Lambda _ _) = lexp
beta lexp@(Apply e1 e2) 
   | isE lexp = beta new 
   | otherwise  = cR
  where 
    name = getX e1
    new = replace e1 e2 name
    fe1 = beta e1
    fe2 = beta e2
    cR = Apply fe1 fe2

replace :: Lexp -> Lexp -> String -> Lexp
replace lexp@(Atom v) cM cX
  | lexp == x = cM
  | otherwise = lexp
 where x = Atom(cX)
replace lexp@(Lambda str e) cM cX 
  | str == cX = replace e cM cX
  | otherwise = Lambda str ne
 where ne = replace e cM cX

replace lexp@(Apply e1 e2) cM cX
  | e2 == cM = fe1
  | otherwise = Apply fe1 fe2
 where 
  fe1 = replace e1 cM cX
  fe2 = replace e2 cM cX

--A dummy variable indicate a empty Lexp
isFinish :: Lexp -> Bool
isFinish lexp@(Atom _) = True
isFinish lexp@(Lambda _ _) = True
isFinish lexp@(Apply e1 e2) 
  | isE lexp = False
  | isFinish e1 && isFinish e2 = True
  | otherwise = False 
--should include e2 ???

reducer :: Lexp -> Lexp
reducer equ = beta equ 

--Help functions match (\x.E M)
isE :: Lexp -> Bool
isE lexp@(Atom _) = False
isE lexp@(Lambda _ _) = False
isE lexp@(Apply e1 e2) = isLambda e1

isLambda :: Lexp -> Bool
isLambda lexp@(Atom _) = False
isLambda lexp@(Lambda _ _) = True
isLambda lexp@(Apply _ _)  = False

getX :: Lexp -> String 
getX lexp@(Lambda str _) = str
getX lexp@(Atom v) = "-"
getX lexp@(Apply e1 e2) = "-"

{--finalReudcer :: Lexp -> Lexp
 - finalReducer lexp = result 
 -  where result = eta afterbeta
 -  afterbeta = callBeta afteralpha
 -  afteralpha = callAlpha lexp 
 - --}

-- Entry point of program
main = do
    args <- getArgs
    let inFile = case args of { x:_ -> x; _ -> "input.lambda" }
    let outFile = case args of { x:y:_ -> y; _ -> "output.lambda"}
    -- id' simply returns its input, so runProgram will result
    -- in printing each lambda expression twice.  
    runProgram inFile outFile reducer
    let x = Atom("x")
    let y = Atom("y")
    let w = Atom("w")
    let a1 = Apply y w
    let a2 = Apply x y
    let lam1 = Lambda "y" a2
    let lam2 = Lambda "x" lam1
    let c2 =Apply lam2 a1
    let c3 = Lambda "x" x
    let c4 = Apply c3 y
    let l1 = []
    let l2 = []
    let fr = getFree c2 [] []
    let dummy = Atom("-")
    let check = findX y fr
    let result = replace c4 y "x"
    print fr
    print result

    
