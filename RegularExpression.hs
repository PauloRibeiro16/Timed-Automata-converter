module RegularExpression where
-- This file contains the Structure of regular expressions 
import Graphics.Gloss
import Text.ParserCombinators.Parsec.Error
import Data.List
import Data.List.Split
import Data.Function 

--Types
-- | This is the name of a State
type StateName = String 
-- | This is the name of a Clock
type ClockName = String
-- | This is the label 
type Event = String
-- | This is the struct of subtitution
type SubsStruct = ([StateName],(StateName,Expr),[(StateName,Expr)])
-- | This is the of a variant form of brzozowski expressions 
type BrozExprs = ([(StateName,Expr)],StateName,[StateName])
-- | This is the structure of an state
type StateStruct = (StateName,  [StateName])
-- | This is the structure of an clock
type ClockStruct = ([ClockName],  [(StateName,ClockName)])
-- | This is the structure of an transition
type TransStruct = [(StateName, (Event, Guard,Reset), StateName)]
-- | This is the struture of an timed automata 
type TAut = (StateStruct, ClockStruct, TransStruct) 
--Structures-
-- | Atomic predicate of Reset Structure
data AtomReset = AtomReset String String Double |
                 Tru  String 
                  deriving (Show,Eq)
-- | Reset Structure
data Reset  = AtmR AtomReset |
              AndR Reset Reset
                  deriving (Show,Eq)
-- | Atomic predicate Structure
data AtomPred = AtomPred String Double Double |
                AtomL0 String String Double   | 
                AtomL1 Double String String   |
                AtomL2 String String String   |
                Tr  String                    
                  deriving (Show,Eq)

-- | Guard Structure Guard
data Guard =  Atom AtomPred   | -- ^ Atomic
              Or Guard Guard  | -- ^ Disjuntion
              And Guard Guard | -- ^ Conjuntion
              Not Guard         -- ^ Negation
                deriving (Show,Eq)

-- | Intermedia data Structure  
data Expr =  Cl ClockName                    | -- ^ Clock Structure 
             St StateName                    | -- ^ State Structure 
             Init StateName                  | -- ^ Initial state
             Plus Expr Expr                  | -- ^ Plus Structure
             Inc String                      | -- ^ 
             Miu Expr Expr                   | -- ^ locked state
             Trans ((String,Guard,Reset),Expr) | -- ^ Transition Structure
             Conj (Expr,Expr)                | -- ^ Conjunction Struture 
             ExpTrue 
                deriving (Show,Eq)

--Pretty Printing-
-- | This function prints all expressions
topic::Float -> [String] -> Picture
topic a b = pictures(defineCoord a b)

-- | this function defines the coordinates where the regular expression is printed
defineCoord::Float -> [String] -> [Picture]
defineCoord a []    = []
defineCoord x (h:t) = [Translate (-670) (x) 
                $ Scale 0.15 0.15          
                $ Text h] ++ (defineCoord (x-30) t)++
                [ Translate (-670) (300) 
                $ Scale 0.2 0.2          
                $ Text "Regular Expression"]

-- |  This Function converts a tuple of (state,expression) into a list of strings
prettyinter::[(Expr,Expr)]-> [String]
prettyinter []       = [""]
prettyinter [(x,y)]  = [(prettyte x) ++ (prettyPrinting2 y)]
prettyinter ((a,b):t)= [(prettyte a) ++ (prettyPrinting1 b)]++(prettyinter t)

prettyte::Expr->String
prettyte (Init a)= ""
prettyte (Inc a) = a++" = " 

-- | This is the main printer
prettyPrinting1 :: Expr -> String
prettyPrinting1 (Cl x)               = "("++"Cl " ++ x++")"
prettyPrinting1 (Trans a)            = (prettyPrintingTra (Trans a))
prettyPrinting1 (Plus a b)           = (prettyPrintingPl (Plus a b))
prettyPrinting1 (Conj (e1, ExpTrue)) = (prettyPrinting1 e1)
prettyPrinting1 (Conj (e1, e2))      = (prettyPrinting1 e1) ++ " /\\ "++ (prettyPrinting1 e2)
prettyPrinting1 (St s)               = s
prettyPrinting1 (Inc a)              = a
prettyPrinting1 (Init a)              = a
prettyPrinting1 (ExpTrue)            = "True"
prettyPrinting1 (Miu (Inc a) b)       = "Miu "++ a ++" ("++(prettyPrinting1 b)++")" ++ "\n"
prettyPrinting1 (Miu (Init a) b)     = "Miu "++ a ++" ("++(prettyPrinting1 b)++")" ++ " Where\n"


prettyPrinting2 :: Expr -> String
prettyPrinting2 (Cl x)               = " Cl " ++ x
prettyPrinting2 (Trans a)            = (prettyPrintingTra (Trans a) )
prettyPrinting2 (Plus a b)           = (prettyPrintingPl (Plus a b))
prettyPrinting2 (Conj (e1, ExpTrue)) = (prettyPrinting2 e1)
prettyPrinting2 (Conj (e1, e2))      = (prettyPrinting2 e1) ++ " /\\ "++ (prettyPrinting2 e2)
prettyPrinting2 (Inc a)              = a
prettyPrinting2 (St s)               = s
prettyPrinting2 (Init s)             = s
prettyPrinting2 (ExpTrue)            = "True"
prettyPrinting2 (Miu (Inc a) b)       = "Miu "++ a ++" ("++(prettyPrinting2 b)++")"
prettyPrinting2 (Miu (Init a) b)     = "Miu "++ a ++" ("++(prettyPrinting1 b)++")" 
-- | This function prints transtitions
prettyPrintingTrans::Expr -> String
prettyPrintingTrans (Trans ((a,b,c),d)) = "("++a++","++(prettyPrintingAtConstrains b)++","++(prettyPrintingReset c)++")."

prettyPrintingTra::Expr -> String
prettyPrintingTra (Trans (a,(Inc d)))= (prettyPrintingTrans (Trans (a,(St d))))++d
prettyPrintingTra (Trans (a,(St d)))= (prettyPrintingTrans (Trans (a,(St d))))++d
prettyPrintingTra (Trans (a,d))     = (prettyPrintingTrans (Trans (a,d)))

-- | This function prints transtitions
prettyPrintingPl::Expr -> String
prettyPrintingPl (Plus a ExpTrue) = (prettyPrinting1 a)
prettyPrintingPl (Plus a b)       = (prettyPrinting1 a)++" + "++(prettyPrinting1 b)

-- | This function prints the Guard
prettyPrintingAtConstrains::Guard -> String
prettyPrintingAtConstrains (Atom a)  = (prettyPrintingAt a)
prettyPrintingAtConstrains (Or a b)  = (prettyPrintingAtConstrains a)++" | "++(prettyPrintingAtConstrains b)
prettyPrintingAtConstrains (And a b) = (prettyPrintingAtConstrains a)++" & "++(prettyPrintingAtConstrains b)
prettyPrintingAtConstrains (Not a)   = "!"++(prettyPrintingAtConstrains a)

prettyPrintingAt::AtomPred -> String
prettyPrintingAt (AtomPred a b c) = a ++ ":"++"["++(show b)++","++(show c)++"]"
prettyPrintingAt (AtomL0  a  b c) = a ++ b ++ (show c)
prettyPrintingAt (AtomL1  a  b c) = (show a) ++ b ++ c
prettyPrintingAt (AtomL2  a  b c) = a ++ b ++ c
prettyPrintingAt (Tr  a )         = a

-- | This function prints the Reset
prettyPrintingReset::Reset -> String
prettyPrintingReset (AtmR a)   =  prettyPrintingRes a  
prettyPrintingReset (AndR a b) = (prettyPrintingReset a)++" & "++(prettyPrintingReset b)

prettyPrintingRes::AtomReset -> String
prettyPrintingRes (AtomReset a b c) = a ++ b ++ (show c) 
prettyPrintingRes (Tru   a )        = "true" 

-- | This function gives us an expression or parse error
pretty' :: ([(Expr,Expr)],[(String,Expr)]) -> Either ParseError Picture
pretty' (e,a) = return (topic (0) (prettyinter e))
------------------Draw Struct ---
drawStruct::[Expr]->[(String,Expr)]
drawStruct []                   = []
drawStruct ((Miu (St a) b):t)   = [(a,(Miu (St a) b))]++(drawStruct t)
drawStruct ((Miu (Init a) b):t) = [(a,(Miu (St a) b))]++(drawStruct t)

--Printer Struct-
-- | This function replace the next state of the transitions with a letter that will correspond to that state
toFinalStruct::[Expr]->[(Expr,Expr)]
toFinalStruct []=[]
toFinalStruct a = (srt.helpSubs.genList) (a)

srt::[(Expr,Expr)]->[(Expr,Expr)]
srt ((Init a,b):t)=(Init a,b):srt t
srt ((Inc a,Miu b d):t)=(Inc a,Miu (Inc a) d):srt t 
srt a = a

-- | This function groups expression into (State,Expression)
genList::[Expr]->([(Expr,Expr)],[(Expr,Expr)])
genList (h:t) = (toTupleExpr) ((Init "a",h):toTable t)

helpSubs::([(Expr,Expr)],[(Expr,Expr)])-> [(Expr,Expr)]
helpSubs (a,b) = subt b a 

toTable::[Expr]->[(Expr,Expr)]
toTable [] = []
toTable a  = genCorrespond ((newGenerator.ceiling) (logBase 26 (fromIntegral (length(a)+1)))) a 

toTupleExpr::[(Expr,Expr)]->([(Expr,Expr)],[(Expr,Expr)])
toTupleExpr a = (a,auxilaryT a)

auxilaryT::[(Expr,Expr)]->[(Expr,Expr)]
auxilaryT []              = []  
auxilaryT ((a,Miu c b):h) = (a,c):(auxilaryT h) 
auxilaryT (h:t)           = (auxilaryT t)

genCorrespond::[String] -> [Expr] -> [(Expr,Expr)]
genCorrespond [] []         =[]
genCorrespond a []          =[]
genCorrespond (h:t) (hs:ts) = (Inc h,hs):(genCorrespond t ts)

-- | This Function generates combination of letters with a certain length
newGenerator::Int->[String]
newGenerator a =  mapM (const "XYZQWERTUIOPASDFGHJKLCVBNM") [1..a]

subt::[(Expr,Expr)] -> [(Expr,Expr)] -> [(Expr,Expr)]
subt [] []  = [] 
subt a []   = []
subt b (h:t)= (mapping b h):(subt b t) 

mapping::[(Expr,Expr)] -> (Expr,Expr) -> (Expr,Expr)
mapping t (a,b) = (a,apply t b)

apply::[(Expr,Expr)]->Expr->Expr
apply [] a   = a 
apply (h:t) a= apply t (addNewStruct h a)
-- | This function 
addNewStruct::(Expr,Expr) -> Expr -> Expr
addNewStruct  (c,d) (Trans (a,b))= if (b==d) then Trans (a,c) else Trans (a,b)
addNewStruct  (c,d) (Conj (a,b)) = Conj (addNewStruct (c,d) a,b)
addNewStruct  _ (ExpTrue)        = ExpTrue
addNewStruct  _ (Cl a)           = Cl a
addNewStruct  (c,d) (Plus a b)   = Plus (addNewStruct (c,d) a) (addNewStruct (c,d) b) 
addNewStruct  (c,d) (Miu a b)    = Miu a (addNewStruct (c,d) b)

--Veirifier
-- | This function gives an error of the clock dosen't exist
verifier::ClockStruct -> ClockStruct
verifier a = if (checker a) then a else (error "invalid clock on line 4") 

checker:: ClockStruct -> Bool
checker (a,b) = (and) (check a (mapStringTuples b))

-- | This function Checks all clock mentioned exists 
check::[ClockName]->[ClockName]->[Bool]
check a []   = [True]
check a (h:t)= (elem h a):(check a t)

mapStringTuples::[(StateName,ClockName)]->[ClockName]
mapStringTuples = fmap(\(a,b) -> b)

--Generates Sub-Expressions
-- | This function decompose an expression into several sub-Expressions
toInTerm::Expr -> [Expr]
toInTerm a = (fst. applyAll. interPrinter) (a)

applyAll::([Expr],[Expr])->([Expr],[Expr])
applyAll (a,[])= (a,[])
applyAll (a,b) = applyAll (a++(applyInter b),applyListNewE b)

applyListNewE::[Expr]->[Expr]
applyListNewE a = concat (map (exprsExplorer) a)

applyInter::[Expr] -> [Expr]
applyInter = fmap (intermPrin)   

interPrinter:: Expr -> ([Expr],[Expr])
interPrinter a = ([intermPrin a],exprsExplorer a)

-- |  This function separates each expression 
exprsExplorer::Expr -> [Expr]
exprsExplorer  (Trans (a,(St b)))    = []
exprsExplorer  (Trans (a,(Miu b c))) = [Miu b c]
exprsExplorer  (St a)                = []
exprsExplorer  (Conj (a,b))          = (exprsExplorer a) 
exprsExplorer  (ExpTrue)             = []
exprsExplorer  (Cl a)                = []
exprsExplorer  (Plus a b)            = (exprsExplorer a)++(exprsExplorer b) 
exprsExplorer  (Miu a b)             = (exprsExplorer b)

-- | This replaces the next Expression by the structure State
intermPrin::Expr -> Expr
intermPrin  (Trans (a,(St b)))    = Trans (a,(St b))
intermPrin  (Trans (a,(Miu b c))) = Trans (a,b)
intermPrin  (St a)                = (St a) 
intermPrin  (Conj (a,ExpTrue))    = (intermPrin a)
intermPrin  (Conj (a,b))          = Conj (intermPrin a,intermPrin b)
intermPrin  (ExpTrue)             = ExpTrue
intermPrin  (Cl a)                = Cl a
intermPrin  (Plus a b)            = Plus (intermPrin a ) (intermPrin b ) 
intermPrin  (Miu a b)             = Miu a (intermPrin b )

--Subtituition-
-- | This function gives the list of free states
freeVar::Expr ->[StateName]
freeVar a = removerept (exploreState(a))\\ removerept(exploreGuardedSt(a))

-- | This function gives a list of all states on a expression
exploreState::Expr-> [StateName]
exploreState  (Trans b)        = exploreState (snd(b)) 
exploreState  (St a)           = [a]
exploreState  (Miu (Init a) b) = exploreState (b)
exploreState  (Miu  (St a) b ) = exploreState (b)
exploreState  (Conj (a,b))     = exploreState(a) ++ exploreState(b)
exploreState  (ExpTrue)        = []
exploreState  (Cl a)           = []
exploreState  (Plus a b)       = exploreState (a) ++ exploreState(b)

-- | This function remove repetitions form a list
removerept :: (Eq a) => [a] -> [a]
removerept (x:xs) = x : removerept (filter (/= x) xs)
removerept []     = []

-- | This function gives list all the states that are guarded
exploreGuardedSt::Expr-> [StateName]
exploreGuardedSt  (Trans b)       = exploreGuardedSt (snd(b)) 
exploreGuardedSt  (St a)          = []
exploreGuardedSt  (Conj (a,b))    = exploreGuardedSt(a)++exploreGuardedSt(b)
exploreGuardedSt  (ExpTrue)       = []
exploreGuardedSt  (Cl a)          = []
exploreGuardedSt  (Plus a b)      = exploreGuardedSt (a) ++ exploreGuardedSt(b)
exploreGuardedSt  (Miu (St a) b)  = [a] ++ exploreGuardedSt (b)
exploreGuardedSt  (Miu (Init a) b)= [a] ++ exploreGuardedSt (b)

-- | This function subtitute a state in a expression 
substituteExpr::Expr-> (StateName,Expr) -> Expr 
substituteExpr  (Trans (a,b)) xs    = Trans (a,substituteExpr b xs)
substituteExpr  (St a)        (x,y) = if (a == x) then y else (St a) 
substituteExpr  (Conj a)      xs    = Conj (substituteExpr  (fst(a)) xs,substituteExpr (snd(a)) xs)
substituteExpr  (ExpTrue)     xs    = ExpTrue
substituteExpr  (Cl a)        xs    = Cl a
substituteExpr  (Plus a b)    xs    = Plus (substituteExpr a xs) (substituteExpr b xs) 
substituteExpr  (Miu a b)     xs    = Miu a (substituteExpr b xs)

thr::SubsStruct->[(StateName,Expr)]
thr (_,_,a) = a

-- | This function puts the updated expression in the list
updateElem::(StateName,Expr)->[(StateName,Expr)]->[(StateName,Expr)]
updateElem (a,b) (h:t) = if (fst(h)==a) then (fst(h),b):t else h:updateElem (a,b) t

-- | This function updates the list, updates the free state list
updateList::(StateName,Expr)->[(StateName,Expr)]->SubsStruct
updateList (x,y) a = (freeVar y,(x,y) ,updateElem (x,y) a) 

-- | This function performs the substitution and places it in the tuple
updateExpression::StateName ->(StateName,Expr)->[(StateName,Expr)]->(StateName,Expr)
updateExpression x (a,b) y = (a,substituteExpr b (x,(findExp x y)))
 
auX::StateName ->(StateName,Expr)->[(StateName,Expr)]->SubsStruct
auX  x y z =  updateList (updateExpression x y z) z

-- | This function will subtitute as long as there are states to be replaced 
whileFree::SubsStruct->SubsStruct
whileFree ([]    , a , x ) = ([],a,x) 
whileFree (   b  , a ,[] ) = (b,a,[])
whileFree ((x:xs), a , y ) = whileFree  (auX x a y)   

-- | This function helps to put the expression in the correct type
auxilary::SubsStruct->[(StateName,Expr)]
auxilary a = thr(whileFree a)

-- | This function returns the element at index x
getIt::(Int,[(StateName,Expr)])->(StateName,Expr)
getIt (a,b) = b!!a 

-- | This function returns the table with all substitutions made
updatedtable:: Int -> [(StateName,Expr)] ->(Int,[(StateName,Expr)])
updatedtable n l = if n >= length l then (n,l)
                else let (s,t) = getIt (n,l)
                         l'= (auxilary)  (freeVar t,(s,t),l)
                         in updatedtable (n+1) l'

-- | This function finds an expression in the expression table
findExp:: StateName -> [(StateName,Expr)]-> Expr
findExp x ((xs,y):ys)= if(x==xs) then y else findExp x ys

--Building Table-

procura::StateName ->[(StateName,Expr)]->[(StateName,Expr)]
procura a []    = []
procura a ((x,y):t) = if (a==x) then (x,(putinit y a)):t else (x,y):procura a t 

-- | This function is used to put a mark oon the initial state 
putinit::Expr -> StateName -> Expr
putinit  (Miu (St a) b) xs    = if (a==xs) then (Miu (Init a) b) else Miu (St a) (putinit b xs)   
putinit  (Trans (a,b)) xs    = Trans (a,putinit b xs)
putinit  (St a)        xs    = St a 
putinit  (Conj a)      xs    = Conj (putinit  (fst(a)) xs,putinit (snd(a)) xs)
putinit  (ExpTrue)     xs    = ExpTrue
putinit  (Cl a)        xs    = Cl a
putinit  (Plus a b)    xs    = Plus (putinit a xs) (putinit b xs) 

-- | This function is used to build the expression table
lockexpressions::[StateName]->[(StateName,Expr)]->[(StateName,Expr)]
lockexpressions [] a = []
lockexpressions (h:t) b = [putLock h b] ++ lockexpressions t b 

putLock::StateName->[(StateName,Expr)]->(StateName,Expr)
putLock a []    = (a,Miu (St a) (ExpTrue))
putLock a ((x,y):h) = if (a==x) then (a,Miu (St a) y) else putLock a h 

tableBuilder:: TransStruct-> ClockStruct->[(StateName,Expr)]
tableBuilder a b = buildTable (transStructtoBroz a) (clockStructtoBroz b)

-- | This function puts Miu structure in the begginig of an expression 
miuHeader::[StateName]->[(StateName,Expr)]->[(StateName,Expr)]
miuHeader a b = lockexpressions a b

buildTable  :: [(StateName,Expr)] -- ^ Transition list
            -> [(StateName,Expr)] -- ^ Clocks list 
            -> [(StateName,Expr)] -- ^ Expression table
buildTable l1 l2 = (mergeExprs . groupByState . sortStrTuples) (buildTrans(l1) ++ l2) 

buildTrans :: [(StateName,Expr)] -> [(StateName,Expr)]
buildTrans = mergeTrans . groupByState . sortStrTuples

-- | This function is used to sort the list by state name 
sortStrTuples :: [(StateName,Expr)] -> [(StateName,Expr)]
sortStrTuples = sortBy (compare `on` f)
                    where f (x,_) = last $ (splitOn " ") x

-- | This function is used to group the list by state name
groupByState ::[(StateName,Expr)] ->[[(StateName,Expr)]]
groupByState = groupBy (\(a,b) (c,d) -> a == c)

-- | This function is used to merge expression from the same  state
mergeExprs :: [[(StateName,Expr)]] -> [(StateName, Expr)]
mergeExprs = fmap flattenExprWithName

-- | This function is used to flatten expressions
flattenExpr :: [Expr] -> Expr
flattenExpr [] = ExpTrue
flattenExpr (x:xs) = Conj (x, (flattenExpr xs))

-- | This function is used to flatten expressions from a given state
flattenExprWithName :: [(StateName,Expr)] -> (StateName, Expr)
flattenExprWithName l = let ne = ((flattenExpr . (fmap snd)) l) in ((fst . head) l, ne)

flattenTrans::[Expr]-> Expr
flattenTrans []  = ExpTrue
flattenTrans [a] = a
flattenTrans (x:xs) = Plus x (flattenTrans xs) 

flattenWithTr :: [(StateName,Expr)] -> (StateName, Expr)
flattenWithTr l = let ne = ((flattenTrans . (fmap snd)) l) in ((fst . head) l, ne)

mergeTrans :: [[(StateName,Expr)]] -> [(StateName, Expr)]
mergeTrans = fmap flattenWithTr

--Building Expression-

-- | This function is used to generate compact expression 
updatedBrozExprs:: Either ParseError BrozExprs -> Either ParseError BrozExprs 
updatedBrozExprs x = x >>= func1
  where
    func1(y,a,z) = Right $( procura a (snd(updatedtable 0 y)),a, z)

-- | This function produces an expression from the brozozovki table
intrExprsTocExpr:: Either ParseError BrozExprs -> Either ParseError ([(Expr,Expr)],[(String,Expr)]) 
intrExprsTocExpr x = x >>= func2
  where
    func2(x,y,z) = Right $ ((toFinalStruct.removerept.toInTerm) (findExp y x),(drawStruct.toInTerm) (findExp y x) )

-- | This function produces a brozozovki table from Timed automata structure
tAutToBrozExprs:: Either ParseError TAut -> Either ParseError BrozExprs
tAutToBrozExprs x = x >>= func
  where
    func ((z,y),b,c) = Right $ (miuHeader y (tableBuilder c b),z,y)

-- | This function verefies if all tuples (state,clock) are valid 
validation:: Either ParseError TAut -> Either ParseError TAut
validation x = x >>= func
  where
    func (a,b,c) = Right $ (a,verifier b,c)

-- | This function produces an expression from a Timed automata structure
tAutToExpr:: Either ParseError TAut -> Either ParseError ([(Expr,Expr)],[(String,Expr)])
tAutToExpr = intrExprsTocExpr . updatedBrozExprs . tAutToBrozExprs . validation

--Building BrozExpression
-- | This function turns the clockstruct structure into an expression that can be manipulated
clockStructtoBroz::ClockStruct->[(StateName,Expr)]
clockStructtoBroz (a,b) = map (\(x,y) -> (x,Cl y)) b

-- | This function turns the transstruct structure into an expression that can be manipulated
transStructtoBroz::TransStruct ->[(StateName, Expr)]
transStructtoBroz = fmap (\(x,(a,b,c),z) ->   (x,Trans ((a,b,c),St z))) 
