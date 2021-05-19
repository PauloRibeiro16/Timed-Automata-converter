module Draw where

import Graphics.Gloss
import Text.ParserCombinators.Parsec.Error
import RegularExpression

lengtLabel::[(String,([Expr],[Expr]))]->[Int]
lengtLabel [] = []
lengtLabel ((a,(c,d)):t) =(map(\(Trans(a,b)) -> length(toLabel a)) (c++d))++(lengtLabel t)

expanson::[Int]->Float
expanson a  = 50* fromIntegral(div (maximum a) 9)

sc::Float -> Float
sc a = if (a/20==0) then 1 else 1/(a/20)

--Structure construction

structDr :: ([(Expr,Expr)],[(String,Expr)]) -> Either ParseError Picture
structDr (e,a) = return ((drawing.makeListOfTransitions) (a))

-- | This function calculates the general size of the states taking into account the length of the longest string
number::[(String,([Expr],[Expr]))]->Float
number c = fromIntegral (20* div ((maximum) (map(\(a,b) -> length(a)) c)) 3)

-- | This generates a list of tuplos  (state,(other transitions,self-loops)) 
makeListOfTransitions::[(String,Expr)]->[(String,([Expr],[Expr]))]
makeListOfTransitions []       =[]
makeListOfTransitions ((a,y):t)=[(a,(toListOfSTrans a y,toListOfSelf a y))]++(makeListOfTransitions t)  

toListOfSelf::String -> Expr -> [Expr]
toListOfSelf a b = exploreSelfTrans a b 

exploreSelfTrans::String ->Expr -> [Expr]
exploreSelfTrans c (Trans (a,(St b)))    = if b==c then [(Trans (a,(St b)))] else [] 
exploreSelfTrans c (Conj (a,b))          = (exploreSelfTrans c a) 
exploreSelfTrans c (ExpTrue)             = []
exploreSelfTrans c (Plus a b)            = (exploreSelfTrans c a)++(exploreSelfTrans c b) 
exploreSelfTrans c (Miu a b)             = (exploreSelfTrans c b) 

toListOfSTrans::String -> Expr -> [Expr]
toListOfSTrans a b= exploreSTrans a  b

exploreSTrans::String ->Expr -> [Expr]
exploreSTrans c (Trans (a,(St b)))    = if b/=c then [(Trans (a,(St b)))] else [] 
exploreSTrans c (Conj (a,b))          = (exploreSTrans c a) 
exploreSTrans c (ExpTrue)             = []
exploreSTrans c (Plus a b)            = (exploreSTrans c a)++(exploreSTrans c b) 
exploreSTrans c (Miu a b)             = (exploreSTrans c b)

-- transitions Separator 
-- | This function creates a tuple  (transitions to existing states,transitions to new states)  
transToExistante::[Expr]->([Expr],[Expr])->[(String,(Float,Float))]->([Expr],[Expr])
transToExistante c a []                  = ([],c)
transToExistante [] b c                  = b
transToExistante ((Trans(a,St b)):t) u  r= transToExistante t (putInTuple (Trans(a,St b)) (existanteBool b r) u ) r

existanteBool::String-> [(String,(Float,Float))] -> Bool
existanteBool c []         = False
existanteBool c [(b,a)]    = if (c==b) then True else False
existanteBool c ((b,a):t)  = if (c==b) then True else (existanteBool c t)

putInTuple::Expr->Bool->([Expr],[Expr])->([Expr],[Expr])
putInTuple a x (b,c) = if x then (b++[a],c) else (b,c++[a])  
                                            --Desenho total
-- | This function draws the automaton 
drawing::[(String,([Expr],[Expr]))] -> Picture
drawing l@((a,b):t)=pictures ([scale (sc(number l)) (sc(number l)) 
                            (pictures((painting ((number l),
                            (expanson.lengtLabel) (l)) (xAxis+(number l),
                            yAxis) (a,b)  l  [(a,(xAxis+(number l),yAxis))])++[start]))]++[automata])

painting::(Float,Float)->(Float,Float)->(String,([Expr],[Expr]))->
                [(String,([Expr],[Expr]))]->[(String,(Float,Float))]-> [Picture]
painting (f,a) c (x,(y,z)) r t = [state (f,a) x c]++(drawNewStates (f,a) c y t r)++(drawSelfloops (f,a) c z)

--Draw States and Arrows
-- | This function draws new states
drawNewStates::(Float,Float)->(Float,Float)->[Expr]->[(String,(Float,Float))]
                    ->[(String,([Expr],[Expr]))]->[Picture]
drawNewStates (d,l) a c f g = multiplePainting (d,l) (transToExistante c ([],[]) f) a f g 

-- | This function draws arrows for states 
multiplePainting::(Float,Float)->([Expr],[Expr])->(Float,Float)
                        ->[(String,(Float,Float))]->[(String,([Expr],[Expr]))]->[Picture]
multiplePainting (f,l) (x,y) b c d= (drawExistState (f,l) (sameState x []) b c)++(drawNewPath (f,l) y b c d)

drawNewPath::(Float,Float)->[Expr]->(Float,Float)->[(String,(Float,Float))]
                                ->[(String,([Expr],[Expr]))]->[Picture]
drawNewPath (f,l) a b c d =[genNewPoints (f,l) b a] ++ (recursiveCall (f,l) b a c d)

-- | This function does a recursive call to draw the next states
recursiveCall::(Float,Float)->(Float,Float)->[Expr]->[(String,(Float,Float))]
                                        ->[(String,([Expr],[Expr]))]->[Picture]
recursiveCall (f,l) a b c d = findNew (f,l) (newStates b) 
                            (gencoordenates (f,l) (length b) a) (upadateEStates c (gencoordenates (f,l) (length b) a) b) d

findNew::(Float,Float)->[String]->[(Float,Float)]->[(String,(Float,Float))]->[(String,([Expr],[Expr]))]->[Picture]
findNew  (f,l) (h:t) (hs:ts) c d  =(painting (f,l) hs (singleF h d) d c )++(findNew (f,l) t ts c d)
findNew _ _ _ _ _            = []

newStates::[Expr]->[String]
newStates = fmap(\(Trans(a,St b)) -> b)

singleF::String->[(String,([Expr],[Expr]))]->(String,([Expr],[Expr]))
singleF a ((b,c):t) = if b==a then (b,c) else singleF a t  

-- | This function draws arrows to new states
toArrow::(Float,Float)->(Float,Float)->[(Float,Float)]->[Expr]->[Picture]
toArrow (d,l) a (h:t) ((Trans(b,(St f))):ts) = (drawPlusArrows (d,l) a h (toLabel b))++(toArrow (d,l) a t ts)
toArrow _ _ _ _                          = []

-- | This Function generates points 
genNewPoints::(Float,Float)->(Float,Float)-> [Expr]-> Picture
genNewPoints (f,l) a b = pictures (toArrow (f,l) a (gencoordenates (f,l) (length b) a)b) 

-- | This function draws arrows to states that already exist 
drawExistState::(Float,Float)->[[Expr]]->(Float,Float)->[(String,(Float,Float))]->[Picture]
drawExistState _ [] _ _    = [blank]
drawExistState (f,l) (h:t) a b = (drawOneState (f,l) h a b):(drawExistState (f,l) t a b)

drawOneState::(Float,Float)->[Expr]->(Float,Float)->[(String,(Float,Float))]-> Picture
drawOneState (f,s) l@((Trans(a,St b)):t) e c = (anyPath (f,s) (givePoint b c) e (manyExpressions l)) 

givePoint::String->[(String,(Float,Float))]->(Float,Float)
givePoint a ((x,y):t) = if (a==x) then y else (givePoint a t) 

-- | This function groups expressions that go to the same state 
sameState::[Expr]->[[Expr]]->[[Expr]]
sameState []             a      = a
sameState ((Trans(a,St b)):t) c = sameState t (expressionAcopler b (Trans(a,St b)) c)

existaBool::String-> [Expr]->Bool 
existaBool a ((Trans(e,St r)):t) = if (a==r) then True else False

expressionAcopler::String->Expr->[[Expr]]->[[Expr]]
expressionAcopler a e []     = [[e]]
expressionAcopler a e (h:t)  = if (existaBool a h) then ([e]++h):t else h:(expressionAcopler a e t) 

-- | This function updates the list of states that have already been drawn 
upadateEStates::[(String,(Float,Float))]->[(Float,Float)]->[Expr]->[(String,(Float,Float))]
upadateEStates a [] []                        = a
upadateEStates a (h:ts) ((Trans(b,(St f))):t) = [(f,h)]++(upadateEStates a ts t)

-- | This function draws a arc arrow between two states
anyPath::(Float,Float)-> (Float,Float) -> (Float,Float)-> String -> Picture
anyPath (f,l) (x1,y1) (x2,y2) z = if (((y2-y1)<0)&&((x2-x1)<0)) then pictures [Translate (x1+(x2-x1)/2) (y1+(y2-y1)/2)
                        $ Rotate (-(degrees.atan)      ((y2-y1)/((x2-x1)-40-f*2)))
                        $ Scale 1 0.5
                        $ arc (-180) (0) (0.5*sqrt((x1-x2+40+f*2)^2+(y1-y2)^2)),
                        Translate (x1+(0.05*sqrt((x1-x2+40+2*f)^2+(y1-y2)^2))) (y2-(y2-y1)/3)
                        $ Scale 0.1 0.1 $ Text z,
                        Line [(x1+10+f,y1),(x1+20+f,y1),(x1+f+15,y1-10)]]
                        else 
                            if (y2==y1) then pictures [Translate (x1+(x2-x1)/2) (y1+(y2-y1)/2)
                            $ Rotate (-(degrees.atan)      ((y2-y1)/((x2-x1)-40-f*2)))
                            $ Scale 1 0.5
                            $ arc (0) (180) (0.5*sqrt((x1-x2+40+f*2)^2+(y1-y2)^2)),
                            Translate (x1+(x2-x1+f*2)/3) (y1+(0.27*sqrt((x1-x2+40+2*f)^2+(y1-y2)^2)))
                            $ Scale 0.1 0.1
                            $ Text z,
                            Line [(x1+16+f,y1+9),(x1+20+f,y1),(x1+27+f,y1+7)]]
                            else
                                if (((y2-y1)>0)&&((x2-x1)>0)) then  pictures [Translate (x1+(x2-x1)/2) (y1+(y2-y1)/2)
                                    $ Rotate (-(degrees.atan)      ((y2-y1)/((x2-x1)-40-2*f)))
                                    $ Scale 1 0.5
                                    $ arc (0) (180) (0.5*sqrt((x1-x2+40+f*2)^2+(y1-y2)^2)),
                                    Translate (x1+(0.05*sqrt((x1-x2+40+2*f)^2+(y1-y2)^2))) (y2-(y2-y1)/3)
                                    $ Scale 0.1 0.1 $ Text z,Line [(x1+10+f,y1),(x1+20+f,y1),(x1+f+15,y1+10)]]
                                else if (((y2-y1)<0)&&((x2-x1)>0)) then
                                    pictures [Translate (x1+(x2-x1)/2) (y1+(y2-y1)/2)
                                    $ Rotate (-(degrees.atan)      ((y2-y1)/((x2-x1)-40-f*2)))
                                    $ Scale 1 0.5
                                    $ arc (-180) (0) (0.5*sqrt((x1-x2+40+f*2)^2+(y1-y2)^2)),
                                    Translate (x1+(0.05*sqrt((x1-x2+40+2*f)^2+(y1-y2)^2))) (y2-(y2-y1)/3)
                                    $ Scale 0.1 0.1 $ Text z,
                                    Line [(x1+10+f,y1),(x1+20+f,y1),(x1+f+15,y1-10)]]
                                        else
                                    pictures [Translate (x1+(x2-x1)/2) (y1+(y2-y1)/2)
                                    $ Rotate (-(degrees.atan)      ((y2-y1)/((x2-x1)-40-2*f)))
                                    $ Scale 1 0.5
                                    $ arc (0) (180) (0.5*sqrt((x1-x2+40+f*2)^2+(y1-y2)^2)),
                                    Translate (x1+(0.05*sqrt((x1-x2+40+2*f)^2+(y1-y2)^2))) (y2-(y2-y1)/3)
                                    $ Scale 0.1 0.1 $ Text z,Line [(x1+30+f,y1),(x1+20+f,y1),(x1+f+25,y1+10)]]

-- | This function draws self-loops 
drawSelfloops::(Float,Float)->(Float,Float)->[Expr]->[Picture]
drawSelfloops _ a []                     = []
drawSelfloops (e,l) a [Trans(c,d)]= [selfLoopAbove (e,l) a (toLabel c)]
drawSelfloops (e,l) a   t= [selfLoopAbove (e,l) a ((manyExpressions.fst.halve) (t))]
                                    ++(selfLoopbelow (e,l) a ((manyExpressions.snd.halve) (t)))

halve :: [a] -> ([a], [a]) 
halve xs = 
    ((take s xs), (drop s xs))
    where
        s = (length xs ) `div` 2
        
-- | This function makes transition with several expressions to the same state
manyExpressions::[Expr]->String
manyExpressions []               =""
manyExpressions [Trans(a,b)]     = (toLabel a)
manyExpressions ((Trans(a,b)):t) = (toLabel a)++" | "++(manyExpressions t)

--Points genarator
-- | This function generates coordenates for new states
gencoordenates::(Float,Float)->Int ->(Float,Float) -> [(Float,Float)]
gencoordenates (f,l) n (x,y) = if (n>1) then (gen (f,l) (n-1) (x+180+f*3+l,y) [(x+180+f*3+l,y)]) else [(x+180+f*3+l,y)]  

gen::(Float,Float)->Int ->(Float,Float)->[(Float,Float)]-> [(Float,Float)]
gen (f,l)  0 _      a = a
gen (f,l)  n (x,y)  t = if (even n) then (gen (f,l) (n-1) (minim t) ((x,y+180+f*3):t))
                                        else (gen (f,l) (n-1) (maxim t) ((x,y-180-f*3):t))
-- | This function finds minimun value in a tuple 
minim :: [(Float,Float)] -> (Float,Float)
minim [x]   = x
minim (x:xs)= minimun x (minim xs)

minimun :: (Float,Float) -> (Float,Float) -> (Float,Float)
minimun (x,y) (z,d)
    | y > d  = (z,d)
    | y < d  = (x,y)
    | y == d = (x,y)

-- | This function finds maximum value in a tuple 
maxim :: [(Float,Float)] -> (Float,Float)
maxim [x]   = x
maxim (x:xs)= maxe x (maxim xs)

maxe :: (Float,Float) -> (Float,Float) -> (Float,Float)
maxe (x,y) (z,d)
    | y < d  = (z,d)
    | y > d  = (x,y)
    | y == d = (x,y)
--Desenhos Genericos
automata::Picture
automata = pictures([Translate (xAxis-80) (yAxis+300)
                    $ Scale 0.2 0.2
                    $ Text "Automaton"])

-- | desenha uma seta que marca o estado inicial 
start::Picture
start = pictures[Line [(xAxis-26,yAxis+7),(xAxis-20,yAxis),(xAxis-26,yAxis-7)],
        Line [(xAxis-40,yAxis),(xAxis-20,yAxis)],
        Translate (xAxis-80) (yAxis)
        $ Scale 0.1 0.1
        $ Text "Start"]

-- | This function converts to a string 
toLabel::(String,Guard,Reset)->String
toLabel (a,b,c) = a++","++(prettyPrintingAtConstrains b)++","++(prettyPrintingReset c)
-- | This function draws states
state::(Float,Float)-> String -> (Float,Float) -> Picture
state (f,l) c (a,b)= pictures  [Translate (a) (b)
                        $ circle (20+f),
                        Translate (a-5) (b-3)
                        $ Scale 0.1 0.1
                        $ Text c]

-- | This function draws self-loops above the state 
selfLoopAbove::(Float,Float)->(Float,Float) -> String -> Picture
selfLoopAbove (f,l) (a,b) d = pictures [Translate (a) (b+40+f)
              $ Scale 0.5 1
              $ circle (20),
              Line [(a-1,b+30+f),(a,b+20+f),(a-11.5,b+22.5+f)],
              Translate (a-15) (b+65+f)
              $ Scale 0.1 0.1
              $ Text d]

-- | This function draws self-loops below the state
selfLoopbelow::(Float,Float)->(Float,Float) -> String -> [Picture]
selfLoopbelow  (f,l) (a,b) d =  [Translate (a) (b-40-f)
              $ Scale 0.5 1
              $ circle (20),
              Line [(a-1,b-30-f),(a,b-20-f),(a-11.5,b-22.5-f)],
              Translate (a-15) (b-70)
              $ Scale 0.1 0.1
              $ Text d]

degrees::Float->Float
degrees a =  a * (180/pi)

-- | This function draw general arrows 
drawPlusArrows::(Float,Float)->(Float,Float)->(Float,Float)->String->[Picture]
drawPlusArrows (f,l) (a,b) (x,y) z = if (b==y) then [Line [(a+20+f,b),(x-20-f,y)],
                            Line [(x-27-f,y+7),(x-20-f,y),(x-27-f,y-7)],
                            Translate (a+35+f) (b+5)
                            $ Scale 0.1 0.1
                            $ Text z]
                            else if (b>y) then [Line [(a+20+f,b),(x-20-f,y)],
                            Line [(x-33-f,y),(x-20-f,y),(x-20-f,y+13)],
                            Translate (a+f+(x-a)/3) (b+(y-b)/2)
                            $ Scale 0.1 0.1
                            $ Text z]
                            else [Line [(a+20+f,b),(x-20-f,y)],
                            Line [(x-30-f,y),(x-20-f,y),(x-20-f,y-10)],
                            Translate (a+f+(x-a)/3) (b+(y-b)/2)
                            $ Scale 0.1 0.1
                            $ Text z]
-- | general coordenats
xAxis::Float
xAxis = 200

yAxis::Float
yAxis = 0