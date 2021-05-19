module ParsingUtils where

import RegularExpression
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number

parserOfNames :: GenParser Char st String
parserOfNames = 
      do eatspaces
         x <-many (noneOf "=,\n()|!*{}[]%&$#<>=:@~^/-+ ")
         eatspaces
         return (x)

-- | The end of line character is \n
eol :: GenParser Char st Char
eol = char '\n'

-- | This function is used to parse names
parserNameList :: GenParser Char st [String]
parserNameList = do first <- parserOfNames
                    next <- remainingNames
                    return (first : next)
                 where remainingNames = (char ',' >> parserNameList) <|> (return [])  

-- | This function is used to parse state and clock/Guardiant list
parserList :: GenParser Char st a ->  GenParser Char st b -> GenParser Char st [(a,b)]
parserList p1 p2 = 
  do first <- (parserPairs p1 p2)
     next <- remainingElements
     return (first : next)
  where
     remainingElements = (char ',' >> (parserList p1 p2)) <|> (return [])

parserPairs :: GenParser Char st a ->  GenParser Char st b -> GenParser Char st (a,b)
parserPairs p1 p2 =
  do eatspaces
     char '('
     eatspaces
     x <- p1
     eatspaces
     char ','
     eatspaces
     y <- p2
     eatspaces
     char ')'
     eatspaces
     return (x,y)
-----------------------Parsing Transtion-----------------------------------------------
-- This function is used to parse the transitions lis
parserTransList :: GenParser Char st [(String,(String,Guard,Reset),String)]
parserTransList = 
    do first <- transContent 
       next <- remainingTrans
       return (first :next)

-- | This function parses what comes after a comma
remainingTrans :: GenParser Char st [(String,(String,Guard,Reset),String)]
remainingTrans =
    (char ',' >> parserTransList)
    <|> (return [])  

transContent :: GenParser Char st (String,(String,Guard,Reset),String)
transContent =
  do eatspaces
     char '('
     x <- parserOfNames
     char ','
     eatspaces
     y <- parserLabel
     eatspaces
     char ','
     z <- parserOfNames
     char ')'
     eatspaces
     return (x,y,z)

-----------------------Parsing Label and Guardiant-----------------------------------------------

-- | This function parses labels
parserLabel::GenParser Char st (String,Guard,Reset)
parserLabel =
      do
        eatspaces
        char '('
        eatspaces
        x <- parserOfNames
        eatspaces
        char ','
        eatspaces
        y <- parserGuard
        eatspaces
        char ','
        eatspaces
        z <- parserReset
        eatspaces
        char ')'
        eatspaces
        return (x,y,z)

parserReset::GenParser Char st Reset
parserReset = choice [try parserAnd, try parserRes, try parserAtomTr ]  where
  parserAnd = do
    -- parses the conjuction of Predicats
    eatspaces
    l <- betweenParens parserReset
    eatspaces
    char '&'
    eatspaces
    r <- betweenParens parserReset
    eatspaces
    return (AndR l r)
  parserRes = do
    eatspaces
    x <- parserOfNames
    eatspaces
    y <- string ":="
    eatspaces
    z <- possiblyFloat
    eatspaces
    return (AtmR (AtomReset x y z))
  parserAtomTr  = do
    eatspaces
    x <- string "true"
    eatspaces
    return (AtmR (Tru x))  


parserGuard :: GenParser Char st Guard
parserGuard =  choice [try parserAnd, try parserOr, try parserNeg,   
                      try parserAtom0, try parserAtom1, try parserAtom2,
                       try parserAtomT, try parserAtom3]  where
  parserAnd = do
    -- parses the conjuction of Predicats
    eatspaces
    l <- betweenParens parserGuard 
    eatspaces
    char '&'
    eatspaces
    r <- betweenParens parserGuard
    eatspaces
    return (And l r)
    -- parses the dijunction of Predicats
  parserOr = do
    eatspaces
    l <- betweenParens parserGuard
    eatspaces
    char '|'
    eatspaces
    r <- betweenParens parserGuard
    eatspaces
    return (Or l r)
    -- parses the negation of Predicats
  parserNeg = do
    eatspaces
    char '!'
    eatspaces
    x <- betweenParens parserGuard
    eatspaces
    return (Not x)
    -- parses the Atom0
  parserAtom0 = do
    x <- parserOfNames
    eatspaces
    char ':'
    eatspaces
    char '['
    n <- possiblyFloat
    char ','
    m <- possiblyFloat
    char ']'
    eatspaces
    return (Atom (AtomPred x n m))
    -- parses the Atom
  parserAtom1 = do
    eatspaces
    x <- parserOfNames
    eatspaces
    y <- logicContent
    eatspaces
    z <- possiblyFloat
    eatspaces
    return (Atom (AtomL0 x y z))
    -- parses the atom Predicats
  parserAtom2 = do
    eatspaces
    x <- possiblyFloat
    eatspaces
    y <- logicContent
    eatspaces
    z <- parserOfNames 
    eatspaces
    return (Atom (AtomL1 x y z))
      -- parses the atom Predicats
  parserAtom3 = do
    eatspaces
    x <- parserOfNames
    eatspaces
    y <- logicContent
    eatspaces
    z <- parserOfNames 
    eatspaces
    return (Atom (AtomL2 x y z))
    -- parses the true Predicats
  parserAtomT  = do
    eatspaces
    x <- string "true"
    eatspaces
    return (Atom (Tr x))  

logicContent::GenParser Char st String
logicContent = choice [try (string "<="),
                       try (string ">="),
                       try (string "!="),
                       try (string ":="),
                       try (string "="), 
                       try (string "<"), 
                       try (string ">")]

eatspaces = many (char ' ')

betweenParens p = do
  eatspaces
  char '('
  eatspaces
  x <- p
  eatspaces
  char ')'
  eatspaces
  return x

possiblyFloat = (floating2 True) 
                <|> (floating2 False) 