module Parser where
--imports the modules utilized
import RegularExpression
import ParsingUtils
--imports the liberary utilized 
import Text.ParserCombinators.Parsec

 -- | This file contains the parser of regular expressions + corresponding functions.
parseFile :: String -> Either ParseError TAut
parseFile = parse fileParser "(unknown)"
  where fileParser =
          do initSt <- nameContent --  Initial State
             eol    
             stList <- parserNameList --  List of States
             eol
             clList <- parserNameList --  List of Clocks
             eol
             clAndStList <- (parserList nameContent clockContent)
                            <|> parseNoClock    --  List  (StateName,Clock)
             eol
             transList <- parserTransList --  List of (StateName,Label,StateName)
             return ( (initSt,stList),(clList, clAndStList), transList)

-- | This function is used in case there isn´t clock
parseNoClock::GenParser Char st [(a,b)]
parseNoClock = do 
    eatspaces
    x <- string "no"
    eatspaces
    return ([])

-- | This function is used to parse  clock/Guard tuple
paraPairs :: GenParser Char st a ->  GenParser Char st b -> GenParser Char st (a,b)
paraPairs p1 p2 =
  do char '('
     x <- p1 --  State Name
     char ','
     y <- p2 --  Clock
     char ')'
     return (x,y)

-- | This function parses what comes after a comma
tremainingElements :: GenParser Char st a ->  GenParser Char st b -> GenParser Char st [(a,b)]
tremainingElements p1 p2 = 
    (char ',' >> (parserList p1 p2))
    <|> (return [])


remainingNames :: GenParser Char st [String]
remainingNames =
    (char ',' >> parserNameList)
    <|> (return [])  

-- | This function limits the number of characters that can be placed on states
nameContent :: GenParser Char st String
nameContent = many (noneOf ",\n) ")


clockContent :: GenParser Char st String
clockContent = many (noneOf ",\n )")