{-# LANGUAGE MultiWayIf #-} -- Что это??



module Main where

import Data.Array.IArray
import Data.Ix
import Control.Monad
import Text.Read


{-
import Lib

analyzeGold :: Int -> String
analyzeGold standard =
    if  | standard == 999 -> "Wow! 999 standard!"
        | standard == 750 -> "Great! 750 standard."
        | standard == 585 -> "Not bad! 585 standard."
        | otherwise -> "I don’t know such a standard..."


analyzeGold :: Int -> String
analyzeGold standard
    | standard == 999 = ”Wow! 999 standard!”
    | standard == 750 = ”Great! 750 standard.”
    | standard == 585 = ”Not bad! 585 standard.”
    | otherwise = ”I don’t know such a standard...”
    

analyzeGold standard =
case standard of
    999 -> ”Wow! 999 standard!”
    750 -> ”Great! 750 standard.”
    585 -> ”Not bad! 585 standard.”
    _   -> ”I don’t know such a standard...”
    
    
let threshold = 40
    in
    if  | timeInS < threshold -> timeInS + correction
        | otherwise -> timeInS + delta + correction
    where
        delta = correction - 4
        correction = timeInS * 2
-}



{-
test :: String -> String
test str = str ++ (show squares2)
    where   squares =  array (1,10) [(i, i*i) | i <- [1..10]] :: Array Int Int
            squares2 = squares // [(i, 0) | i <- [1,3..9]]
            first = (squares ! 1) 
-}        
data Env = Env 
    { ws :: Array Int String --address space
    , wc :: Int              --word counter
    , xs :: [Int]            --data stack
    , rs :: [Int]            --returns
    --, ls :: [Int]            --stack of loop counters
    , as :: [(String, Int)]  --Articles
    , sb :: String           --Buffer
    } deriving (Show)
   

performOp ::  Env -> (Int -> Int -> Int) -> Env
performOp env op  = env {xs = res:rest, wc = (wc env)+1}
    where   first  = xs env !! 0
            second = xs env !! 1
            res = op second first
            rest = tail $ tail $ xs env
            
printLast :: Env -> Env
printLast env = env {sb = sb env ++ (show . head . xs) env, wc = (wc env)+1}

printStack :: Env -> Env
printStack env = env {sb = sb env ++ (show . xs) env, wc = (wc env)+1}


defineSmth :: Env -> Env
defineSmth env = 
    if  | command == "define" -> defineSmth newenv
        | command == "end"   -> env {wc = curwc + 1}
        | otherwise          -> defineSmth env {wc = curwc + 1}
    where isItEnd = curwc >= (length $ ws env)
          curwc   = (wc env)
          command = (ws env) ! curwc  
          nextwc  = curwc + 1
          pair    = ((ws env) ! nextwc, nextwc + 1)
          newenv  = env {as = (pair : (as env)), wc = curwc + 2} 

findName :: String -> [(String, Int)] -> Int
findName name ((aname, adr) : rest) = 
    if | name == aname -> adr
       | otherwise     -> findName name rest
       

callSmth :: Env -> Env
callSmth env = process env {rs = newrs, wc = adr}
    where adr    = findName name (as env)
          curwc  = (wc env)
          name   = (ws env) ! curwc
          newrs  = (curwc + 1) : (rs env)    


processIf :: Env -> (Int -> Int -> Bool) -> String -> Env
processIf env op ifAlias = 
    if  | isItT     -> process   env {wc = curwc + 2}
        | isItF     -> processIf env {wc = curwc + 2} op ifAlias
        | isItThen  -> process   env {wc = curwc + 1}
        | otherwise -> processIf env {wc = curwc + 1} op ifAlias
    where isItT     = (command == ifAlias) && (ifValue `op` value)
          isItF     = (command == ifAlias)
          isItThen  = (command == "endif")
          curwc     = (wc env) 
          command   = (ws env) ! curwc
          nextwc    = (wc env) + 1
          ifValue   = (read $ (ws env) ! nextwc) :: Int
          value     = head $ xs $ env
          

process :: Env -> Env 
process env = 
    if  | eoa             -> env
        | isItInt         -> process $ env {xs = (read command) : (xs env), wc = curwc+1}
        | command == "+"  -> process $ performOp env (+)
        | command == "*"  -> process $ performOp env (*)
        | command == "/"  -> process $ performOp env div
        | command == "-"  -> process $ performOp env (-)
        | command == "^"  -> process $ performOp env (^)
        | command == "."  -> process $ printLast env
        | command == ".s" -> process $ printStack env
        | isItDup         -> process $ env {xs = (head $ xs $ env) : (xs env), wc = curwc+1}
        | isItIf          -> process $ processIf env (==) "if"
        | isItIfN         -> process $ processIf env (/=) "ifn"
        | isItEndIf       -> process $ env {wc = curwc+1} 
        | isItDef         -> process $ defineSmth env
        | isItEnd         -> process $ env {rs = restOfRs, wc = newwc}
        | otherwise       -> callSmth env
    where curwc    = (wc env)
          command  = (ws env) ! curwc
          -- предикаты
          eoa      = curwc >= (length $ ws env) -- end of array
          isItInt  = ((readMaybe command) :: Maybe Int) /= Nothing 
          isItDef  = command == "define"
          isItEnd  = command == "end"
          isItIf   = command == "if"
          isItIfN  = command == "ifn"
          isItEndIf= command == "endif"
          isItDup  = command == "dup"
          -- значения для возврата
          restOfRs = tail $ rs env
          newwc    = head $ rs env

parse :: Env -> String -> Env
parse env line = process env { ws = newws }
    where   newenv = env {ws = newws}
            newcommands = words line :: [String]
            commands = (elems $ ws env) ++ newcommands
            len = length commands :: Int
            newws = listArray (0, len-1) commands :: Array Int String
                          

loop :: Env -> IO()
loop env = do 
  line <- getLine 
  unless (line == "q") $ do
    newEnv <- return $ parse env line
    --print $ (ws env) ! (wc env)
    --print $ (elems $ ws env) ++ (words line)
    -- output <- return $ (sb newEnv)
    let output = sb newEnv
        flag = length (sb newEnv) > 0
    when flag $ print output
    --print newEnv 
    loop newEnv {sb = ""}
    --loop $ parse env line

interpriter :: IO()
interpriter = do
  loop env
  where env = Env ws wc xs rs as sb
        ws = array (0,0) [(0, "start_point")] :: Array Int String
        wc = 1
        xs = [] :: [Int]
        rs = [] :: [Int]
        as = [] :: [(String, Int)]
        sb = ""


main :: IO()
main = interpriter 

{-
    m a -> (a -> m b) -> m b
    https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/guide-to-ghc-extensions/basic-syntax-extensions


    IF 12 FILL-CARTON  THEN
    12 = IF 
    
    define fab ifn 1 dup 1 - fab * endif end
        
-}


