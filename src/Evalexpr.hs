module Evalexpr
    where

import System.Environment
import System.Exit
import System.IO
import Text.Printf
import Data.Char
import Data.Maybe
import Parser

isOperator :: Char -> Bool -> Bool
isOperator c isNbBefore = case c == '*' || c == '/' || c == '^' of
                        True -> True
                        False -> case (c == '+' || c == '-') && isNbBefore == True of
                                True -> True
                                False -> False

isPower :: String -> Int -> Bool -> Maybe Int
isPower [] _ _ = Nothing
isPower (hd:tl) i isNbBefore = case hd == '^' of
                                True -> Just (i + 1)
                                False -> case isOperator hd isNbBefore of
                                        True -> isPower tl (i + 1) False
                                        False -> case isDigit hd of
                                                True -> isPower tl i True
                                                False -> isPower tl i False

isMulDiv :: String -> Int -> Bool -> Maybe Int
isMulDiv [] _ _ = Nothing
isMulDiv (hd:tl) i isNbBefore = case hd == '*' || hd == '/' of
                                True -> Just (i + 1)
                                False -> case isOperator hd isNbBefore of
                                        True -> isMulDiv tl (i + 1) False
                                        False -> case isDigit hd of
                                                True -> isMulDiv tl i True
                                                False -> isMulDiv tl i False

isAddMul :: String -> Int -> Int -> Bool -> Bool
isAddMul (hd:tl) pos count isNbBefore = case count >= pos of
                                        False -> case isOperator hd isNbBefore of
                                                True -> case (hd == '+' || hd == '*') && count == pos - 1 of
                                                        True -> True
                                                        False -> isAddMul tl pos (count + 1) False
                                                False -> case isDigit hd of
                                                        True -> isAddMul tl pos count True
                                                        False -> isAddMul tl pos count False
                                        True -> False


isAddSub :: String -> Int -> Bool -> Maybe Int
isAddSub [] _ _ = Nothing
isAddSub (hd:tl) i isNbBefore = case (hd == '+' || hd == '-') && isNbBefore == True of
                                True -> Just (i + 1)
                                False -> case isOperator hd isNbBefore of
                                        True -> isAddSub tl (i + 1) False
                                        False -> case isDigit hd of
                                                True -> isAddSub tl i True
                                                False -> isAddSub tl i False

reachNumber :: String -> Int -> Int -> Bool -> String
reachNumber [] _ _ _ = []
reachNumber (hd:tl) count pos isNbBefore = case count == pos of 
                                        True -> (hd:tl)
                                        False -> case isOperator hd isNbBefore of
                                                True -> reachNumber tl (count + 1) pos False
                                                False -> case isDigit hd of
                                                        True -> reachNumber tl count pos True
                                                        False -> reachNumber tl count pos False

getNumber :: String -> Int -> Maybe Double
getNumber str pos = case parseDouble (reachNumber str 0 pos False) of
                Just (nb, a) -> Just nb
                _ -> Nothing

getStringStart :: String -> String -> Int -> Int -> Bool -> String
getStringStart [] _ _ _ _ = []
getStringStart (hd:tl) buff count pos isNbBefore = case count == pos of 
                                                True -> buff
                                                False -> case isOperator hd isNbBefore of
                                                        True -> getStringStart tl (buff ++ [hd]) (count + 1) pos False
                                                        False -> case isDigit hd of
                                                                True -> getStringStart tl (buff ++ [hd]) count pos True
                                                                False -> getStringStart tl (buff ++ [hd]) count pos False

getStringEnd :: String -> Int -> Int -> Bool -> String
getStringEnd [] _ _ _ = []
getStringEnd (hd:tl) count pos isNbBefore = case isOperator hd isNbBefore of
                                                True -> case count == pos - 1 of
                                                        True -> (hd:tl)
                                                        False -> getStringEnd tl (count + 1) pos False
                                                False -> case isDigit hd of
                                                        True -> getStringEnd tl count pos True
                                                        False -> getStringEnd tl count pos False

getFinalNumber :: String -> Maybe Double
getFinalNumber str = case parseDouble str of
                        Just (nb, a) -> Just nb
                        _ -> Nothing

checkNbChar :: String -> Char -> Int -> Int
checkNbChar [] c nb = nb
checkNbChar (hd:tl) c nb = case hd == c of
        True -> checkNbChar tl c (nb + 1)
        False -> checkNbChar tl c nb

getStrBeforePar :: String -> String -> Int -> String
getStrBeforePar (hd:tl) str nb = case hd == '(' of
                        True -> case nb == 1 of
                                True -> str
                                False -> getStrBeforePar tl (str ++ [hd]) (nb - 1)
                        False -> getStrBeforePar tl (str ++ [hd]) nb

getStrMiddlePar :: String -> String -> Int -> Bool -> String
getStrMiddlePar (hd:tl) str nb state = case state == False of
                        True -> case hd == '(' of
                                True -> case nb == 1 of
                                        True -> getStrMiddlePar tl str (nb - 1) True
                                        False -> getStrMiddlePar tl str (nb - 1) False
                                False -> getStrMiddlePar tl str nb False
                        False -> case hd == ')' of
                                True -> str
                                False -> getStrMiddlePar tl (str ++ [hd]) nb True

getStrAfterPar :: String -> Int -> Bool -> String
getStrAfterPar (hd:tl) nb state = case state == False of
                        True -> case hd == '(' of
                                True -> case nb == 1 of
                                        True -> getStrAfterPar tl (nb - 1) True
                                        False -> getStrAfterPar tl (nb - 1) False
                                False -> getStrAfterPar tl nb False
                        False -> case hd == ')' of
                                True -> tl
                                False -> getStrAfterPar tl nb True

evalexpr :: String -> IO ()
evalexpr str = case checkNbChar str '(' 0 >= 1 of
                        True -> case resultStrPar of
                                Nothing -> hPutStrLn stderr "Error: Wrong expression (Mathematical Problem with parenthesis)" >> exitWith (ExitFailure 84)
                                _ -> evalexpr (strBeforePar ++ (show (fromJust resultStrPar) :: String) ++ strAfterPar)
                        False -> case evalExprNd str of
                                Nothing -> hPutStrLn stderr "Error: Wrong expression (Mathematical Problem)" >> exitWith (ExitFailure 84)
                                _ -> printf "%.2f\n" $ fromJust (evalExprNd str)
        where
            strBeforePar = getStrBeforePar str "" (checkNbChar str '(' 0)
            resultStrPar = evalExprNd (getStrMiddlePar str "" (checkNbChar str '(' 0) False)
            strAfterPar = getStrAfterPar str (checkNbChar str '(' 0) False

evalExprNd :: String -> Maybe Double
evalExprNd str = case posPower of
            Nothing -> case posMulDiv of
                        Nothing -> case posAddSub of
                                    Nothing -> getFinalNumber str
                                    _ -> case getNumber str (fromJust posAddSub) of
                                        Nothing -> Nothing
                                        _ -> case isAddMul str (fromJust posAddSub) 0 False of
                                                True -> evalExprNd (startStrAddSub ++ (show resAdd :: String) ++ endStrAddSub)
                                                False -> evalExprNd (startStrAddSub ++ (show resSub :: String) ++ endStrAddSub)
                        _ -> case getNumber str (fromJust posMulDiv) of
                                Nothing -> Nothing
                                _ -> case isAddMul str (fromJust posMulDiv) 0 False of
                                        True ->  evalExprNd (startStrMulDiv ++ (show resMul :: String) ++ endStrMulDiv)
                                        False -> case fromJust (getNumber str (fromJust posMulDiv)) == 0 of
                                                True -> Nothing
                                                False -> evalExprNd (startStrMulDiv ++ (show resDiv :: String) ++ endStrMulDiv)
            _ -> case getNumber str (fromJust posPower) of
                    Nothing -> Nothing
                    _ -> evalExprNd (startStrPower ++ (show resPow :: String) ++ endStrPower)
        where
            posAddSub = isAddSub str 0 False
            posMulDiv = isMulDiv str 0 False
            posPower = isPower str 0 False
            resAdd = fromJust (getNumber str (fromJust posAddSub - 1)) + fromJust (getNumber str (fromJust posAddSub))
            resSub = fromJust (getNumber str (fromJust posAddSub - 1)) - fromJust (getNumber str (fromJust posAddSub))
            resMul = fromJust (getNumber str (fromJust posMulDiv - 1)) * fromJust (getNumber str (fromJust posMulDiv))
            resDiv = fromJust (getNumber str (fromJust posMulDiv - 1)) / fromJust (getNumber str (fromJust posMulDiv))
            resPow = fromJust (getNumber str (fromJust posPower - 1)) ^ (round (fromJust (getNumber str (fromJust posPower))) :: Int)
            startStrAddSub = getStringStart str "" 0 (fromJust posAddSub - 1) False
            endStrAddSub = getStringEnd str 0 (fromJust posAddSub + 1) False
            startStrMulDiv = getStringStart str "" 0 (fromJust posMulDiv - 1) False
            endStrMulDiv = getStringEnd str 0 (fromJust posMulDiv + 1) False
            startStrPower = getStringStart str "" 0 (fromJust posPower - 1) False
            endStrPower = getStringEnd str 0 (fromJust posPower + 1) False