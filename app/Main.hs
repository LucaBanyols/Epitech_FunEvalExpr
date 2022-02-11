module Main where

import System.Environment
import System.Exit
import System.IO
import Lib
import Parser
import Evalexpr
import Text.Printf
import Data.Char
import Data.Maybe

isOpSign :: Char -> Bool
isOpSign c = case c == '+' || c == '-' || c == '*' || c == '/' || c == '^' of
            True -> True
            False -> False

isValidStart :: String -> Bool
isValidStart [] = False
isValidStart (hd:tl) = case hd == '(' || hd == '-' of
                    True ->  isValidStart tl
                    False -> case isDigit hd == True of
                        True -> True 
                        False -> False

isValidStr :: String -> Bool
isValidStr [] = True
isValidStr (c:tl) = case isOpSign c == False && c /= '(' && c /= ')' 
                    && c /= '.' && c /= 'e' && isDigit c == False of
                True -> False
                False -> isValidStr tl

areEnchainedOperators :: String -> Char -> Bool
areEnchainedOperators [] _ = False
areEnchainedOperators (hd:tl) c = case isOpSign c == True && isOpSign hd == True of
                                True -> True
                                False -> areEnchainedOperators tl hd

areInvertedPar :: String -> Bool
areInvertedPar [] = False
areInvertedPar (hd:tl) = case hd == '(' of
                    True -> False
                    False -> case hd == ')' of
                        True -> True
                        False -> areInvertedPar tl

isDblNbWithoutSign :: String -> Bool
isDblNbWithoutSign [] = False
isDblNbWithoutSign (hd:tl) = case isDigit hd of
                    True -> case afterFirstNb (hd:tl) of
                        True -> True
                        False -> isDblNbWithoutSign tl
                    False -> isDblNbWithoutSign tl

afterFirstNb :: String -> Bool
afterFirstNb [] = False
afterFirstNb (hd:tl) = case isDigit hd of
                    True -> afterFirstNb tl
                    False -> case hd == ' ' || hd == '\t' || hd == '\a' || hd == '\b' ||
                            hd == '\n' || hd == '\v' || hd == '\f' || hd == '\r' of
                        True -> case spaceAfterFirstNb tl of
                            True -> True
                            False -> isDblNbWithoutSign tl
                        False -> isDblNbWithoutSign tl

spaceAfterFirstNb :: String -> Bool
spaceAfterFirstNb [] = False
spaceAfterFirstNb (hd:tl) = case hd == ' ' || hd == '\t' || hd == '\a' || hd == '\b' ||
                                hd == '\n' || hd == '\v' || hd == '\f' || hd == '\r' of
                        True -> spaceAfterFirstNb tl
                        False -> case isDigit hd of
                            True -> True
                            False -> isDblNbWithoutSign tl

main :: IO ()
main = do
       args <- getArgs
       case args of
            (hd:tl) -> case tl == [] of 
                True -> case checkNbChar hd '(' 0 == checkNbChar hd ')' 0 of
                    True -> case isDblNbWithoutSign hd == False of
                        True -> case isValidStart (removeSpaces hd "") of
                                True -> case isValidStr (removeSpaces hd "") of
                                        True -> case areEnchainedOperators (removeSpaces hd "") '0' of
                                            True -> hPutStrLn stderr "Error: Wrongs expression (Enchained Operators)" >> exitWith (ExitFailure 84)
                                            False -> case areInvertedPar (removeSpaces hd "") of
                                                True -> hPutStrLn stderr "Error: Wrongs expression (Interverted Parenthesis)" >> exitWith (ExitFailure 84)
                                                False -> evalexpr (removeSpaces hd "")
                                        False -> hPutStrLn stderr "Error: Wrongs expression (Invalid Character)" >> exitWith (ExitFailure 84)
                                False -> hPutStrLn stderr "Error: Wrongs expression (invalid start of expression)" >> exitWith (ExitFailure 84)
                        False -> hPutStrLn stderr "Error: Double Number Without Sign" >> exitWith (ExitFailure 84)
                    False -> hPutStrLn stderr "Error: Wrongs expression (parentheses open/close are incorrect)" >> exitWith (ExitFailure 84)
                False -> hPutStrLn stderr "Error: Too Many Arguments" >> exitWith (ExitFailure 84)
            _ -> hPutStrLn stderr "Error: Argument Needed" >> exitWith (ExitFailure 84)