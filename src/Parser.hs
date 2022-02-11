module Parser
    where

import Data.Char
import Data.Maybe

-- From:
type Parser a = String -> Maybe (a, String)
-- To:
-- newtype Parser a = Parser {
--     runParser :: String -> Maybe (a, String)
-- }

parseChar :: Char -> Parser Char
parseChar c (hd:tl) = case c == hd of
                    True -> Just (c, tl)
                    False -> Nothing

-- parseChar :: Char -> Parser Char
-- parseChar c = Parser pchar
--     where pchar (hd:tl) | c == hd = Just (c, tl)
--                         | otherwise = Nothing

parseAnyChar :: String -> Parser Char
parseAnyChar [] (hd:tl) = Nothing
parseAnyChar (x:xs) [] = Nothing
parseAnyChar (x:xs) (hd:tl) = case x == hd of
                    True -> Just (x, tl)
                    False -> parseAnyChar xs (hd:tl)

-- parseAnyChar :: String -> Parser Char
-- parseAnyChar (x:xs) = Parse panychar
--     where panychar (hd:tl)  | (x:xs) == [] = Nothing
--                             |

parseOr :: Parser a -> Parser a -> Parser a
parseOr a b str = case isNothing (a str) of
                    True -> b str
                    False -> a str

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd a b str = case a str of
                    Nothing -> Nothing
                    Just (x,xs) -> case b xs of
                                        Nothing -> Nothing
                                        Just (hd, tl) -> Just ((x, hd), tl)

parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith truple a b str = case parseAnd a b str of
                    Nothing -> Nothing
                    Just((x, hd), tl) -> Just ((truple x hd), tl)

parseMany :: Parser a -> Parser [a]
parseMany _ [] = Just ([], [])
parseMany a (x:xs) = case a (x:xs) of
                        Nothing -> Just ([], x:xs)
                        _ -> Just (hd:st, en)
    where
        Just (hd,tl) = a (x:xs)
        Just (st,en) = parseMany a tl

parseSome :: Parser a -> Parser [a]
parseSome a (x:xs) = case parseMany a (x:xs) of
                        Just (st:en, hd:tl) -> Just(st:en, hd:tl)
                        _ -> Nothing

parseUint :: Parser Int
parseUint (x:xs) = case (parseMany (parseAnyChar ['0'..'9']) (x:xs)) of
                        Just (st:en, hd:tl) -> Just ((read (st:en) :: Int), hd:tl)
                        _ -> Nothing

parseInt :: Parser Int
parseInt (x:xs) = case x == '-' of
                        True -> case (parseMany (parseAnyChar ['0'..'9']) xs) of
                            Just (st:en, hd:tl) -> Just ((read (x:st:en) :: Int), hd:tl)
                            _ -> Nothing
                        False -> case (parseMany (parseAnyChar ['0'..'9']) (x:xs)) of
                            Just (st:en, hd:tl) -> Just ((read (st:en) :: Int), hd:tl)
                            _ -> Nothing

parseTuple :: Parser a -> Parser (a, a)
parseTuple a (x:xs) = case x == '(' of
                        True -> case a xs of
                            Just (nbone, hd:tl) -> case hd == ',' of
                                True -> case a tl of
                                    Just (nbtwo, zh:zt) -> Just ((nbone, nbtwo), zt)
                                    _ -> Nothing
                                False -> Nothing
                            _ -> Nothing
                        False -> Nothing

parseDouble :: Parser Double
parseDouble [] = Nothing
parseDouble (x:xs) = case x of
                        '-' -> case parseMany (parseAnyChar "0123456789e") xs of
                            Just (nbstart, hd:tl) -> case hd of
                                '.' -> case parseMany (parseAnyChar "0123456789e") tl of
                                    Just (nblast, zh:zt) -> Just ((read (("-" ++ nbstart) ++ "." ++ nblast) :: Double), zh:zt)
                                    Just (nblast, []) -> Just ((read (("-" ++ nbstart) ++ "." ++ nblast) :: Double), [])
                                _ -> Just ((read ("-" ++ nbstart) :: Double), hd:tl)
                            Just (nbstart, _) -> Just ((read ("-" ++ nbstart) :: Double), [])
                            _ -> Nothing
                        _ -> case parseMany (parseAnyChar "0123456789e") (x:xs) of
                            Just (nbstart, hd:tl) -> case hd of
                                '.' -> case parseMany (parseAnyChar "0123456789e") tl of
                                    Just (nblast, zh:zt) -> Just ((read (nbstart ++ "." ++ nblast) :: Double), zh:zt)
                                    Just (nblast, []) -> Just ((read (nbstart ++ "." ++ nblast) :: Double), [])
                                _ -> Just ((read nbstart :: Double), hd:tl)
                            Just (nbstart, _) -> Just ((read nbstart :: Double), [])
                            _ -> Nothing

removeSpaces :: String -> String -> String
removeSpaces [] [] = ""
removeSpaces [] str = str
removeSpaces (hd:tl) str = case hd == ' ' || hd == '\a' || hd == '\b' ||
                            hd == '\t' || hd == '\n' || hd == '\v' ||
                            hd == '\f' || hd == '\r' of
                        True -> removeSpaces tl str
                        False -> removeSpaces tl (str ++ [hd])