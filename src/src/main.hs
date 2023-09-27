import ParseChar
import ParseOr
import ParseAnd
import ParseInt


-- Fonction pour tester `parseChar`
testParseChar :: IO ()
testParseChar = do
    print $ parseChar 'a' "apple"  -- Doit retourner Just ('a', "pple")
    print $ parseChar 'b' "apple"  -- Doit retourner Nothing
    print $ parseChar 'a' ""       -- Doit retourner Nothing

-- Fonction pour tester `parseAnyChar`
testParseAnyChar :: IO ()
testParseAnyChar = do
    print $ parseAnyChar "aeiou" "apple"     -- Doit retourner Just ('a', "pple")
    print $ parseAnyChar "aeiou" "banana"    -- Doit retourner Just ('a', "nana")
    print $ parseAnyChar "aiou" "cherry"    -- Doit retourner Nothing
    print $ parseAnyChar "aeiou" ""          -- Doit retourner Nothing

-- Fonction pour tester `parseOr`
testParseOr :: IO ()
testParseOr = do
    let parserA = parseChar 'a'
    let parserB = parseChar 'b'

    print $ parseOr parserA parserB "apple"  -- Doit retourner Just ('a', "pple")
    print $ parseOr parserA parserB "banana" -- Doit retourner Just ('b', "anana")
    print $ parseOr parserA parserB "cherry" -- Doit retourner Nothing
    print $ parseOr parserA parserB ""       -- Doit retourner Nothing

-- Fonction pour tester `parseAnyCharUsingOr`
testParseAnyCharUsingOr :: IO ()
testParseAnyCharUsingOr = do
    print $ parseAnyCharUsingOr "aeiou" "apple"     -- Doit retourner Just ('a', "pple")
    print $ parseAnyCharUsingOr "aeiou" "banana"    -- Doit retourner Just ('a', "nana")
    print $ parseAnyCharUsingOr "aiou" "cherry"    -- Doit retourner Nothing
    print $ parseAnyCharUsingOr "aeiou" ""          -- Doit retourner Nothing
    print $ parseAnyCharUsingOr "abc" "banana"      -- Doit retourner Just ('b', "anana")
    print $ parseAnyCharUsingOr "xyz" "banana"      -- Doit retourner Nothing

-- Fonction pour tester `parseAnd`
testParseAnd :: IO ()
testParseAnd = do
    let parserA = parseChar 'a'
    let parserB = parseChar 'b'

    -- Doit reconnaître 'a' suivi de 'b'.
    print $ parseAnd parserA parserB "apple"  -- Doit retourner Nothing
    print $ parseAnd parserA parserB "banana" -- nothing
    print $ parseAnd parserA parserB "abacus" -- Doit retourner Just (('a', 'b'), "acus")

-- Fonction pour tester `parseAndWith`
testParseAndWith :: IO ()
testParseAndWith = do
    let combine = \x y -> [x,y]

    -- Devrait combiner les deux caractères reconnus en une chaîne.
    print $ parseAndWith combine (parseChar 'a') (parseChar 'b') "apple"  -- Doit retourner Nothing
    print $ parseAndWith combine (parseChar 'a') (parseChar 'b') "banana" -- Nothing
    print $ parseAndWith combine (parseChar 'a') (parseChar 'b') "abacus" -- Doit retourner Just ("ab", "acus")


-- Fonction pour tester `parseMany`
testParseMany :: IO ()
testParseMany = do
    -- Devrait reconnaître autant de caractères 'a' que possible.
    print $ parseMany (parseChar 'a') "aaaaapple"  -- Doit retourner Just ("aaaaa", "pple")
    print $ parseMany (parseChar 'a') "banana"     -- Doit retourner Just ("", "banana")
    print $ parseMany (parseChar 'a') "cherry"     -- Doit retourner Just ("", "cherry")
    print $ parseMany (parseChar 'a') ""           -- Doit retourner Just ("", "")

-- Fonction pour tester `parseSome`
testParseSome :: IO ()
testParseSome = do
    print $ parseSome (parseChar 'a') "aaaaapple"  -- Doit retourner Just ("aaaaa", "pple")
    print $ parseSome (parseChar 'a') "banana"     -- Doit retourner Nothing
    print $ parseSome (parseChar 'a') "cherry"     -- Doit retourner Nothing car il n'y a pas d'"a" au début.
    print $ parseSome (parseChar 'a') "apple"      -- Doit retourner Just ("a", "pple")
    print $ parseSome (parseChar 'a') "aa apple"   -- Doit retourner Just ("aa", " apple")

testParseUInt :: IO ()
testParseUInt = do
    putStrLn "\nTesting parseUInt:"
    print $ parseUInt "12345apple"    -- Doit retourner Just (12345, "apple")
    print $ parseUInt "0banana"       -- Doit retourner Just (0, "banana")
    print $ parseUInt "cherry"        -- Doit retourner Nothing car il n'y a pas de nombre au début.
    print $ parseUInt "98765"         -- Doit retourner Just (98765, "")
    print $ parseUInt "42 "           -- Doit retourner Just (42, " ") car il devrait également parser les espaces.

-- Fonction pour tester `parseInt`
testParseInt :: IO ()
testParseInt = do
    putStrLn "\nTesting parseInt:"
    print $ parseInt "+12345apple"   -- Doit retourner Just (12345, "apple")
    print $ parseInt "-123banana"    -- Doit retourner Just (-123, "banana")
    print $ parseInt "cherry"        -- Doit retourner Nothing car il n'y a pas de nombre au début.
    print $ parseInt "-42"           -- Doit retourner Just (-42, "")
    print $ parseInt "42"            -- Doit retourner Just (42, "")
    print $ parseInt "+0apple"       -- Doit retourner Just (0, "apple")


-- Modification de la fonction main pour inclure les nouveaux tests
main :: IO ()
main = do
    putStrLn "Testing parseChar:"
    testParseChar

    putStrLn "\nTesting parseAnyChar:"
    testParseAnyChar

    putStrLn "\nTesting parseOr:"
    testParseOr

    putStrLn "\nTesting parseAnyCharUsingOr:"
    testParseAnyCharUsingOr

    putStrLn "\nTesting parseAnd:"
    testParseAnd

    putStrLn "\nTesting parseAndWith:"
    testParseAndWith

    putStrLn "\nTesting parseMany:"
    testParseMany

    putStrLn "\nTesting parseSome:"
    testParseSome

    putStrLn "\nTesting parseUInt:"
    testParseUInt

    putStrLn "\nTesting parseInt:"
    testParseInt

