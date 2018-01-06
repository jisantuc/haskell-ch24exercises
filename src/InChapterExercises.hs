module InChapterExercises where


import Control.Applicative
import Text.Trifecta

-- some parsers

one :: Parser Char
one = char '1'

two :: Parser Char
two = char '2'

oneTwo :: Parser Char
oneTwo = one >> two

oneOrTwo :: Parser Char
oneOrTwo = one <|> two

-- Parsing Exercises
-- 1. Make a parser fail by not exhausting all the input

oneTwoFail :: Parser Char
oneTwoFail = oneTwo <* eof

-- parseString oneTwoFail mempty "12"
-- parseString oneTwoFail mempty "123"

-- 2. Use string to make a parser that parses "1", "123", and "123"
oneOrOneTwoOrOneTwoThree :: Parser String
oneOrOneTwoOrOneTwoThree =
  string "123" <* eof
  <|> string "12" <* eof
  <|> string "1" <* eof

-- 3. a parser that does what string does, but using char
-- I think this is the idea?
stringLikeChar :: Parser [Char]
stringLikeChar = do
  one <- char '1'
  two <- char '2'
  three <- char '3'
  return $ [one, two, three]

-- Unit of Success
-- This isn't a great return type when our intent is obviously to get the integer
aUnit :: Result ()
aUnit = parseString (integer >> eof) mempty "123"

-- making it better by returning the integer
intParser :: Parser Integer
intParser = integer <* eof

anInt :: Result Integer
anInt = parseString intParser mempty "123"

aFailedInt :: Result Integer
aFailedInt = parseString intParser mempty "123abc"
