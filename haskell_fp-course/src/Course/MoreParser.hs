{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Course.MoreParser where

import Course.Applicative
import Course.Core
import Course.Functor
import Course.List
import Course.Monad
import Course.Optional
import Course.Parser
import Course.Traversable
import Numeric hiding (readHex)

{- $setup
>>> :set -XOverloadedStrings
>>> import Course.Parser(isErrorResult, character, lower, is)
>>> import Data.Char(isUpper, isLower)
-}

{- | Parses the given input and returns the result.
The remaining input is ignored.
-}
(<.>) ::
  Parser a ->
  Input ->
  Optional a
P p <.> i =
  case p i of
    Result _ a -> Full a
    _ -> Empty

{- | Write a parser that will parse zero or more spaces.

>>> parse spaces " abc"
Result >abc< " "

>>> parse spaces "abc"
Result >abc< ""
-}
spaces ::
  Parser Chars
spaces = list space

{- | Write a function that applies the given parser, then parses 0 or more spaces,
then produces the result of the original parser.


>>> parse (tok (is 'a')) "a bc"
Result >bc< 'a'

>>> parse (tok (is 'a')) "abc"
Result >bc< 'a'
-}
tok ::
  Parser a ->
  Parser a
tok a = a <* spaces

{- | Write a function that parses the given char followed by 0 or more spaces.

>>> parse (charTok 'a') "abc"
Result >bc< 'a'

>>> isErrorResult (parse (charTok 'a') "dabc")
True
-}
charTok ::
  Char ->
  Parser Char
charTok a = is a <* spaces

{- | Write a parser that parses a comma ',' followed by 0 or more spaces.

>>> parse commaTok ",123"
Result >123< ','

>>> isErrorResult( parse commaTok "1,23")
True
-}
commaTok ::
  Parser Char
commaTok = charTok ','

{- | Write a parser that parses either a double-quote or a single-quote.


>>> parse quote "'abc"
Result >abc< '\''

>>> parse quote "\"abc"
Result >abc< '"'

>>> isErrorResult (parse quote "abc")
True
-}
quote ::
  Parser Char
quote = is '\'' ||| is '"'

{- | Write a function that parses the given string (fails otherwise).

>>> parse (string "abc") "abcdef"
Result >def< "abc"

>>> isErrorResult (parse (string "abc") "bcdef")
True
-}
string ::
  Chars ->
  Parser Chars
string chars = traverse id $ is <$> chars

-- string Nil = pure Nil
-- string (a :. r) = is a .:. string r

{- | Write a function that parses the given string, followed by 0 or more spaces.


>>> parse (stringTok "abc") "abc  "
Result >< "abc"

>>> isErrorResult (parse (stringTok "abc") "bc  ")
True
-}
stringTok ::
  Chars ->
  Parser Chars
stringTok s = string s <* spaces

{- | Write a function that tries the given parser, otherwise succeeds by producing the given value.

>>> parse (option 'x' character) "abc"
Result >bc< 'a'

>>> parse (option 'x' character) ""
Result >< 'x'
-}
option ::
  a ->
  Parser a ->
  Parser a
option a p = p ||| pure a

{- | Write a parser that parses 1 or more digits.

>>> parse digits1 "123"
Result >< "123"

>>> isErrorResult (parse digits1 "abc123")
True
-}
digits1 ::
  Parser Chars
digits1 = list1 digit

{- | Write a function that parses one of the characters in the given string.

>>> parse (oneof "abc") "bcdef"
Result >cdef< 'b'

>>> isErrorResult (parse (oneof "abc") "def")
True
-}
oneof ::
  Chars ->
  Parser Char
oneof elems = satisfy (`elem` elems)

{- | Write a function that parses any character, but fails if it is in the given string.


>>> parse (noneof "bcd") "abc"
Result >bc< 'a'

>>> isErrorResult (parse (noneof "abcd") "abc")
True
-}
noneof ::
  Chars ->
  Parser Char
noneof elems = satisfy (not . (`elem` elems))

{- | Write a function that applies the first parser, runs the third parser keeping the result,
then runs the second parser and produces the obtained result.


>>> parse (between (is '[') (is ']') character) "[a]"
Result >< 'a'

>>> isErrorResult (parse (between (is '[') (is ']') character) "[abc]")
True

>>> isErrorResult (parse (between (is '[') (is ']') character) "[abc")
True

>>> isErrorResult (parse (between (is '[') (is ']') character) "abc]")
True
-}
between ::
  Parser o ->
  Parser c ->
  Parser a ->
  Parser a
between l r c = l *> c <* r

{- | Write a function that applies the given parser in between the two given characters.


>>> parse (betweenCharTok '[' ']' character) "[a]"
Result >< 'a'

>>> isErrorResult (parse (betweenCharTok '[' ']' character) "[abc]")
True

>>> isErrorResult (parse (betweenCharTok '[' ']' character) "[abc")
True

>>> isErrorResult (parse (betweenCharTok '[' ']' character) "abc]")
True
-}
betweenCharTok ::
  Char ->
  Char ->
  Parser a ->
  Parser a
betweenCharTok l r = between (is l) (is r)

{- | Write a function that parses 4 hex digits and return the character value.

/Tip:/ Use `readHex`, `isHexDigit`, `replicateA`, `satisfy`, `chr` and the monad instance.

>>> parse hex "0010"
Result >< '\DLE'

>>> parse hex "0a1f"
Result >< '\2591'

>>> isErrorResult (parse hex "001")
True

>>> isErrorResult (parse hex "0axf")
True
-}
hex ::
  Parser Char
hex = do
  x <- thisMany 4 (satisfy isHexDigit)
  let hex = readHex x
  case hex of
    Full num -> pure $ chr num
    Empty -> unexpectedCharParser '?'

{- | Write a function that parses the character 'u' followed by 4 hex digits and return the character value.


>>> parse hexu "u0010"
Result >< '\DLE'

>>> parse hexu "u0a1f"
Result >< '\2591'

>>> isErrorResult (parse hexu "0010")
True

>>> isErrorResult (parse hexu "u001")
True

>>> isErrorResult (parse hexu "u0axf")
True
-}
hexu ::
  Parser Char
hexu = is 'u' *> hex

{- | Write a function that produces a non-empty list of values coming off the given parser (which must succeed at least once),
separated by the second given parser.

>>> parse (sepby1 character (is ',')) "a"
Result >< "a"

>>> parse (sepby1 character (is ',')) "a,b,c"
Result >< "abc"

>>> parse (sepby1 character (is ',')) "a,b,c,,def"
Result >def< "abc,"

>>> isErrorResult (parse (sepby1 character (is ',')) "")
True
-}
sepby1 ::
  Parser a ->
  Parser s ->
  Parser (List a)
sepby1 a s = a .:. list (s *> a)

{- | Write a function that produces a list of values coming off the given parser,
separated by the second given parser.


>>> parse (sepby character (is ',')) ""
Result >< ""

>>> parse (sepby character (is ',')) "a"
Result >< "a"

>>> parse (sepby character (is ',')) "a,b,c"
Result >< "abc"

>>> parse (sepby character (is ',')) "a,b,c,,def"
Result >def< "abc,"
-}
sepby ::
  Parser a ->
  Parser s ->
  Parser (List a)
sepby a s = sepby1 a s ||| pure Nil

{- | Write a parser that asserts that there is no remaining input.

>>> parse eof ""
Result >< ()

>>> isErrorResult (parse eof "abc")
True
-}
eof ::
  Parser ()
eof = P parseFun
 where
  parseFun Nil = Result Nil ()
  parseFun _ = UnexpectedEof

{- | Write a parser that produces a character that satisfies all of the given predicates.


>>> parse (satisfyAll (isUpper :. (/= 'X') :. Nil)) "ABC"
Result >BC< 'A'

>>> parse (satisfyAll (isUpper :. (/= 'X') :. Nil)) "ABc"
Result >Bc< 'A'

>>> isErrorResult (parse (satisfyAll (isUpper :. (/= 'X') :. Nil)) "XBc")
True

>>> isErrorResult (parse (satisfyAll (isUpper :. (/= 'X') :. Nil)) "")
True

>>> isErrorResult (parse (satisfyAll (isUpper :. (/= 'X') :. Nil)) "abc")
True
-}
satisfyAll ::
  List (Char -> Bool) ->
  Parser Char
satisfyAll p = satisfy (and . sequence p)

-- satisfyAll p = satisfy (\x -> and (p <*> pure x))
-- satisfyAll p = satisfy (\x -> let r = ($) <$> p <*> pure x in and r)

{- | Write a parser that produces a character that satisfies any of the given predicates.

>>> parse (satisfyAny (isLower :. (/= 'X') :. Nil)) "abc"
Result >bc< 'a'

>>> parse (satisfyAny (isLower :. (/= 'X') :. Nil)) "ABc"
Result >Bc< 'A'

>>> isErrorResult (parse (satisfyAny (isLower :. (/= 'X') :. Nil)) "XBc")
True

>>> isErrorResult (parse (satisfyAny (isLower :. (/= 'X') :. Nil)) "")
True
-}
satisfyAny ::
  List (Char -> Bool) ->
  Parser Char
satisfyAny p = satisfy (or . sequence p)

{- | Write a parser that parses between the two given characters, separated by a comma character ','.


>>> parse (betweenSepbyComma '[' ']' lower) "[a]"
Result >< "a"

>>> parse (betweenSepbyComma '[' ']' lower) "[]"
Result >< ""

>>> parse (betweenSepbyComma '[' ']' lower) "[a,b,c]"
Result >< "abc"

>>> parse (betweenSepbyComma '[' ']' lower) "[a,  b, c]"
Result >< "abc"

>>> parse (betweenSepbyComma '[' ']' digits1) "[123,456]"
Result >< ["123","456"]

>>> isErrorResult (parse (betweenSepbyComma '[' ']' lower) "[A]")
True

>>> isErrorResult (parse (betweenSepbyComma '[' ']' lower) "[abc]")
True

>>> isErrorResult (parse (betweenSepbyComma '[' ']' lower) "[a")
True

>>> isErrorResult (parse (betweenSepbyComma '[' ']' lower) "a]")
True
-}
betweenSepbyComma ::
  Char ->
  Char ->
  Parser a ->
  Parser (List a)
betweenSepbyComma l r a = between (is l) (is r) $ sepby a (is ',' .:. spaces)
