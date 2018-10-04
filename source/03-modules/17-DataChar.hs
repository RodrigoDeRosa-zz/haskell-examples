-- This module has a set of functions for character manipulation. They are useful
-- for strings as they are just arrays of chars
import Data.Char

-- isControl checks whether a character is a control character.

-- isSpace checks whether a character is a white-space characters.
-- That includes spaces, tab characters, newlines, etc.

-- isLower checks whether a character is lower-cased.
-- isUpper checks whether a character is upper-cased.
-- isAlpha checks whether a character is a letter.
-- isAlphaNum checks whether a character is a letter or a number.

-- isPrint checks whether a character is printable. Control characters,
-- for instance, are not printable.

-- isDigit checks whether a character is a digit.
-- isOctDigit checks whether a character is an octal digit.
-- isHexDigit checks whether a character is a hex digit.
-- isLetter checks whether a character is a letter.

-- isMark checks for Unicode mark characters.
-- Those are characters that combine with preceding letters to form
-- latters with accents. Use this if you are French.

-- isNumber checks whether a character is numeric.
-- isPunctuation checks whether a character is punctuation.
-- isSymbol checks whether a character is a fancy mathematical or currency symbol.
-- isSeparator checks for Unicode spaces and separators.

-- isAscii checks whether a character falls into the first 128 characters of
-- the Unicode character set.

-- isLatin1 checks whether a character falls into the first 256 characters of Unicode.
-- isAsciiUpper checks whether a character is ASCII and upper-case.
-- isAsciiLower checks whether a character is ASCII and lower-case

exampleAlphaNum = all isAlphaNum "bobby283"
-- => True
exampleAlphaNum' = all isAlphaNum "eddy the fish!"
-- => False (the !)

-- GeneralCategory is an enum that categorizes chars.
exampleGenCat = map generalCategory " \t\nA9?|"
-- => [Space,Control,Control,UppercaseLetter,DecimalNumber,OtherPunctuation,MathSymbol]

-- toUpper converts a character to upper-case. Spaces, numbers, and the like
-- remain unchanged.

-- toLower converts a character to lower-case.

-- toTitle converts a character to title-case. For most characters, title-case
-- is the same as upper-case.

-- digitToInt converts a character to an Int. To succeed, the character must be
-- in the ranges '0'..'9', 'a'..'f' or 'A'..'F'.

-- intToDigit is the inverse function of digitToInt. It takes an Int in the
-- range of 0..15 and converts it to a lower-case character.

-- The ord and chr functions convert characters to their corresponding numbers
-- and vice versa:

-- Caesar cipher. Shifting chars in a String by a fixed value
encode :: Int -> String -> String
encode shift msg =
    let ords = map ord msg
        shifted = map (+ shift) ords
    in  map chr shifted
-- Example
exampleEncode = encode 5 "Heeeey"
-- => "Mjjjj~"

-- Decoding
decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg
-- Example
exampleDecode = decode 5 "Mjjjj~"
-- => "Heeeey"
