package aptus.utils

// ===========================================================================
object CharUtils {

  lazy val Whitespaces                                 : Set[Char] = Set(' ', '\t', '\r', '\n') // TODO: see Character.isWhitespace
  lazy val NumberLikeChars                             : Set[Char] = Set('-', '.', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9') // TODO: add e/E?

  lazy val LowerAlphaSet                               : Set[Char] = ('a' to 'z').toSet
  lazy val UpperAlphaSet                               : Set[Char] = ('A' to 'Z').toSet
  lazy val DigitSet                                    : Set[Char] = ('0' to '9').toSet

  lazy val AlphaSet                                    : Set[Char] = (LowerAlphaSet ++ UpperAlphaSet            ).toSet
  lazy val NumericalSet                                : Set[Char] =                                    DigitSet .toSet
  lazy val AlphaNumericalSet                           : Set[Char] = (LowerAlphaSet ++ UpperAlphaSet ++ DigitSet).toSet

  lazy val AlphaNumericalWithUnderscoreSet             : Set[Char] = AlphaNumericalSet + '_'
  lazy val AlphaNumericalWithDashSet                   : Set[Char] = AlphaNumericalSet + '-'
  lazy val AlphaNumericalWithDotSet                    : Set[Char] = AlphaNumericalSet + '.'

  lazy val AlphaNumericalWithDashAndDotSet             : Set[Char] = AlphaNumericalSet + '-' + '.'
  lazy val AlphaNumericalWithDashAndUnderscoreSet      : Set[Char] = AlphaNumericalSet + '-' + '_'
  lazy val AlphaNumericalWithDotAndUnderscoreSet       : Set[Char] = AlphaNumericalSet + '.' + '_'

  lazy val AlphaNumericalWithDashAndDotAndUnderscoreSet: Set[Char] = AlphaNumericalSet + '-' + '.' + '_'
}

// ===========================================================================
