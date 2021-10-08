package gallia.inferring.table

import gallia.reflect.BasicType

// ===========================================================================
object TypeGuessing {

  private val NumberLikeChars = Set('-', '.', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9')

  // ---------------------------------------------------------------------------
  def apply(value: String): BasicType =
    if (value.isEmpty) BasicType._String // can only happen if explicitly exclude empty string from null values; TODO: t201229113846 - a temporary "undetermined" type?
      // cheap optimization... TODO: benchmark
      else if (!NumberLikeChars.contains(value.head)) BasicType._String // e-notation should work fine since the letter can't appear at the beginning or the end
      else if (value._isValidInt)    BasicType._Int
      else if (value._isValidDouble) BasicType._Double
      else                           BasicType._String

    // ---------------------------------------------------------------------------
    implicit class _tmp(u: String) { // TODO: very inefficient
      def _isValidInt   : Boolean = util.Try(u.toInt   ).isSuccess
      def _isValidDouble: Boolean = util.Try(u.toDouble).isSuccess
    }

}

// ===========================================================================
