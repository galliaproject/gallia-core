package gallia
package io

import aptus.{Anything_, String_}
import meta.{Info, Ofni}
import inferring.table.TypeGuessing
import reflect.{BasicType, Container, UnparameterizedBasicType}

// ===========================================================================
case class CellConf(
    nullValues: Seq[String] = Seq(DefaultNullValue),

    /** order matters, acts dumb if more than one such separator within the same value */
    arraySeparators: Seq[String] = Seq(DefaultArraySeparator)) {

  private val noNulls : Boolean = nullValues     .isEmpty
  private val noArrays: Boolean = arraySeparators.isEmpty

  // ---------------------------------------------------------------------------
  private val nullValueSet      : Set[String]    = nullValues.toSet
  private val soleArraySeparator: Option[String] = if (arraySeparators.size == 1) Some(arraySeparators.head) else None

  // ===========================================================================
  def inferContainerOnly(value: String): Container =
           if (isNull (value)) Container._Opt
      else if (isArray(value)) Container._Nes
      else                     Container._One

  // ---------------------------------------------------------------------------
  def inferOfni(value: String): Ofni =
           if (isNull (value)) Ofni(_Optional, Info(_Single,   BasicType._String))
      else if (isArray(value)) Ofni(_Required, Info(_Multiple, arrayType(splitArray(value))))
      else                     Ofni(_Required, Info(_Single,   TypeGuessing(value)))

    // ---------------------------------------------------------------------------
    private def arrayType(values: Seq[String]): BasicType =
      values
        .map(TypeGuessing.apply)
        .distinct
        .pipe(reflect.BasicTypeUtils.combine)

  // ===========================================================================
  def transformValue(multiple: Boolean)(value: String): Either[Option[String], Seq[String]] =
    if (!multiple) Left (value.in.noneIf(isNull))
    else           Right(value.in.noneIf(isNull).toSeq.flatMap(splitArray))

  // ---------------------------------------------------------------------------
  def valueSet(value: String): Set[String] =
    transformValue(isArray(value))(value).fold(
        _.toSeq,
        identity)
      .toSet

  // ===========================================================================
  def transformBasicValue(tipe: BasicType)(value: String): Any =
    tipe match {
      case _: BasicType._Enm           => BasicType._Enm.parseString(value)
      case    BasicType._Boolean       => inferring.table.BooleanDetector.forceBoolean(value)
      case x: UnparameterizedBasicType => x.parseString(value) }

  // ===========================================================================
  private def isNull (value: String): Boolean = !noNulls  && nullValueSet.contains(value) /* no trimming intentionally */
  private def isArray(value: String): Boolean = !noArrays && arraySeparators.exists(value.contains)

  // ---------------------------------------------------------------------------
  private def splitArray(value: String): Seq[String] =
    soleArraySeparator match {
      case Some(sep) => value.splitBy(sep)
      case None =>
        arraySeparators.foldLeft(Seq(value)) { (curr, sep) =>
          curr.flatMap(_.splitBy(sep)) } /* hopefully only one sep per value */ }
}

// ===========================================================================
object CellConf {
  val Default = CellConf()
}

// ===========================================================================
