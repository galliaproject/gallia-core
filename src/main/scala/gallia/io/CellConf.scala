package gallia.io

import aptus.{Anything_, String_}
import gallia.reflect.Container
import gallia.reflect.BasicType
import gallia.meta.Info
import gallia.inferring.table.TypeGuessing

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
  def inferInfo(value: String): Info =
           if (isNull (value)) Info(Container._Opt, BasicType._String)
      else if (isArray(value)) Info(Container._Nes, arrayType(splitArray(value)))
      else                     Info(Container._One, TypeGuessing(value))

    // ---------------------------------------------------------------------------
    private def arrayType(values: Seq[String]): BasicType =
      values
        .map(TypeGuessing.apply)
        .distinct
        .thn(BasicType.combine)

  // ===========================================================================
  def transformValue(multiple: Boolean)(value: String): Either[Option[String], Seq[String]] =
    if (!multiple) Left (value.as.noneIf(isNull))
    else           Right(value.as.noneIf(isNull).toSeq.flatMap(splitArray))

  // ---------------------------------------------------------------------------
  def valueSet(value: String): Set[String] =
    transformValue(isArray(value))(value).fold(
        _.toSeq,
        identity)
      .toSet

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
