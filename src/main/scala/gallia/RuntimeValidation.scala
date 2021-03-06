package gallia

import aptus.Anything_
import aptus.Pes

import gallia.reflect.BasicType

// ===========================================================================
object RuntimeValidation { // 210115153346 - POC
  import gallia.meta._
  type DistinctSeq[A] = Seq[A]

  // ---------------------------------------------------------------------------
  case class ValErr(
      code: Int,
      key: Key,
      index: Option[Int],
      details: Any) {
    def format = s"VALIDATION ERROR for ${key}: ${details}"
  }

  // ===========================================================================
  def validate(c: Cls)(o: Obj): Pes[ValErr] = rec(c)(o).as.noneIf(_.isEmpty)

    // ---------------------------------------------------------------------------
    def rec(c: Cls)(o: Obj): Seq[ValErr] = {
      val keySet = o.keySet

      // ---------------------------------------------------------------------------
      val unknownFieldErrors: Seq[ValErr] =
        keySet
          .diff(c.keySet)
          .thn { unknownKeys =>
            if (unknownKeys.isEmpty) Nil
            else
              o .entries
                .filter(x => unknownKeys.contains(x._1))
                .map { case (key, value) =>
                  ValErr(10, key, None, s"unknown field: $key, ${tipe(value) /* TODO: format */}") } }

      // ---------------------------------------------------------------------------
      val missingRequiredFieldErrors: Seq[ValErr] =
        c.requiredKeySet.diff(keySet)
          .thn { missingKeySet =>
            if (missingKeySet.isEmpty) Nil
            else
              c ._filterByKey(missingKeySet.contains)
                .map { field => ValErr(11, field.key, None, s"missing required field: ${field}") } }

      // ---------------------------------------------------------------------------
      val otherFieldErrors: Seq[ValErr] =
        o .entries
          .flatMap { case (key, value) =>
            c .lookup
              .get(key) // ignore unknown fields
              .toSeq
              .flatMap {
                  validateField(_, key, value) } }

      // ---------------------------------------------------------------------------
      unknownFieldErrors ++
      missingRequiredFieldErrors ++
      otherFieldErrors
    }

  // ===========================================================================
  @gallia.NumberAbstraction
  def validateField(field: Fld, key: Key, value: AnyValue): Seq[ValErr] = {

    val (isMultiple: Boolean, types: DistinctSeq[Option[Either[Unit, BasicType]]]) =
      value match {
        case seq: Seq[_] => true  -> seq.map(tipe).distinct
        case sgl         => false -> Seq(    tipe(sgl))
      }

    // ---------------------------------------------------------------------------
    val multiplicityErrors: Option[ValErr] =
           if (field.info.   isMultiple && !isMultiple) Some(ValErr(12, key, None, s"should be mult"))
      else if (field.info.isNotMultiple &&  isMultiple) Some(ValErr(13, key, None, s"shouldn't be mult"))
      else                                              None

    // ---------------------------------------------------------------------------
    val typeErrors: Iterable[ValErr] =
      types match {

        // ---------------------------------------------------------------------------
        case Nil => ??? // can't happen: see a201104150254

        // ---------------------------------------------------------------------------
        case Seq(Some(Left(() /* = object(s) */))) =>
          field.info.containee match {

            case basicTipe: BasicType => Some(ValErr(14, key, None, s"is obj but should be ${basicTipe}"))

            // ---------------------------------------------------------------------------
            case subClass: Cls =>
              (value match {
                  case seq: Seq[_] => seq
                  case sgl         => Seq(sgl) })
                .map(_.asInstanceOf[Obj])
                .map(rec(subClass))
                .zipWithIndex
                .flatMap { case (nestedErrorsOpt, index) =>
                  nestedErrorsOpt
                    .as.noneIf(_.isEmpty)
                    .map(nestedErrors =>
                      ValErr(15, key, Some(index), (s"nested errors", nestedErrors))) }
                .as.noneIf(_.isEmpty)
                .map{ nestedErrorss => ValErr(16, key, None, (s"nested errors", nestedErrorss)) }
          }

        // ---------------------------------------------------------------------------
        case Seq(Some(Right(singleType))) =>
          field.info.containee match {
            case x: BasicType if x == singleType => None
            case l: BasicType                    => Some(ValErr(17, key, None, s"is $singleType but should be ${l}"))
            case _: Cls                          => Some(ValErr(18, key, None, s"is $singleType but should be an obj")) }

        // ---------------------------------------------------------------------------
        case Seq(None) => Some(ValErr(19, key, None, s"unrecognized type"))

        // ---------------------------------------------------------------------------
        // TODO: t201017102332 - allow int/double mix?
        case multiple => Some(ValErr(20, key, None, s"has an hetereogenous type: ${multiple}")) // TODO: format...
      }

    // ---------------------------------------------------------------------------
    Nil ++
      multiplicityErrors ++
      typeErrors
  }

  // ===========================================================================
  trait HasKey { val key: Key }

  // ---------------------------------------------------------------------------
  trait HasField extends HasKey {
    val field: Fld
    final lazy val key = field.key // TODO: late init
  }

  // ---------------------------------------------------------------------------
  case class UnknownField(key: Key, tipe: BasicType) extends HasKey

  case class MissingRequiredField(field: Fld, keysPresent: Seq[Key]) extends HasField

  case class NonMultipleField(key: Key) extends HasKey
  case class NonSingleField(key: Key) extends HasKey

  case class    NonBasicType(key: Key) extends HasKey
  case class    NonObjType(key: Key) extends HasKey

  // ===========================================================================
  import BasicType._

  // ---------------------------------------------------------------------------
  @TypeMatching
  def tipe(value: AnyValue): Option[Either[Unit, BasicType]] =
    value match {
      case x: String  => Some(Right(_String))
      case x: Int     => Some(Right(_Int))
      case x: Double  => Some(Right(_Double))
      case x: Boolean => Some(Right(_Boolean))

      case x: enumeratum.EnumEntry => Some(Right(_Enum)) //TODO: make sure works as expected
      //TODO: scala enum

      case x: Obj => Some(Left(()))

      //TODO: errors out on Any? Unit?
      case _ =>
        value match {
          case x: Byte  => Some(Right(_Byte))
          case x: Short => Some(Right(_Short))
          case x: Long  => Some(Right(_Long))

          case x: Float => Some(Right(_Float))

          case x: BigInt        => Some(Right(_BigInt))
          case x: BigDecimal    => Some(Right(_BigDecimal))

          case x: LocalDate     => Some(Right(_LocalDate))
          case x: LocalDateTime => Some(Right(_LocalDateTime))

          case _ => None
        }
    }

}

// ===========================================================================
