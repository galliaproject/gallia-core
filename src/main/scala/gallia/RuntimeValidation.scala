package gallia

import aptus.Anything_
import aptus.Pes

// ===========================================================================
object RuntimeValidation { import meta._ // 210115153346 - POC  
  type DistinctSeq[A] = Seq[A]

  // ---------------------------------------------------------------------------
  case class ValErr(
      code: Int,
      key: Key,
      index: Option[Int],
      details: Any) {
    def format = s"VALIDATION ERROR for ${key}: ${details}" }

  // ===========================================================================
  // TODO: t220517132701 - support union types
  def validate(c: Cls)(o: Obj): Pes[ValErr] = rec(c)(o).in.noneIf(_.isEmpty)

    // ---------------------------------------------------------------------------
    def rec(c: Cls)(o: Obj): Seq[ValErr] = {
      val keySet = o.keySet

      // ---------------------------------------------------------------------------
      val unknownFieldErrors: Seq[ValErr] =
        keySet
          .diff(c.keySet)
          .pipe { unknownKeys =>
            if (unknownKeys.isEmpty) Nil
            else
              o .entries
                .filter(x => unknownKeys.contains(x._1))
                .map { case (key, value) =>
                  ValErr(10, key, None, s"unknown field: $key, ${tipe(value) /* TODO: format */}") } }

      // ---------------------------------------------------------------------------
      val missingRequiredFieldErrors: Seq[ValErr] =
        c.requiredKeySet.diff(keySet)
          .pipe { missingKeySet =>
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
      unknownFieldErrors         ++
      missingRequiredFieldErrors ++
      otherFieldErrors
    }

  // ===========================================================================
  @NumberAbstraction
  def validateField(field: Fld, key: Key, value: AnyValue): Seq[ValErr] = {

    val (isMultiple: Boolean, types: DistinctSeq[Option[Either[Unit, BasicType]]]) =
      value match {
        case seq: Seq[_] => true  -> seq.map(tipe).distinct
        case sgl         => false -> Seq(    tipe(sgl)) }

    // ---------------------------------------------------------------------------
    val multiplicityErrors: Option[ValErr] = {
           if ( field.subInfo1.isMultiple && !isMultiple) Some(ValErr(12, key, None, s"should be mult"))
      else if (!field.subInfo1.isMultiple &&  isMultiple) Some(ValErr(13, key, None, s"shouldn't be mult"))
      else                                             None }

    // ---------------------------------------------------------------------------
    val typeErrors: Iterable[ValErr] =
      types match {

        // ---------------------------------------------------------------------------
        case Nil => ??? // can't happen: see a201104150254

        // ---------------------------------------------------------------------------
        case Seq(Some(Left(() /* = object(s) */))) =>
          field.subInfo1.valueType match {

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
                    .in.noneIf(_.isEmpty)
                    .map(nestedErrors =>
                      ValErr(15, key, Some(index), (s"nested errors", nestedErrors))) }
                .in.noneIf(_.isEmpty)
                .map{ nestedErrorss => ValErr(16, key, None, (s"nested errors", nestedErrorss)) }
          }

        // ---------------------------------------------------------------------------
        case Seq(Some(Right(singleType))) =>
          field.subInfo1.valueType match {
            case x: BasicType if x == singleType => None
            case e: _Enm =>
              if (singleType.isEnm)
                if (e.values.contains(value)) None
                else                          Some(ValErr(23, key, None, s"invalid enum value: ${value}"))
              else                            Some(ValErr(21, key, None, s"is $singleType but should be ${e}"))
            case l: BasicType =>
              if (singleType.isEnm) Some(ValErr(22, key, None, s"is _Enm but should be ${l}"))
              else                  Some(ValErr(17, key, None, s"is $singleType but should be ${l}"))
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
  trait HasKey { def key: Key }

  // ---------------------------------------------------------------------------
  trait HasField extends HasKey {
    val field: Fld
    final val key = field.key /* t231003141803 - scala 2 had late init issues, ok now? */ }

  // ---------------------------------------------------------------------------
  case class UnknownField(key: Key, tipe: BasicType) extends HasKey

  case class MissingRequiredField(field: Fld, keysPresent: Seq[Key]) extends HasField

  case class NonMultipleField(key: Key) extends HasKey
  case class NonSingleField  (key: Key) extends HasKey

  case class    NonBasicType(key: Key) extends HasKey
  case class    NonObjType  (key: Key) extends HasKey

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

      case x: EnumValue => Some(Right(_Enm.Dummy)) //TODO: make sure works as expected

      case x: Obj => Some(Left(()))

      //TODO: errors out on Any? Unit?
      case _ =>
        value match {
          case x: Byte  => Some(Right(_Byte))
          case x: Short => Some(Right(_Short))
          case x: Long  => Some(Right(_Long))

          case x: Float => Some(Right(_Float))

          case x: BigInt        => Some(Right(_BigInt))
          case x: BigDec        => Some(Right(_BigDec))

          case x: LocalDate      => Some(Right(_LocalDate))
          case x: LocalTime      => Some(Right(_LocalTime))
          case x: LocalDateTime  => Some(Right(_LocalDateTime))
          case x: OffsetDateTime => Some(Right(_OffsetDateTime))
          case x: ZonedDateTime  => Some(Right(_ZonedDateTime))
          case x: Instant        => Some(Right(_Instant))

          case x: ByteBuffer     => Some(Right(_Binary))

          case _ => None
        }
    }

}

// ===========================================================================
