package gallia
package inferring

import meta.basic.BasicType._
import meta.{Cls => _, Fld => _, _}

// ===========================================================================
object SchemaInferrer { // mostly for JSON for now...
  type AnySingleValue = Any
  import SchemaInferrerUtils._

  // ===========================================================================
  def klass(values: Seq[Obj]): Cls =
    values
      .iterator
      .map(klass(_))
      .reduceLeft(_ combine _)

  // ---------------------------------------------------------------------------
  def klass(o: Obj): Cls =
    o .entries
      .map { case (key, value) =>
        Fld(key, info(value)) }
      .pipe(Cls.apply)

  // ===========================================================================
  def info(value: AnyValue): Info =
      value match {
        case seq: Seq[_] =>
          // a201113123227 - no heterogenous arrays
          Info.nes(//FIXME: pes/nes issue?
            (seq.head match {
              case _: Obj => seq.map(_.asInstanceOf[Obj]).pipe(klass)
              case leaf   => valueType(leaf) }))
        case _ =>
          Info.one(valueType(value)) }

    // ---------------------------------------------------------------------------
    @PartialTypeMatching
    @NumberAbstraction
    def valueType(value: AnySingleValue): ValueType =
      value match {
        case x: Obj => klass(x) 
        
        case x: String  => _String
        case x: Double  => // 201119115427
          if (x.isValidInt /* Long? */) _Int // _Long?
          else                          _Double
        case x: Int     => _Int
        case x: Boolean => _Boolean

        case x: Byte    => _Byte
        case x: Short   => _Short
        case x: Long    => _Long
        
        case x: Float   => // 201119115427
          if (x.isValidInt /* Long? */) _Int
          else                          _Float
        
        case x: BigInt        => _BigInt    
        case x: BigDec        => _BigDec

        case x: LocalDate      => _LocalDate
        case x: LocalTime      => _LocalTime
        case x: LocalDateTime  => _LocalDateTime
        case x: OffsetDateTime => _OffsetDateTime
        case x: ZonedDateTime  => _ZonedDateTime
        case x: Instant        => _Instant
        
        case x: ByteBuffer     => _Binary

        case x: EnumValue      => _Enm(Seq(x))
        case x: EnumEntry      => _Enm(Seq(EnumValue(x.entryName))) }

}

// ===========================================================================
