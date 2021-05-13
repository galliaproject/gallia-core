package gallia.inferring

import aptus.Anything_

import gallia._
import gallia.reflect.Container._
import gallia.reflect.BasicType._
import gallia.meta.{Cls => _, Fld => _, _}

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
      .thn(Cls.apply)

  // ===========================================================================
  def info(value: AnyValue): Info =
      (Info.apply _)
        .tupled(
          value match {
            case seq: Seq[_] =>
              // a201113123227 - no heterogenous arrays
              _Nes -> //FIXME: pes/nes issue?
                (seq.head match {
                  case _: Obj => seq.map(_.asInstanceOf[Obj]).thn(klass)
                  case leaf   => containee(leaf) })
            case sgl => _One -> containee(value)
          })

    // ---------------------------------------------------------------------------
    @PartialTypeMatching
    @NumberAbstraction
    def containee(value: AnySingleValue): Containee =
      value match {
        case x: Obj => klass(x) 
        case x: String  => _String
        case x: Double  => // 201119115427
          if (x.isValidInt /* Long? */) _Int // _Long?
          else                          _Double
        case x: Int     => _Int
        case x: Boolean => _Boolean

        case x: enumeratum.EnumEntry => _Enum

        case x: Byte    => _Byte
        case x: Short   => _Short
        case x: Long    => _Long
        
        case x: Float   => // 201119115427
          if (x.isValidInt /* Long? */) _Int
          else                          _Float
        
        case x: BigInt        => _BigInt    
        case x: BigDecimal    => _BigDecimal
        
        case x: LocalDate     => _LocalDate        
        case x: LocalDateTime => _LocalDateTime }

}

// ===========================================================================
