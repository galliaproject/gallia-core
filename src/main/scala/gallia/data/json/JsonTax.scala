package gallia
package data.json

import meta._
import aptus._

// ===========================================================================
object JsonTax {

  def payAllUp(c: Cls) =    
            (JsonNumberTax.payUp(c) _)
    //TODO: t220315141543 - optim: skip if schema doesn't contain bignums/times            
    .andThen(BigIntJsonTax.payUp(c) _)
    .andThen(BigDecJsonTax.payUp(c) _)
    .andThen(LocalDataJsonTax.payUp(c) _)  
}

// ===========================================================================
trait JsonTax extends atoms.utils.ModifyObj {
  type T
  
  // ---------------------------------------------------------------------------
  def qualifies(field: Fld): Boolean
  def transformValue(qualityingField: Fld)(value: T): Any

  // ---------------------------------------------------------------------------
  final def payUp(c: Cls)(o: Obj): Obj = super.modify(c)(o)

  // ===========================================================================
  @NumberAbstraction
  final override protected def qualifyingFields(c: Cls): Seq[Fld] =
    c
      .fields
      .filter { field =>
        !field.isString && !field.isBoolean && // optimization
        ( qualifies(field) || 
          field.nestedClassOpt.exists { qualifyingFields(_).nonEmpty }) }

  // ===========================================================================
  final override protected def transformation(qualifyingField: Fld): Any => Any =
    (qualifyingField.isMultiple, qualifyingField.nestedClassOpt) match {
      case (false, None)     => _.asInstanceOf[    T ]     .pipe(transformValue(qualifyingField))
      case (true , None)     => _.asInstanceOf[Seq[T]]     .map (transformValue(qualifyingField))
      case (false, Some(c2)) => _.asInstanceOf[    Obj    ].pipe(payUp(c2))
      case (true , Some(c2)) => _.asInstanceOf[Seq[Obj   ]].map (payUp(c2)) }

}

// ===========================================================================
object JsonNumberTax extends JsonTax { type T = Double // because of 201119115427; this is a high price to pay to support integers...  
  def qualifies(field: Fld): Boolean = field.isIntegerLikeType
  def transformValue(qualitifyingField: Fld)(value: T): Any  = qualitifyingField.forceIntegerLikeType.toIntegerLike(value) }

// ---------------------------------------------------------------------------
object BigIntJsonTax extends JsonTax { type T = String
    def qualifies(field: Fld): Boolean = field.isBigInt
    def transformValue(ignoredQualitifyingField: Fld)(value: T): Any  = BigInt.apply(value) }
  
  // ---------------------------------------------------------------------------
  object BigDecJsonTax extends JsonTax { type T = String
    def qualifies(field: Fld): Boolean = field.isBigDec
    def transformValue(ignoredQualitifyingField: Fld)(value: T): Any  = BigDecimal.apply(value) }

// ---------------------------------------------------------------------------
object LocalDataJsonTax extends JsonTax { type T = String
    def qualifies(field: Fld): Boolean = field.isDate 
    def transformValue(ignoredQualitifyingField: Fld)(value: T): Any  = value.parseLocalDate }

// ===========================================================================
