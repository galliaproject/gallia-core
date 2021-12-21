package gallia
package data.json

import meta._

// ===========================================================================
object JsonNumberTax extends atoms.utils.ModifyObj { // because of 201119115427; this is a high price to pay to support integers...

  // ---------------------------------------------------------------------------
  def payUp(c: Cls)(o: Obj): Obj = super.modify(c)(o)

  // ===========================================================================
  @NumberAbstraction
  override protected def qualifyingFields(c: Cls): Seq[Fld] =
    c
      .fields
      .filter { field =>
        !field.isStringDoubleBoolean && // most will stop here (optimization)
        ( field.isIntegerLikeType || 
          field.nestedClassOpt.exists { qualifyingFields(_).nonEmpty }) }

  // ===========================================================================
  override protected def transformation(qualifyingField: Fld): Any => Any =
    (qualifyingField.isMultiple, qualifyingField.nestedClassOpt) match {
      case (false, None)     => _.asInstanceOf[    Double ].pipe(qualifyingField.forceIntegerLikeType.toIntegerLike)
      case (true , None)     => _.asInstanceOf[Seq[Double]].map (qualifyingField.forceIntegerLikeType.toIntegerLike)
      case (false, Some(c2)) => _.asInstanceOf[    Obj    ].pipe(payUp(c2))
      case (true , Some(c2)) => _.asInstanceOf[Seq[Obj   ]].map (payUp(c2)) }

}

// ===========================================================================
