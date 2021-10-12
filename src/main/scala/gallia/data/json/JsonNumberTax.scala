package gallia
package data.json

import meta._

// ===========================================================================
object JsonNumberTax extends gallia.atoms.utils.ModifyObj { // because of 201119115427

  // ---------------------------------------------------------------------------
  def payUp(c: Cls)(o: Obj): Obj = super.modify(c)(o)

  // ===========================================================================
  override protected def qualifyingFields(c: Cls): Seq[Fld] =
    c
      .fields
      .filter { field =>
        field.isInt ||
        field.nestedClassOpt.exists {
          qualifyingFields(_).nonEmpty } }

  // ===========================================================================
  override protected def transformation(qualifyingField: Fld): Any => Any =
    (qualifyingField.isMultiple, qualifyingField.nestedClassOpt) match {
      case (false, None)     => _.asInstanceOf[    Double ]      .toInt
      case (true , None)     => _.asInstanceOf[Seq[Double]].map(_.toInt)
      case (false, Some(c2)) => _.asInstanceOf[    Obj    ].pipe(payUp(c2))
      case (true , Some(c2)) => _.asInstanceOf[Seq[Obj   ]].map(payUp(c2)) }

}

// ===========================================================================
