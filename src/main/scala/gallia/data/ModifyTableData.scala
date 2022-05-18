package gallia
package data

import meta.Fld

// ===========================================================================
class ModifyTableData(conf: io.CellConf) extends atoms.utils.ModifyObj { // 201231113658

  // ---------------------------------------------------------------------------
  def modify(x: AObjs): Objs = x.z.map(super.modify(x.c))

    // ===========================================================================
    override protected def qualifyingFields(c: Cls): Seq[Fld] = c.fields.filterNot(_.isOneString)

      // ===========================================================================
      override protected def transformation(qualifyingField: Fld): Any => Any =
        any => conf.transformValue(qualifyingField.subInfo1.isMultiple)(any.asInstanceOf[String]).fold(
          _.map /* option's */(conf.transformBasicValue(qualifyingField.forceBasicType)),
          _.map /* seq   's */(conf.transformBasicValue(qualifyingField.forceBasicType)) )

}

// ===========================================================================
