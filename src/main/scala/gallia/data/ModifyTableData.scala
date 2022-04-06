package gallia
package data

import meta.Fld
import reflect.BasicType
import io.CellConf

// ===========================================================================
// TODO: t220406110532: proper TableTax counterpart to JSON's (see 220406110635)
class ModifyTableData(conf: CellConf) extends atoms.utils.ModifyObj { // 201231113658

  // ---------------------------------------------------------------------------
  def modify(x: AObjs): Objs = x.z.map(super.modify(x.c))

    // ===========================================================================
    override protected def qualifyingFields(c: Cls): Seq[Fld] = c.fields.filterNot(_.info.isOneString)

      // ===========================================================================
      override protected def transformation(qualifyingField: Fld): Any => Any =
        any => conf.transformValue(qualifyingField.isMultiple)(any.asInstanceOf[String]).fold(
          _.map /* option's */(convert(qualifyingField.forceBasicType)),
          _.map /* seq   's */(convert(qualifyingField.forceBasicType)) )

      // ===========================================================================
      private def convert(tipe: BasicType)(value: String): Any = // TODO: t220406110532: proper TableTax counterpart to JSON's (see 220406110635)
             if (tipe.isInt    ) value.toInt
        else if (tipe.isDouble ) value.toDouble
        else if (tipe.isBoolean) inferring.table.BooleanDetector.forceBoolean(value)
        else                     value
}

// ===========================================================================
