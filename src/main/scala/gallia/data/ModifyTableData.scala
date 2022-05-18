package gallia
package data

import meta._

// ===========================================================================
class ModifyTableData(conf: io.CellConf) { // 201231113658

  def modify(value: AObjs): Objs =
      value
        .z
        .map { o =>
          qualifyingFields(value.c)
            .foldLeft(o) { (curr, qualifyingField) =>
              val key = qualifyingField.key

              if (!curr.contains(key)) curr
              else curr.transformPath(key, transformation(qualifyingField)) } }

    // ===========================================================================
    private def qualifyingFields(c: Cls): Seq[Fld] = c.fields.filterNot(_.isOneString)

      // ===========================================================================
      private def transformation(qualifyingField: Fld): Any => Any =
        any => conf.transformValue(qualifyingField.subInfo1.isMultiple)(any.asInstanceOf[String]).fold(
          _.map /* option's */(conf.transformBasicValue(qualifyingField.forceBasicType)),
          _.map /* seq   's */(conf.transformBasicValue(qualifyingField.forceBasicType)) )

}

// ===========================================================================
