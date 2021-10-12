package gallia
package atoms.utils

import meta._

// ===========================================================================
trait ModifyObj {
  // TODO: + handle enums (t210116114654)?

  // ---------------------------------------------------------------------------
  def modify(c: Cls)(o: Obj): Obj =
      qualifyingFields(c)
        .foldLeft(o) { (curr, qualifyingField) =>
          val key = qualifyingField.key

          if (!curr.contains(key)) curr
          else curr.transformPath(key, transformation(qualifyingField)) }

    // ===========================================================================
    protected def qualifyingFields(c: Cls): Seq[Fld]
    protected def transformation(qualifyingField: Fld): Any => Any
}

// ===========================================================================
