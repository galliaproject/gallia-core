package gallia
package actions
package utils

// ===========================================================================
private[actions] object ActionsCommonHelper {

  object Translate {

    def wrap(mapping: Seq[(_, _)], toOpt: Option[_]): Any => Any = wrap(mapping, strict = toOpt.nonEmpty)

    def wrap(mapping: Seq[(_, _)], strict: Boolean): Any => Any = {
      val map =
        mapping
          .asInstanceOf[Seq[(Any, Any)]]
          .toMap

      if (strict) x => map.apply    (x)
      else        x => map.getOrElse(x, x)
    }
  }

}

// ===========================================================================
