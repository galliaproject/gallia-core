package gallia
package plans

// ===========================================================================
private[plans] trait InputData { def formatDebug: String } // this is a bit of an afterthought... TODO: t210114125607 - improve

  // ===========================================================================
  object InputData {
      private def formatObjsDebug(values: Objs) = { // TODO: see t210114111539 (identity wrapped Us vs full on Zs)
        val itr = values.closeabledIterator // safer with RDD

        try {
          (    if (!itr.hasNext) "(empty data)"
               else              itr.next().formatPrettyJson) // pretty ugly...
            .tap { _ => itr.close() } }
        catch {
          case t: Throwable => s"Throwable: ${t.getMessage} (220623145732)" }
      }

      // ===========================================================================
      case object _None         extends InputData { def formatDebug = "(no input data)" }
      case object _Undetermined extends InputData { def formatDebug = "(could not determine)" }

      case class  _Obj  (value: Obj ) extends InputData { def formatDebug = value.formatPrettyJson }
      case class  _Objs (value: Objs) extends InputData { def formatDebug = formatObjsDebug(value) }
      case class  _Vle  (value: Vle ) extends InputData { def formatDebug = value.toString }

      case class  _Obj2 (value1: Obj , value2: Obj ) extends InputData { def formatDebug = s"${value1.formatPrettyJson}\n${value2.formatPrettyJson}" }
      case class  _Objs2(value1: Objs, value2: Objs) extends InputData { def formatDebug = s"${formatObjsDebug(value1)}\n${formatObjsDebug(value2)}" }
    }

// ===========================================================================
