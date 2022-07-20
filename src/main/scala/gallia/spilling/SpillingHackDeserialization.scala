package gallia
package spilling

import aptus._    

// ===========================================================================
object SpillingHackDeserialization { // see https://github.com/galliaproject/gallia-core#poor-mans-scaling-spilling

  def deserializeSortingLine[K, V](valueIsObj: Boolean)(line: Line): (K, V) = {
    val (formattedKey, formattedValue) = line.splitBy("\t").force.tuple2

    deserializeSortingKey(formattedKey).asInstanceOf[K] -> 
      deserializeSortingValue(valueIsObj)(formattedValue).asInstanceOf[V]
  }

  // ---------------------------------------------------------------------------
  def deserializeJoiningLine[V](line: Line): (V, V) = {
    val (formattedLeftValue, formattedRightValue) = line.splitBy("\t").force.tuple2

    (deserializeJoiningValue(formattedLeftValue) .asInstanceOf[V], 
     deserializeJoiningValue(formattedRightValue).asInstanceOf[V])
  }

  // ===========================================================================
  private def deserializeSortingKey(formatted: String): Any =
    formatted
      .in.noneIf(_ == "")
      .map { s =>
        // no other way until homogenize classtag/typetag (see https://github.com/galliaproject/gallia-docs/blob/init/tasks.md#t210116153713)
        deserializeSortingValue(s.startsWith("{"))(s) }

  // ---------------------------------------------------------------------------
  private def deserializeSortingValue(isObj: Boolean)(formatted: String): Any = 
    if (isObj) GsonToObj.fromObjectString(formatted)
    else       formatted

  // ---------------------------------------------------------------------------
  private def deserializeJoiningValue(formatted: String): Any =
    formatted
      .in.noneIf(_.isEmpty)
      .map(GsonToObj.fromObjectString)

}

// ===========================================================================
