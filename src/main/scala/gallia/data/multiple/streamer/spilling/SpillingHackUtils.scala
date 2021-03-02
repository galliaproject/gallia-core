package gallia.data.multiple.streamer.spilling

import aptus._    
import gallia.data.{single, json}

// ===========================================================================
object SpillingHackUtils { // see https://github.com/galliaproject/gallia-core#poor-mans-scaling-spilling

  def serializeEntry(valueIsObj: Boolean)(entry: (Any, Any)) = {         
      val formattedKey  : String = serializeOption(entry._1).getOrElse("")
      val formattedValue: String = serialize(valueIsObj)(entry._2)

      // ---------------------------------------------------------------------------
      s"${formattedKey}\t${formattedValue}"
    }
        
  // ---------------------------------------------------------------------------
  def deserializeEntry[K, V](valueIsObj: Boolean)(line: String): (K, V) = {
    val (formattedKey, formattedValue) = line.splitBy("\t").force.tuple2

    deserializeOption(formattedKey).asInstanceOf[K] -> 
      deserialize(valueIsObj)(formattedValue).asInstanceOf[V]
  }
        
  // ===========================================================================
  private def serializeOption(raw: Any): Option[String] =
    raw
      .asInstanceOf[Option[_]]
      .map { // no other way until homogenize classtag/typetag (see https://github.com/galliaproject/gallia-docs/blob/init/tasks.md#t210116153713)        
        case o: single.Obj => serializeObject(o)
        case x            => x.toString /* will cause issues with sorting eg numbers */ }

  // ---------------------------------------------------------------------------
  private def serialize(valueIsObj: Boolean)(raw: Any): String = 
    if (valueIsObj) serializeObject(raw.asInstanceOf[single.Obj])
    else            raw.toString

  // ---------------------------------------------------------------------------
  private def serializeObject(o: single.Obj): String = o.formatCompactJson /* pretty wasteful (especially the keys)... */
  
  // ===========================================================================
  private def deserializeOption(formatted: String): Any =
    formatted
      .as.noneIf(_ == "")
      .map(_.asInstanceOf[String])
      .map { s =>
        // no other way until homogenize classtag/typetag (see https://github.com/galliaproject/gallia-docs/blob/init/tasks.md#t210116153713)
        deserialize(s.startsWith("{"))(s) }

  // ---------------------------------------------------------------------------
  private def deserialize(isObj: Boolean)(formatted: String): Any = 
    if (isObj) json.GsonToObj.fromObjectString(formatted)
    else       formatted
        
}

// ===========================================================================
