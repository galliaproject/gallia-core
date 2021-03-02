package gallia.data.multiple.streamer.spilling

// ===========================================================================
object SpillingHackSerialization { // see https://github.com/galliaproject/gallia-core#poor-mans-scaling-spilling
  // TODO: t210301143641 - how to use '\0' in the sort and join command ProcessBuilder calls?
  //   getting: Exception in thread "main" java.io.IOException: invalid null character in command
  val FieldSeparator = "\t"

  // =========================================================================== 
  def serializeSortingLine(valueIsObj: Boolean)(entry: (Any, Any)): Line = {         
      val formattedKey  : String = serializeSortingKey(entry._1).getOrElse("")
      val formattedValue: String = serializeSortingValue(valueIsObj)(entry._2)

      // ---------------------------------------------------------------------------
      s"${formattedKey}\t${formattedValue}"
    }

  // ---------------------------------------------------------------------------
  def serializeSideSortingLine(entry: (Any, Any)): Line = {         
      val formattedKey  : String = serializeSortingKey2  (entry._1).getOrElse("")
      val formattedValue: String = serializeSideSortingValue(entry._2).getOrElse("")

      // ---------------------------------------------------------------------------
      s"${formattedKey}\t${formattedValue}"
    }
  
  // ===========================================================================
  private def serializeSortingKey(raw: Any): Option[String] =
    raw
      .asInstanceOf[Option[_]]
      .map { // no other way until homogenize classtag/typetag (see https://github.com/galliaproject/gallia-docs/blob/init/tasks.md#t210116153713)        
        case o: Obj => serializeObject(o)
        case x      => serializeRawValue(x) }

  // ---------------------------------------------------------------------------
  private def serializeSortingKey2(raw: Any): Option[String] =
    raw
      .asInstanceOf[Option[_]]
      .map(serializeRawValue)
  
  // ---------------------------------------------------------------------------
  private def serializeSortingValue(valueIsObj: Boolean)(raw: Any): String = 
    if (valueIsObj) serializeObject(raw.asInstanceOf[Obj])
    else            raw.toString

  // ---------------------------------------------------------------------------
  private def serializeSideSortingValue(raw: Any): Option[String] = raw.asInstanceOf[Option[Obj]].map(serializeObject)
    
  // ===========================================================================
  private def serializeRawValue(x: Any): String = x.toString          // will cause issues with sorting eg numbers
  private def serializeObject  (o: Obj): String = o.formatCompactJson // pretty wasteful (especially the keys)... 
}

// ===========================================================================
