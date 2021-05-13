package aptus.utils

import java.io._
import scala.collection.JavaConverters._

// ===========================================================================
object UrlUtils { // TODO: or use commons-io (IOUtils.toString(new java.net.URL(str)))?
  
  def content(value: String): String = {
    val connection = new java.net.URL(value).openConnection()
    val reader     = new BufferedReader(new InputStreamReader(connection.getInputStream(), UTF_8))
    val content    = reader.lines().collect(java.util.stream.Collectors.joining("\n"))
    reader.close() // TODO: enough?
    
    content
  }

  // ---------------------------------------------------------------------------
  def lines(value: String): Seq[String] = {
    val connection = new java.net.URL(value).openConnection()
    val reader     = new BufferedReader(new InputStreamReader(connection.getInputStream(), UTF_8))
    val lines      = reader.lines().collect(java.util.stream.Collectors.toList()).asScala.toList
    
    reader.close() // TODO: enough?
    
    lines
  }  
  
}

// ===========================================================================
