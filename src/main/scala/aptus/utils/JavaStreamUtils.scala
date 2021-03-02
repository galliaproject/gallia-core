package aptus.utils

import java.io._

// ===========================================================================
object JavaStreamUtils {  
  private val NewlineByte = '\n'.toByte

  // ===========================================================================
  def readLines(is: InputStream, closeables: Closeable*): Iterator[String] = {
    val src = scala.io.Source.fromInputStream(is)
    
    IteratorUtils.selfClosing(src.getLines, (src +: closeables):_*)
  }
	
  // ===========================================================================
  def writeLinesToStream(os: OutputStream, debug: String = "")(input: Iterator[String]) {	  
      _writeLines(new BufferedOutputStream(os), debug)(input) }
  
    // ---------------------------------------------------------------------------
    def writeLinesToPath(path: String, debug: String = "")(input: Iterator[String]) {  		
      _writeLines(new BufferedOutputStream(new FileOutputStream(path)), debug)(input) }

    // ===========================================================================	
    private def _writeLines(bos: BufferedOutputStream, debug: String)(input: Iterator[String]) {
      input
  	.foreach { line =>  		    
  	bos.write(line.getBytes :+ NewlineByte)
  	bos.flush() } // TODO: t210301143620 - flush every X       
  		
  	bos.close() // will close underlyings
    }

}

// ===========================================================================
