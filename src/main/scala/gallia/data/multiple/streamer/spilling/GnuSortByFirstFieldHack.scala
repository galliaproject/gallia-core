package gallia.data.multiple.streamer.spilling

import java.io.OutputStream
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import gallia.data.multiple.streamer.StreamerUtils

// ===========================================================================
object GnuSortByFirstFieldHack { // see https://github.com/galliaproject/gallia-core#poor-mans-scaling-spilling
  
	def apply(numerical: Boolean)(input: Iterator[String]): Iterator[String] = {
  	  //TODO: check linux (see https://github.com/galliaproject/gallia-core#poor-mans-scaling-spilling); mac ok too?
	    //  looks like GNU sort can be installed for windows as well: same options?

      val sortFieldsOption: String =
        if (numerical) "-k1n,1n"
        else           "-k1,1"

      // call sort-by-first-field ("key" here) and stream results lines
      // TODO: t210301143641 - how to use '\0' here?
      // TODO: t210301165810 - allow hack to provide more options via EV (eg -T /dev/shm, ...)
      val (os, itr) = streamSystemCall("sort", "-t", "\t", sortFieldsOption)

      writeLines(input)(os)

      itr
    }

    // ===========================================================================
  	private def streamSystemCall(args: String*): (OutputStream /* to write input into */, Iterator[String]) = {
      val builder = new java.lang.ProcessBuilder(args:_*)
        builder.redirectErrorStream(true)
  
      val process = builder.start()

      val os = process.getOutputStream() // to write input into
      val is = process.getInputStream () // to obtain output from

      Future(process.waitFor())

      val src = scala.io.Source.fromInputStream(is)      

      os -> StreamerUtils.selfClosing(src.getLines, src)    
    }
      
  // ===========================================================================
	private val NewlineByte = '\n'.toByte
	
	// ---------------------------------------------------------------------------
  private def writeLines(input: Iterator[String])(os: OutputStream) {
    input
      .foreach { line =>
        os.write(line.getBytes :+ NewlineByte)
        os.flush() } // TODO: t210301143620 - flush every X       

    os.close()
  }   	
}

// ===========================================================================
