package gallia.data.multiple.streamer.spilling

import scala.language.postfixOps
import aptus.utils.SystemUtils
import aptus.utils.JavaStreamUtils._

// ===========================================================================
object GnuJoinByFirstFieldHack { // see https://github.com/galliaproject/gallia-core#poor-mans-scaling-spilling
  // note: nt210302124437 - GNU join needs 2 inputs, forcing us to use at least one named pipe; may be simpler to reimplement in scala (unlike sort)
  //   maybe via process substitution (but then how to provide sort inputs?)
  //   nt210302124437: won't work with windows as a result
  // =========================================================================== 
  def apply(ec: ExecutionContext)(left: Iterator[Line], right: Iterator[Line]): Iterator[Line] = {				
      if (IsWindows) { windowsError() }
      
      // ---------------------------------------------------------------------------
      val ts = System.currentTimeMillis()
        val leftFifo  = s"/tmp/gallia.left.${ts}"
        val rightFifo = s"/tmp/gallia.right.${ts}"

      // ---------------------------------------------------------------------------  	    
      // create named pipes
      (s"mkfifo ${leftFifo}"  !)	    
      (s"mkfifo ${rightFifo}" !)

      // ---------------------------------------------------------------------------
      // pipe removal procedure: to be executed after reading is done
      val removeFifos = closeable { s"rm ${leftFifo} ${rightFifo}" !; () }
    
      // ---------------------------------------------------------------------------
      val (_, is) = SystemUtils.streamSystemCall(ec)(
        "join", 
            "-t", SpillingHackSerialization.FieldSeparator, 
            "-j", "1",       // on first field
            "-o", "1.2,2.2", // don't output join key
          // using fifos for both, for consistency (see note nt210302124437) 
          leftFifo, rightFifo)
          
      // ---------------------------------------------------------------------------
      // write input to pipes asynchronously
      Future { writeLinesToPath(leftFifo , "left" )(left)  }(ec)
      Future { writeLinesToPath(rightFifo, "right")(right) }(ec)

      // ---------------------------------------------------------------------------
      readLines(is, removeFifos)
    } 

}

// ===========================================================================
