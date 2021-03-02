package gallia.data.multiple.streamer.spilling

import aptus.utils.SystemUtils
import aptus.utils.JavaStreamUtils._

// ===========================================================================
object GnuJoinByFirstFieldHack { // see https://github.com/galliaproject/gallia-core#poor-mans-scaling-spilling
  // note: nt210302124437 - GNU join needs 2 inputs, forcing us to use at least one named pipe; may be simpler to reimplement in scala (unlike sort)

  // =========================================================================== 
  def apply(ec: ExecutionContext)(left: Iterator[Line], right: Iterator[Line]): Iterator[Line] = {				

      val ts = System.currentTimeMillis()
        val leftFifo  = s"/tmp/gallia.left.${ts}"
        val rightFifo = s"/tmp/gallia.right.${ts}"

      // ---------------------------------------------------------------------------  	    
      // create named pipes; TODO: won't work with windows (see note nt210302124437)
      (s"mkfifo ${leftFifo}"  !)	    
      (s"mkfifo ${rightFifo}" !)

      // ---------------------------------------------------------------------------
      // pipe removal procedure: to be executed after reading is done
      val removeFifos = closeable { s"rm ${leftFifo} ${rightFifo}" !; () }
    
      // ---------------------------------------------------------------------------
      val (os, is) = SystemUtils.streamSystemCall(ec)(
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
