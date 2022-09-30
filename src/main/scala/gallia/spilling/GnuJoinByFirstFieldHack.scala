package gallia
package spilling

import scala.language.postfixOps

// ===========================================================================
// TODO: t220623113637 - reimplement in scala
object GnuJoinByFirstFieldHack { // see https://github.com/galliaproject/gallia-core#poor-mans-scaling-spilling
  // note: nt210302124437 - GNU join needs 2 inputs, forcing us to use at least one named pipe; may be simpler to reimplement in scala (unlike sort)
  //   maybe via process substitution (but then how to provide sort inputs?)
  //   nt210302124437: won't work with windows as a result

  // ---------------------------------------------------------------------------
  def apply(ec: ExecutionContext)(left: CloseabledIterator[Line], right: CloseabledIterator[Line]): CloseabledIterator[Line] = {
      if (IsWindows) { //TODO: t210304095419 - confirm mac ok; same options for sort/join?
        aptus.illegalState("210304095421 - cannot use Windows in hack at the moment (see t210304095420)") }

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
      val removeFifos: java.io.Closeable = () => { s"rm ${leftFifo} ${rightFifo}" !; () }
    
      // ---------------------------------------------------------------------------
      val (_, is) = SystemUtils.streamSystemCall(ec)( // TODO: t210308150015 - look into https://github.com/com-lihaoyi/os-lib - especially for named pipes
        "join", 
            "-t", SpillingHackSerialization.FieldSeparator, 
            "-j", "1",       // on first field
            "-o", "1.2,2.2", // don't output join key
          // using fifos for both, for consistency (see note nt210302124437) 
          leftFifo, rightFifo)

      // ---------------------------------------------------------------------------
      // write input to pipes asynchronously
      Future { JavaStreamUtils.writeLinesToPath(leftFifo , "left" )(left .underlying) }(ec)
      Future { JavaStreamUtils.writeLinesToPath(rightFifo, "right")(right.underlying) }(ec)

      // ---------------------------------------------------------------------------
      new CloseabledIterator(JavaStreamUtils.readLines(is), () => {
        removeFifos.close();
        left.close(); right.close()
        is.close() })
    }
}

// ===========================================================================
