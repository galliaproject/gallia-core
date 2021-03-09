package aptus.utils

import java.io.OutputStream
import java.io.InputStream
import scala.concurrent.Future
import scala.concurrent.ExecutionContext

// ===========================================================================
object SystemUtils {
 
  // TODO: t210308150015 - look into https://github.com/com-lihaoyi/os-lib 
  def streamSystemCall(ec: ExecutionContext)(args: String*): (OutputStream /* to write input into */, InputStream /* to read output from */) = {
    val builder = new java.lang.ProcessBuilder(args:_*)
      builder.redirectErrorStream(true)

    val process = builder.start()

    val os = process.getOutputStream() // to write input into
    val is = process.getInputStream () // to obtain output from

    Future.apply { process.waitFor() } (ec)

    (os, is)    
  }

}

// ===========================================================================
