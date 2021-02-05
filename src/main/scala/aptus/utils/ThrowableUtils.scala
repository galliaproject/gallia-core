package aptus.utils

// ===========================================================================
object ThrowableUtils {

  def stackTraceString(throwable: Throwable): String = { // 201103091210
    val sw = new java.io.StringWriter

    throwable.printStackTrace(
      new java.io.PrintWriter(sw))

    val str = sw.toString
    sw.close()

    str
  }

}

// ===========================================================================
