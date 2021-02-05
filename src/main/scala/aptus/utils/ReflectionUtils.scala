package aptus.utils

import aptus.Seq_

// ===========================================================================
object ReflectionUtils {

  def formatExitTrace(trace: List[StackTraceElement], message: String): String =
    trace
      .dropWhile(_.getMethodName != "__exit$extension")
      .dropWhile(_.getMethodName != "p__$extension")
      .drop(1)
      .section(message)

}

// ===========================================================================
