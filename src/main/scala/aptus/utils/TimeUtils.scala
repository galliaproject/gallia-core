package aptus.utils

// ===========================================================================
object TimeUtils {

  def elapsed[A](block: => A): (A, Long) = {
    val start = System.currentTimeMillis()
    val result = block
    val end = System.currentTimeMillis()

    (result, end - start)
  }

}

// ===========================================================================
