package gallia.data.multiple.streamer

import java.io.Closeable

// ===========================================================================
private object StreamerUtils {

  def selfClosing[A](parent: Iterator[A], cls: Closeable) = // TODO: to aptus?
    new Iterator[A] with Closeable {
      private var closed: Boolean = false

      // ===========================================================================
      override def hasNext: Boolean = !closed && parent.hasNext

      override def next(): A = {
        val next = parent.next()
        if (!parent.hasNext) { close() }
        next
      }

      // ---------------------------------------------------------------------------
      override def close() { closed = true; cls.close() }
    }

}

// ===========================================================================

