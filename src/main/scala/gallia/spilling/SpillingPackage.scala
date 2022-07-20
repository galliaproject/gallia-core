package gallia

// ===========================================================================
package object spilling {
  val IsWindows: Boolean = Option(System.getProperty("os.name")).exists(_.toLowerCase.startsWith("windows")) // should cover most cases?

  // ---------------------------------------------------------------------------
  private[spilling] type CloseabledIterator[T] = aptus.CloseabledIterator[T]
  private[spilling] val  CloseabledIterator    = aptus.CloseabledIterator

  // ---------------------------------------------------------------------------
  private[spilling] val SystemUtils     = aptus.aptutils.SystemUtils
  private[spilling] val JavaStreamUtils = aptus.aptutils.JavaStreamUtils

  // ---------------------------------------------------------------------------
  private[spilling] type Line = aptus.Line

  // ---------------------------------------------------------------------------
  private[spilling] type Obj       = gallia.Obj
  private[spilling] val  GsonToObj = gallia.data.json.GsonToObj
  
  // ---------------------------------------------------------------------------  
  private[spilling] implicit def stringToProcess: String => sys.process.ProcessBuilder = sys.process.stringToProcess _
  
  // ---------------------------------------------------------------------------  
  private[spilling] type Future[T] = scala.concurrent.Future[T]
  private[spilling] val  Future    = scala.concurrent.Future
  
  // ---------------------------------------------------------------------------
  private[spilling] type ExecutionContext = scala.concurrent.ExecutionContext
  
  // ---------------------------------------------------------------------------
  private[spilling] def closeable(f: => Unit) = new java.io.Closeable { def close() = { f } }

  // ===========================================================================
  trait SortWrapper {
    val multiple   : Boolean
    val numerical  : Boolean
    val reverse    : Boolean
    val missingLast: Boolean

    // ---------------------------------------------------------------------------
    def sortingPair = domain.SortingPair(descending  = reverse, missingLast)
  }
}

// ===========================================================================
