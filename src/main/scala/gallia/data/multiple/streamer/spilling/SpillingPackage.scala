package gallia
package data
package multiple
package streamer

// ===========================================================================
package object spilling {
  val IsWindows: Boolean = Option(System.getProperty("os.name")).exists(_.toLowerCase.startsWith("windows")) // should cover most cases?

  // ---------------------------------------------------------------------------
  type Line = String

  // ---------------------------------------------------------------------------
  type Obj       = gallia.Obj
  val  GsonToObj = gallia.data.json.GsonToObj
  
  // ---------------------------------------------------------------------------  
  implicit def stringToProcess: String => sys.process.ProcessBuilder = sys.process.stringToProcess _
  
  // ---------------------------------------------------------------------------  
  type Future[T] = scala.concurrent.Future[T]
  val  Future    = scala.concurrent.Future
  
  // ---------------------------------------------------------------------------
  type ExecutionContext = scala.concurrent.ExecutionContext
  
  // ---------------------------------------------------------------------------
  def closeable(f: => Unit) = new java.io.Closeable { def close() = { f } }

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