package gallia.data.multiple.streamer

// ===========================================================================
package object spilling { 
  val IsWindows: Boolean = Option(System.getProperty("os.name")).exists(_.toLowerCase.startsWith("windows")) // should cover most cases?  
    def windowsError() { gallia.illegal("210304095421 - cannot use Windows in hack at the moment (see t210304095420)") } //TODO: t210304095419 - confirm mac ok; same options for sort/join?        

  // ---------------------------------------------------------------------------
  type Line = String

  // ---------------------------------------------------------------------------
  type Obj       = gallia.data.single.Obj
  val  GsonToObj = gallia.data.json.GsonToObj
  
  // ---------------------------------------------------------------------------  
  implicit def stringToProcess = sys.process.stringToProcess _
  
  // ---------------------------------------------------------------------------  
  type Future[T] = scala.concurrent.Future[T]
  val  Future    = scala.concurrent.Future
  
  // ---------------------------------------------------------------------------
  type ExecutionContext = scala.concurrent.ExecutionContext
  
  // ---------------------------------------------------------------------------
  def closeable(f: => Unit) = new java.io.Closeable { def close() { f } }
}

// ===========================================================================