package gallia.data.multiple.streamer

// ===========================================================================
package object spilling {  
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