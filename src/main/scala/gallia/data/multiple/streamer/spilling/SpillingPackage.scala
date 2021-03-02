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
  var  executionContext: ExecutionContext = concurrent.ExecutionContext.global
    // ---------------------------------------------------------------------------
    // these need shutdown
    //val DefaultExecutorService  = java.util.concurrent.Executors.newFixedThreadPool(1000) // TODO: variable      
    //val DefaultExecutionContext = concurrent.ExecutionContext.fromExecutorService(executorService)  
  
  // ---------------------------------------------------------------------------
  def closeable(f: => Unit) = new java.io.Closeable { def close() { f } }
}

// ===========================================================================