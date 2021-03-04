package gallia

// ===========================================================================
object Hacks {
  var IteratorParGroupSize: Option[Int] = None // eg: gallia.Hacks.IteratorPar.GroupSizeOpt = Some(100); may need more memory if big: -XmxNg -XmsNg

  // ---------------------------------------------------------------------------
  var LoseOrderOnGrouping = false 
    
	// ---------------------------------------------------------------------------
	/** removes some runtime checks; TODO: option to remove them all (if confident) */
	var DisableRuntimeChecks: Boolean = false // temporary hack... (see t210107094406); performance won't improve much until t210104164036 is done at least
	
	// ---------------------------------------------------------------------------
	/** to provide an alternative ec if needed */
	var ExecutionContext: concurrent.ExecutionContext = concurrent.ExecutionContext.global
    // these would need shutdown
    //val DefaultExecutorService  = java.util.concurrent.Executors.newFixedThreadPool(1000) // TODO: variable      
    //val DefaultExecutionContext = concurrent.ExecutionContext.fromExecutorService(executorService)  
	
}

// ===========================================================================
