package gallia

import aptus.aptmisc.ThreadLocalWrapper

// ===========================================================================
object Hacks {
  private[gallia] val sparkRddHack = ThreadLocalWrapper.empty[SparkRddHack] // until dust settles and we do proper injection

  // ===========================================================================
  // eg: gallia.Hacks.IteratorPar.GroupSizeOpt = Some(100); may need more memory if big: -XmxNg -XmsNg
  //   note: will be timed by the number of CPUs
  val iteratorParGroupSize = ThreadLocalWrapper.empty[Int]

  // ---------------------------------------------------------------------------
  var loseOrderOnGrouping = ThreadLocalWrapper.empty[Boolean]
    
  // ---------------------------------------------------------------------------
  /** removes some runtime checks (beyond -Xdisable-asserts); TODO: option to remove them all (if confident) */
  var disableRuntimeChecks = ThreadLocalWrapper.empty[Boolean] // temporary hack... (see t210107094406); performance won't improve much until t210104164036 is done at least
  
  // ---------------------------------------------------------------------------
  var extraGnuSortOptions = ThreadLocalWrapper.empty[Seq[String]] // eg Seq("-T", "/dev/shm"), Seq("--parallel=16"); though note: https://unix.stackexchange.com/questions/120096/how-to-sort-big-files#comment188976_120100
  
  // ---------------------------------------------------------------------------
  /** to provide an alternative ec if needed */
  var galliaExecutionContext = ThreadLocalWrapper.init[concurrent.ExecutionContext](concurrent.ExecutionContext.global)
    // these would need shutdown
    //val DefaultExecutorService  = java.util.concurrent.Executors.newFixedThreadPool(1000) // TODO: variable      
    //val DefaultExecutionContext = concurrent.ExecutionContext.fromExecutorService(executorService)
}

// ===========================================================================
