package gallia.data.multiple.streamer.spilling

import aptus.aptutils.SystemUtils
import aptus.aptutils.JavaStreamUtils._

// ===========================================================================
object GnuSortByFirstFieldHack { // see https://github.com/galliaproject/gallia-core#poor-mans-scaling-spilling

  def apply(ec: ExecutionContext, debug: String)(numerical: Boolean)(input: Iterator[Line]): Iterator[Line] = {    
    if (IsWindows) { windowsError() }        

    // ---------------------------------------------------------------------------
    // call sort-by-first-field ("key" here) and stream results lines    
    val (os, is) = SystemUtils.streamSystemCall(ec)(Seq( // TODO: t210308150015 - look into https://github.com/com-lihaoyi/os-lib
      "sort",
            "-t", SpillingHackSerialization.FieldSeparator,
            if (numerical) "-k1n,1n"
            else           "-k1,1") ++ 
          gallia.Hacks.ExtraGnuSortOptions // empty by default
        :_*) // TODO: t210304095755 - set locale?

    // ---------------------------------------------------------------------------
    // write input to stream asynchronously
    Future { writeLinesToStream(os, "sort")(input) }(ec)    

    // ---------------------------------------------------------------------------
    readLines(is)
  }
   
}

// ===========================================================================
