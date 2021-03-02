package gallia.data.multiple.streamer.spilling

import aptus.utils.SystemUtils
import aptus.utils.JavaStreamUtils._

// ===========================================================================
object GnuSortByFirstFieldHack { // see https://github.com/galliaproject/gallia-core#poor-mans-scaling-spilling

  def apply(ec: ExecutionContext, debug: String)(numerical: Boolean)(input: Iterator[Line]): Iterator[Line] = {
    //TODO: t210302132752 - check is linux (see https://github.com/galliaproject/gallia-core#poor-mans-scaling-spilling); mac ok too?
    //  looks like GNU sort can be installed for windows as well: same options?

    // ---------------------------------------------------------------------------
    // call sort-by-first-field ("key" here) and stream results lines
    val (os, is) = SystemUtils.streamSystemCall(ec)(
    		// TODO: t210301165810 - allow hack to provide more options via EV (eg -T /dev/shm, ...)
        "sort",
            "-t", SpillingHackSerialization.FieldSeparator,
            if (numerical) "-k1n,1n"
            else           "-k1,1",
          "-")

    // ---------------------------------------------------------------------------
    // write input to stream asynchronously
    Future { writeLinesToStream(os, "sort")(input) }(ec)    

    // ---------------------------------------------------------------------------
    readLines(is)
  }
   
}

// ===========================================================================
