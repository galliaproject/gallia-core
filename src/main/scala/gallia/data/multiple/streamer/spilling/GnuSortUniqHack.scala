package gallia
package data
package multiple
package streamer
package spilling

import aptus.CloseabledIterator
import aptus.aptutils.SystemUtils
import aptus.aptutils.JavaStreamUtils._

// ===========================================================================
object GnuSortUniqHack { // see https://github.com/galliaproject/gallia-core#poor-mans-scaling-spilling

  def apply(ec: ExecutionContext, debug: String)(input: CloseabledIterator[Line]): CloseabledIterator[Line] = {
    //TODO: t210304095419 - confirm mac ok; same options for sort/join? also for window's CoreUtils port
    //if (IsWindows) { windowsError() } - seems like gnusort is also usable in Windows via http://gnuwin32.sourceforge.net/packages/coreutils.htm

    // ---------------------------------------------------------------------------
    // call sort-by-first-field ("key" here) and stream results lines
    val (os, is) = SystemUtils.streamSystemCall(ec)(Seq( // TODO: t210308150015 - look into https://github.com/com-lihaoyi/os-lib
      "sort", "-u") ++
          Hacks.ExtraGnuSortOptions // empty by default
        :_*) // TODO: t210304095755 - set locale?

    // ---------------------------------------------------------------------------
    // write input to stream asynchronously
    Future { writeLinesToStream(os, debug = "sort")(input.underlying) }(ec)

    // ---------------------------------------------------------------------------
    new CloseabledIterator(readLines(is), () => { input.close(); is.close() })
  }

}

// ===========================================================================
