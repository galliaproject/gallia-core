package gallia
package io
package out

import aptus.String_

// ===========================================================================
private[gallia] object OutputDataUtils {

  def writeFileContent(flwc: FileLikeWriteContext)(content: String) = {
    val writer = flwc.outputStreamWriter()
    writer.write(content)
    writer.close() // TODO: confirm closes underlyings?
  }

  // ===========================================================================
  // TODO: move to context?
  // TODO; use streamer
  def writeFileLines(flwc: FileLikeWriteContext)(lines: Iterator[String]) = { // TODO: t210202112204 - linesPostprocessing: Option[LinesPostprocessing]
    val writer = flwc.outputStreamWriter()

    var counter: Long = 0

    lines
      .map(_.newline)
      .foreach { line =>
        counter += 1

        if ((counter % flwc.flushModulo) == 0) {
          writer.flush()

          flwc.log(counter)
        }

        writer.write(line)
      }

    writer.close() // TODO: confirm closes underlyings?
  }

}

// ===========================================================================
