package gallia
package io.in

import java.io.Closeable
import java.net.URL

import aptus._
import aptus.aptutils.InputStreamUtils
import gallia.data.multiple.streamer.Streamer

// ===========================================================================
case class InputUrlLike( // TODO: t210115193904 - check URI, regular file vs symlink, ...
    private val inputString : InputString,

    charset     : SupportedCharset     = SupportedCharset    .UTF_8,
    compression : SupportedCompression = SupportedCompression.NoCompression,

    linesPreprocessing: Option[LinesPreprocessing] = None) {

  val _inputString = InputUrlLike.normalize(inputString) // TODO: validation

  // ===========================================================================
  def content(): Content = _inputString.pipe(new URL(_)).pipe(_content())

  // ---------------------------------------------------------------------------
  @gallia.Distributivity
  def firstLine(): Line = { // TODO: t210114154924 - generalize to n lines + make optional + cache
    val (itr, cls) = linesPair()

    val streamer =
      Streamer
        .fromIterator(itr)
        .pipeOpt(linesPreprocessing)(f => _.pipe(f))

    val first: Line =
      if (streamer.nonEmpty) streamer.iterator.next()
      else                   ??? // TODO: see t210114154924

    cls.close()

    first
  }

  // ---------------------------------------------------------------------------
  def streamLines(inMemoryMode: Boolean): Streamer[Line] =
    InputUrlLike
      .hackOpt
      .map(_.apply(this))
      .getOrElse {
        linesPair()
          .pipe { x =>
            if (inMemoryMode) Streamer.fromList(Closeabled.fromPair(x).consume(_.toList))
            else              Streamer.fromIterator(x) } }
      .pipeOpt(linesPreprocessing)(f => _.pipe(f))

  // ===========================================================================
  private def linesPair(): (Iterator[Line], Closeable) = _inputString.pipe(new URL(_)).pipe(_linesPair())

  // ===========================================================================
  private def _content()(url: URL): Content =
    compression match {

      case SupportedCompression.NoCompression | SupportedCompression.Gzip | SupportedCompression.Bzip2 =>
        InputStreamUtils.content(url, charset.charset)

      case SupportedCompression.Zip =>
        url.toExternalForm().stripPrefix("file:") /* FIXME? */.pipe(aptus.aptmisc.Zip.content) }

  // ===========================================================================
  private def _linesPair()(url: URL): (Iterator[Line], Closeable) =
    compression match {
      case SupportedCompression.NoCompression | SupportedCompression.Gzip | SupportedCompression.Bzip2 =>
        val x = InputStreamUtils.lines(url, charset.charset)
        x.underlying -> x

      case SupportedCompression.Zip => ??? /* TODO */ }
}

// ===========================================================================
object InputUrlLike {
  var hackOpt: Option[InputUrlLike => Streamer[Line]] = None // until dust settles and we do proper injection

  // ---------------------------------------------------------------------------
  private def normalize(value: String): String = SupportedUriScheme.file.normalizeOpt(value).getOrElse(value)
}

// ===========================================================================