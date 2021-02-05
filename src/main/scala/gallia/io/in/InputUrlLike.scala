package gallia.io.in

import java.io.Closeable
import java.net.URL

import aptus.Anything_
import aptus.{Content, Line}
import aptus.utils.InputStreamUtils
import aptus.Closeabled

import gallia.data.multiple.streamer.Streamer

// ===========================================================================
case class InputUrlLike( // TODO: t210115193904 - check URI, regular file vs symlink, ...
    private val inputString : InputString,

    charset     : SupportedCharset     = SupportedCharset    .UTF_8,
    compression : SupportedCompression = SupportedCompression.NoCompression,

    linesPreprocessing: Option[LinesPreprocessing] = None) {

  val _inputString = InputUrlLike.normalize(inputString) // TODO: validation

  // ===========================================================================
  def content(): Content = _inputString.thn(new URL(_)).thn(_content())

  // ---------------------------------------------------------------------------
  @gallia.Distributivity
  def firstLine(): Line = { // TODO: t210114154924 - generalize to n lines + make optional + cache
    val (itr, cls) = linesPair()

    val streamer =
      Streamer
        .fromIterator(itr)
        .thnOpt(linesPreprocessing)(f => _.thn(f))

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
          .thn { x =>
            if (inMemoryMode) Streamer.fromList(Closeabled.fromPair(x).consume(_.toList))
            else              Streamer.fromIterator(x) } }
      .thnOpt(linesPreprocessing)(f => _.thn(f))

  // ===========================================================================
  private def linesPair(): (Iterator[Line], Closeable) = _inputString.thn(new URL(_)).thn(_linesPair())

  // ===========================================================================
  private def _content()(url: URL): Content =
    compression match {

      case SupportedCompression.NoCompression | SupportedCompression.Gzip | SupportedCompression.Bzip2 =>
        InputStreamUtils.content(url, charset.charset)

      case SupportedCompression.Zip =>
        url.toExternalForm().stripPrefix("file:") /* FIXME? */.thn(aptus.misc.Zip.content) }

  // ===========================================================================
  private def _linesPair()(url: URL): (Iterator[Line], Closeable) =
    compression match {
      case SupportedCompression.NoCompression | SupportedCompression.Gzip | SupportedCompression.Bzip2 =>
        val x = InputStreamUtils.lines(url, charset.charset)
        x.u -> x

      case SupportedCompression.Zip => ??? /* TODO */ }
}

// ===========================================================================
object InputUrlLike {
  var hackOpt: Option[InputUrlLike => Streamer[Line]] = None // until dust settles and we do proper injection

  // ---------------------------------------------------------------------------
  private def normalize(value: String): String = SupportedUriScheme.file.normalizeOpt(value).getOrElse(value)
}

// ===========================================================================