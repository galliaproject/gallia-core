package gallia
package io.in

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
  @Distributivity
  def firstLine(): Line = { // TODO: t210114154924 - generalize to n lines + make optional + cache
    val itr = linesPair()

    val streamer =
      Streamer
        .fromIterator(itr)
        .pipeOpt(linesPreprocessing)(f => _.pipe(f))

    val first: Line =
      if (streamer.nonEmpty) streamer.iterator.next()
      else                   ??? // TODO: see t210114154924

    itr.close()

    first
  }

  // ---------------------------------------------------------------------------
  def streamLines(inMemoryMode: Boolean): Streamer[Line] =
    InputUrlLike
      .hackOpt
      .map(_.apply(this))
      .getOrElse {
        linesPair()
          .pipe(InputUrlLike.streamer(inMemoryMode)) }
      .pipeOpt(linesPreprocessing)(f => _.pipe(f))

  // ===========================================================================
  private def linesPair(): Closeabled[Iterator[Line]] = _inputString.pipe(new URL(_)).pipe(_lines())

  // ===========================================================================
  private def _content()(url: URL): Content =
    compression match {

      case SupportedCompression.NoCompression | SupportedCompression.Gzip | SupportedCompression.Bzip2 =>
        InputStreamUtils.content(url, charset.charset)

      case SupportedCompression.Zip =>
        url.toExternalForm().stripPrefix("file:") /* FIXME? */.pipe(aptus.aptmisc.Zip.content) }

  // ===========================================================================
  private def _lines()(url: URL): Closeabled[Iterator[Line]] =
    compression match {
      case SupportedCompression.NoCompression | SupportedCompression.Gzip | SupportedCompression.Bzip2 =>
        InputStreamUtils.lines(url, charset.charset)        

      case SupportedCompression.Zip => ??? /* TODO */ }
}

// ===========================================================================
object InputUrlLike {
  var hackOpt: Option[InputUrlLike => Streamer[Line]] = None // until dust settles and we do proper injection

  // ---------------------------------------------------------------------------
  private def normalize(value: String): String = SupportedUriScheme.file.normalizeOpt(value).getOrElse(value)
  
  // ---------------------------------------------------------------------------
  def streamer[T](inMemoryMode: Boolean)(iter: Closeabled[Iterator[T]]): Streamer[T] = // TODO: move + include hack above properly?
    if (inMemoryMode) Streamer.fromList    (iter.consume(_.toList))
    else              Streamer.fromIterator(iter)

}

// ===========================================================================