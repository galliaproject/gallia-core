package aptus.misc

import scala.collection.JavaConverters._
import java.util.zip._

import aptus.{Anything_, Seq_}

import aptus.{FilePath, Line, Content}
import aptus.utils.InputStreamUtils

// ===========================================================================
object Zip {
  type RelativePath = String
  //TODO: charset

  // ---------------------------------------------------------------------------
  /** will contain '/' if nesting */
  def listContent(in: FilePath): Seq[RelativePath] =
    new ZipFile(in)
      .thn { zipFile =>
        val names =
          zipFile
            .entries()
            .asScala
            .toList
            .map(_.getName)

        zipFile.close()

        names
      }

  // ---------------------------------------------------------------------------
  def streamLines(in: FilePath)                         : (Iterator[Line], java.io.Closeable) = streamLines(in, _ => true)
  def streamLines(in: FilePath, pred: String => Boolean): (Iterator[Line], java.io.Closeable) =
    new ZipFile(in)
      .thn { zipFile =>
        val zipEntry = entry(zipFile, pred)

        val pair =
          InputStreamUtils.lines (
            zipFile
              .getInputStream(zipEntry)
              .thn(Closeabled.fromPair(_, zipFile)),
            StandardCharsets.UTF_8)

        pair.u -> pair }

  // ---------------------------------------------------------------------------
  def content(in: FilePath)                         : Content = content(in, _ => true)
  def content(in: FilePath, pred: String => Boolean): Content =
    new ZipFile(in)
      .thn { zipFile =>
        val zipEntry = entry(zipFile, pred)

        InputStreamUtils.content(
          zipFile
            .getInputStream(zipEntry)
            .thn(Closeabled.fromPair(_, zipFile)),
          StandardCharsets.UTF_8) }

  // ===========================================================================
  private def entry(zipFile: ZipFile, pred: String => Boolean): ZipEntry =
    zipFile
      .entries()
      .asScala
      .filter(x => pred(x.getName))
      .toList
      .force.one
}

// ===========================================================================