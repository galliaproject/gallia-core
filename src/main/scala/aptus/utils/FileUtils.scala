package aptus.utils

import java.io._
import java.util.zip.GZIPOutputStream

import aptus.{FilePath, Content, Line}
import aptus.Unit_

// ===========================================================================
object FileUtils {

  def readFileContent(path: FilePath): Content     = { val src = io.Source.fromInputStream(inputStream(quickNormalize(path))); val content = src.mkString       ; src.close; content }
  def readFileLines  (path: FilePath): Seq[String] = { val src = io.Source.fromInputStream(inputStream(quickNormalize(path))); val lines   = src.getLines.toList; src.close; lines   }

  def streamFileLines  (path: FilePath): (Iterator[String], Closeable) = { val src = io.Source.fromInputStream(inputStream(quickNormalize(path))); src.getLines -> src }
  
    // ---------------------------------------------------------------------------
    private def inputStream(path: FilePath): InputStream =
      InputStreamUtils.smartInputStream(
        new BufferedInputStream(
          new FileInputStream(path)))

  // ===========================================================================
  def writeContent     (path: FilePath, content: Content): FilePath = if (path.endsWith(".gz")) writeGzipContent(path, content) else writePlainContent(path, content)
  def writePlainContent(path: FilePath, content: Content): FilePath = { val fw = plainFileWriter(path); fw.write(content); fw.close(); path }
  def writeGzipContent (path: FilePath, content: Content): FilePath = { val fw =  gzipFileWriter(path); fw.write(content); fw.close(); path }

  // ---------------------------------------------------------------------------
  def writeLines     (path: FilePath, values: Seq[Line]): FilePath = if (path.endsWith(".gz")) writeGzipLines(path, values) else writePlainLines(path, values)
  def writePlainLines(path: FilePath, values: Seq[Line]): FilePath = { val fw = plainFileWriter(path); values.map(x => s"${x}\n").foreach(fw.write) /* TODO: flush often? */; fw.close(); path }
  def writeGzipLines (path: FilePath, values: Seq[Line]): FilePath = { val fw =  gzipFileWriter(path); values.map(x => s"${x}\n").foreach(fw.write) /* TODO: flush often? */; fw.close(); path }

  // ===========================================================================
  // writers

  private def gzipFileWriter(path: FilePath): Writer = new OutputStreamWriter(syncFlushGzipOutputStream(path))

  private def plainFileWriter(path: FilePath, append: Boolean = false): Writer = new FileWriter(quickNormalize(path), append)

  // ===========================================================================
  // streams

  private def fileOutputStream(path: FilePath, append: Boolean = false): OutputStream = new FileOutputStream(quickNormalize(path), append)

  //https://stackoverflow.com/questions/3640080/force-flush-on-a-gzipoutputstream-in-java
  private def syncFlushGzipOutputStream(path: FilePath): OutputStream =
      new GZIPOutputStream(fileOutputStream(quickNormalize(path)), /*syncFlush = */true)

  // ===========================================================================
  private def quickNormalize(path: FilePath): FilePath =
    if (!path.startsWith("~/")) path
    else                        s"${().fs.homeDirectoryPath}/${path.drop(2)}" // eg ~/foo to /home/tony/foo

}

// ===========================================================================
