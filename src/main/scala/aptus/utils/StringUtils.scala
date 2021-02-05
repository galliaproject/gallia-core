package aptus.utils

import scala.collection.JavaConverters._
import aptus.{Anything_, String_}
import aptus.Iterator_
import aptus.Cell

// ===========================================================================
object StringUtils {

  lazy val Default = org.apache.commons.csv.CSVFormat.DEFAULT

  // ===========================================================================
  // TODO: efficiency

  def indentAll(n: Int, indenter: String)(str: String): String =
      str
        .splitBy("\n")
        .map(indent(n, indenter))
        .mkString("\n")

    // ---------------------------------------------------------------------------
    def sectionAllOff(n: Int, indenter: String)(str: String): String =
      str
        .indentAll(n)
        .prepend("\n")

    // ---------------------------------------------------------------------------
    def indent(n: Int, indenter: String)(str: String): String = {
      require(n >= 0, (n, str))

      (indenter * n) + str
    }

    // ---------------------------------------------------------------------------
    def section[A](coll: Seq[A], n: Int, title: String): String =
      coll
        .mkString("\n")
        .sectionAllOff(n)
        .prepend(
          title
            .thnIf(!_.endsWith(":")) {
              _.append(":") } )

  // ===========================================================================
  def splitXsv(line: String, sep: Char): List[Cell] = {
    require(!line.contains("\n"), line)

    val parser =
      Default
        .withDelimiter(sep)
        .parse(new java.io.StringReader(line))

    val cells =
      parser
        .iterator().asScala.last() // grab first and only record
        .iterator().asScala.toList

     parser.close()

    cells
  }

}

// ===========================================================================
