package gallia
package io
package out

import scala.jdk.CollectionConverters._
import org.apache.commons.csv._

// ===========================================================================
case class TableWritingContext( // 220623164424
        fieldSeparator: FieldSeparator,
        arraySeparator: String,
        hasHeader     : Boolean,
        nullValue     : String)
      extends in.TableIoContext {

    def formatTable(c: Cls)(data: Objs): Iterator[String] =
        (if (hasHeader)      Iterator(formatTableHeader(c)) else Iterator()) ++
          data.consumeSelfClosing.map(formatTableRow   (c))

      // ---------------------------------------------------------------------------
      private def formatTableHeader(c: Cls): String =
        TableWritingContext.formatLine(Format)(c.skeys)

      // ---------------------------------------------------------------------------
      private def formatTableRow(c: Cls)(o: gallia.Obj): String =
        data.GalliaToTableData
          .convert(nullValue, arraySeparator)(c)(o)
          .pipe(TableWritingContext.formatLine(Format))
  }

  // ===========================================================================
  object TableWritingContext {

    val Default =
      TableWritingContext(
        fieldSeparator = '\t',
        arraySeparator = "|",
        hasHeader      = true,
        nullValue      = "")

      // ---------------------------------------------------------------------------
      private def formatLine(format: CSVFormat)(values: Seq[String]): String = {
        // seems pretty inefficient... find alternative? also limits to Char for separator (see t201229151510)?
        val writer = new java.io.StringWriter()
          val printer = new CSVPrinter(writer, format)
            printer.printRecord(values.asJava)
          printer.close()
        writer.close()

        writer.toString.dropRight(1 /* "record" separator automatically added */) }

  }

// ===========================================================================
