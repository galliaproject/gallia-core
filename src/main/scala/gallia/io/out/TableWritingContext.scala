package gallia.io.out

import scala.collection.JavaConverters._
import org.apache.commons.csv._
import aptus.Anything_
import gallia.data.DataFormatting.formatBasicValue

// ===========================================================================
case class TableWritingContext(
        fieldSeparator: FieldSeparator,
        arraySeparator: String,
        hasHeader     : Boolean,
        nullValue     : String) {

    // ---------------------------------------------------------------------------
    private val Format =
      TableWritingContext
        .DefaultTableFormat
        .withDelimiter(fieldSeparator) // TODO: cache common ones

    // ===========================================================================
    def formatTable(keys: Seq[String])(data: gallia.Objs): Iterator[String] =
      (if (hasHeader) Iterator(formatTableHeader(keys)) else Iterator()) ++
        data.consume.map      (formatTableRow   (keys))

      // ---------------------------------------------------------------------------
      private def formatTableHeader(keys: Seq[String]): String = formatLine(Format)(keys)

      // ---------------------------------------------------------------------------
      private def formatTableRow(keys: Seq[String])(o: gallia.Obj): String =
        keys
          .map { key =>
            o .opt(key)
              .map(formatValue)
              .getOrElse(nullValue) }
          .thn(formatLine(Format))

    // ===========================================================================
    private def formatValue(value: Any): String = // TODO: use schema rather than pattern match (see t210115095838)
      value match {
        case seq: Seq[_] => seq.map(formatBasicValue).mkString(arraySeparator)
        case sgl         =>         formatBasicValue(sgl) }

    // ===========================================================================
    // seems pretty inefficient... find alternative? also limits to Char for separator (see t201229151510)?
    private def formatLine(format: CSVFormat)(values: Seq[String]): String = {
      val writer = new java.io.StringWriter()
        val printer = new CSVPrinter(writer, format)
          printer.printRecord(values.asJava)
        printer.close()
      writer.close()

      writer.toString.dropRight(1 /* "record" separator automatically added */)
    }
  }

  // ===========================================================================
  object TableWritingContext {
    private lazy val DefaultTableFormat = CSVFormat.DEFAULT

    // ---------------------------------------------------------------------------
    val Default =
      TableWritingContext(
        fieldSeparator = '\t',
        arraySeparator = "|",
        hasHeader      = true,
        nullValue      = "")
  }

// ===========================================================================