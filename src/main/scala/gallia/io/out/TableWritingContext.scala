package gallia
package io.out

import scala.collection.JavaConverters._
import org.apache.commons.csv._

// ===========================================================================
case class TableWritingContext(
        fieldSeparator: FieldSeparator,
        arraySeparator: String,
        hasHeader     : Boolean,
        nullValue     : String) {

    // ---------------------------------------------------------------------------
    private lazy val Format =
      TableWritingContext
        .DefaultTableFormat
        .withDelimiter(fieldSeparator) // TODO: cache common ones

    // ===========================================================================
    def formatTable(keys: Seq[String])(data: gallia.Objs): Iterator[String] =
      (if (hasHeader) Iterator(formatTableHeader(keys)) else Iterator()) ++
        data.consume.map      (formatTableRow   (keys))

      // ---------------------------------------------------------------------------
      private def formatTableHeader(keys: Seq[String]): String = formatLine(keys)

      // ---------------------------------------------------------------------------
      private def formatTableRow(keys: Seq[String])(o: gallia.Obj): String =
        keys
          .map { key =>
            o .opt(key)
              .map(TableWritingContext.formatValue(arraySeparator))
              .getOrElse(nullValue) }
          .pipe(formatLine)

    // ===========================================================================
    private def formatLine(values: Seq[String]): String = {
      if (fieldSeparator == '\t') values.mkString("\t") // TODO: escape tabs
      else {
    	  // seems pretty inefficient... find alternative? also limits to Char for separator (see t201229151510)?
        val writer = new java.io.StringWriter()
          val printer = new CSVPrinter(writer, Format)
            printer.printRecord(values.asJava)
          printer.close()
        writer.close()

        writer.toString.dropRight(1 /* "record" separator automatically added */)
      }
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
     
    // ===========================================================================
    private[out] def formatValue(arraySeparator: String)(value: Any): String = // TODO: use schema rather than pattern match (see t210115095838)
      value match {
        case seq: Seq[_] => seq.head match { // guaranteed non-empty by design (else None, not Nil)                    
          case _: Obj      => seq.map(_.asInstanceOf[Obj]).pipe(Objs.from).formatCompactJson
          case _           => seq.map(data.DataFormatting.formatBasicValue).mkString(arraySeparator) }
        case objs: Objs => objs.formatCompactJson // TODO: check can happen?
        case obj : Obj  => obj .formatCompactJson
        case sgl        => data.DataFormatting.formatBasicValue(sgl) }

  }

// ===========================================================================
