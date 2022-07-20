package gallia
package data

import aptus.Anything_

// ===========================================================================
object TableToGalliaData {

  def convert(conf: io.CellConf /* TODO: a lighter version */)(c: Cls)(o: Obj): Obj =
      c
        .fields
        .flatMap { field =>
          c.unknownKeys(o).assert(_.isEmpty) // necessary for union types (see 220615165554)

          o .attemptKey(field.key)
            .flatMap {
              _ .asInstanceOf[String]
                .in.noneIf(conf.isNull)
                .map(processStringField(conf)(field))
                .map(field.key -> _) } }
        .pipe(gallia.obj)

    // ---------------------------------------------------------------------------
    private def processStringField(conf: io.CellConf)(field: Fld)(value: String): AnyValue =
      field
        .valueExtractionWithFailures { _ => aptus.illegalState() } { // TODO: support a form of nesting directly? eg nested JSON documents?
          bsc => multiple =>
            if (!multiple) value                      .pipe(conf.transformBasicValue(bsc))
            else           value.pipe(conf.splitArray).map (conf.transformBasicValue(bsc)) }

}

// ===========================================================================