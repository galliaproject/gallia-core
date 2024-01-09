package gallia
package data

// ===========================================================================
object GalliaToTableData {

  // note: separator escaping is handled elsewhere (see 220623164424)
  def convert
        (nullValue: String, arraySeparator: String)
        (c: Cls)
        (o: Obj)
      : Seq[String /* cell */] =
    c .fields
      .map { field =>
        o .attemptKey(field.key)
          .map(processFieldString(field, arraySeparator))
          .getOrElse(nullValue) }

  // ===========================================================================
  private def processFieldString(field: Fld, arraySeparator: String)(value: AnyValue): String =
    field.valueExtractionWithMatching(debug = field.skey)(value) {

        // ---------------------------------------------------------------------------
        _ => multiple =>
          if (!multiple) value.asInstanceOf[     Obj  ]                .formatCompactJson
          else           value.asInstanceOf[List[Obj ]].pipe(Objs.from).formatCompactJson } {

        // ---------------------------------------------------------------------------
        bsc => multiple =>
          if (!multiple)  value                      .pipe(bsc.formatStringAsAny)
          else            value.asInstanceOf[List[_]].map (bsc.formatStringAsAny).mkString(arraySeparator) }
      .asInstanceOf[String]

}

// ===========================================================================
