package gallia.actions.in

import aptus.{Anything_, String_}
import aptus.Size

import gallia._
import gallia.io.in._
import gallia.inferring.table.TableSchemaInferrer

// ===========================================================================
case class TableInputZ( // TODO: t210101150123 - split up?
      input: InputUrlLike,

      formatConf: FormatConf,
      cellConf  : CellConf,

      inMemoryMode  : Boolean,
      indexKeysMode : Boolean, // may still be invoked if a header is present (will take precedence over key names from the header)

      schemaProvider: TableSchemaProvider,
      projectionOpt : Option[ReadProjection])
    extends ActionIZd with HasProjection {
  private val Tsp = TableSchemaProvider

  private var defaultCls2: Cls = _ // can't use resultCls because of t210106120036 (delayed data projection)

  private val sep      : Char    = formatConf.sep      (input._inputString)
  private val hasHeader: Boolean = formatConf.hasHeader(input._inputString)

  // ===========================================================================
  def vldt: Errs = Nil
    //TODO: validate sep/hasHeader
    //      checkUri(inputString: String) ++
    //      vldt.checkFileLike(url) ++
    //      vldt.checkCharset(charset)

  // ===========================================================================
  def _meta: Cls = {
      val default: Cls =
        input
          .firstLine()
          .splitXsv(sep)
          .pipe { cells =>
            if (hasHeader && !indexKeysMode) cells.map(_.pipeIf(_.isEmpty)(_ => "_missing").symbol)
            else                             cells.size.pipe(sizeToKeys) }
          .pipe(Cls.from)

      // ---------------------------------------------------------------------------
      def inferFully(keys: Seq[Key]) =
          atomiz // will ignore uninitialised defaultCls2 (see t201214105653 and t210106120036)
            .stringObjs(keys)
            /*.pipe(projectData(cc)) TODO: data projection (wasteful)...*/
            .pipe(TableSchemaInferrer.fullInferring(cellConf, keys))

        // ---------------------------------------------------------------------------
        def inferStringsOnly(keys: Seq[Key]) =
          atomiz // will ignore uninitialised defaultCls2 (see t201214105653 and t210106120036)
            .stringObjs(keys)
            /*.pipe(projectData(cc)) TODO: data projection (wasteful)...*/
            .pipe(TableSchemaInferrer.stringsOnly(cellConf, keys))

      // ---------------------------------------------------------------------------
      (schemaProvider match {
            case Tsp.NoInferring        => default
            case Tsp.StringsOnly        => inferStringsOnly(default.keys)
            case Tsp.InferSchema        => inferFully(default.keys)
            case Tsp.ExplicitKeys(keyz) => inferFully(default.keys).pipe(renameKeys(keyz))
            case Tsp.ExplicitSchema(c)  => c /* TODO: allow contradictions even in field names vs header? */ })
        .tap(defaultCls2 = _)
        .pipe(projectMeta) // note: must come after tap because for now it's not a proper retain (we still read it all first) - see t210106120036
    }

    // ===========================================================================
    def sizeToKeys(size: Size): Seq[Key] = //TODO: ensure header was data
      Range(1, size + 1)
        .toList
        .map(rankToKey)

    // ---------------------------------------------------------------------------
    def renameKeys(keyz: Keyz)(c: Cls): Cls =
      c .fields
        .zip(keyz.values) // TODO: check same size
        .map { case (field, newKey) => field.copy(key = newKey) }
        .pipe(Cls.apply)

   // ===========================================================================
   def atomiz =
     AtomsIX._Table(input,
         cellConf, inMemoryMode,
         schemaProvider, projectionOpt,
         sep, hasHeader,
          defaultCls2) // TODO: t201214105653 - address resultCls hack

}

// ===========================================================================
