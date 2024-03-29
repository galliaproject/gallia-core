package gallia
package actions
package in

import aptus.{Anything_, String_}
import aptus.Size

import io.in._
import inferring.table.TableSchemaInferrer

// ===========================================================================
case class TableInputZ( // TODO: t210101150123 - split up?
      input: InputUrlLike,

      formatConf: FormatConf,
      cellConf  : CellConf,

      inMemoryMode  : Boolean,
      indexKeysMode : Boolean, // may still be invoked if a header is present (will take precedence over key names from the header)

      schemaProvider: TableSchemaProvider,
      projectionOpt : Option[ReadProjection])
    extends ActionIZ01 with HasProjection {

  private val Tsp = TableSchemaProvider

  private val sep      : Char    = formatConf.sep      (input._inputString)
  private val hasHeader: Boolean = formatConf.hasHeader(input._inputString)

  // ---------------------------------------------------------------------------
  private var preProjectionSchema: Cls = _

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
    def inferFully(keys: Seq[Key]): Cls =
        atomizTable // will ignore uninitialised preProjectionSchema (see t201214105653 and t210106120036)
          .stringObjs(keys)
          /*.pipe(projectData(cc)) TODO: data projection (wasteful)...*/
          .pipe(TableSchemaInferrer.fullInferring(cellConf, keys))

    // ---------------------------------------------------------------------------
    def inferStringsOnly(keys: Seq[Key]): Cls =
        atomizTable // will ignore uninitialised preProjectionSchema (see t201214105653 and t210106120036)
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
      .tap { preProjectionSchema = _ }
      .pipe(projectMeta) } // note: must come after tap because for now it's not a proper retain (we still read it all first) - see t210106120036

  // ===========================================================================
  private def sizeToKeys(size: Size): Seq[Key] = //TODO: ensure header was data
    Range(1, size + 1)
      .toList
      .map(rankToKey)

  // ---------------------------------------------------------------------------
  private def renameKeys(keyz: Keyz)(c: Cls): Cls =
    c .fields
      .zip(keyz.values) // TODO: check same size
      .map { case (field, newKey) => field.copy(key = newKey) }
      .pipe(Cls.apply)

   // ===========================================================================
   final override def atomiz: AtomIZ = atomizTable

     // ---------------------------------------------------------------------------
     private def atomizTable =
       AtomsIX._Table(input,
         cellConf, inMemoryMode,
         schemaProvider, projectionOpt,
         sep, hasHeader,
         preProjectionSchema)

}

// ===========================================================================
