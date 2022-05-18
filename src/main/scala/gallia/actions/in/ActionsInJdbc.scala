package gallia
package actions
package in

// ===========================================================================
case class JdbcInputZ1(
      inputString: String,
      queryingOpt: Option[ReadQuerying] /* missing if URI-driven */)
    extends ActionIZd {
  private var c: Cls = null

  // ---------------------------------------------------------------------------
  def vldt : Errs = Nil //TODO

  def _meta: Cls  =
    AtomsIX._JdbcInputZ1(inputString, queryingOpt, None)
      .columns
      .pipe(utils.JdbcUtils.columnsToCls)
      .tap { c = _ }

  def atomiz: AtomIZ = AtomsIX._JdbcInputZ1(inputString, queryingOpt, Some(c)) }

// ===========================================================================
case class JdbcInputZ2(
      connection: java.sql.Connection,      
      querying: ReadQuerying)
    extends ActionIZd {
  private var c: Cls = null

  // ---------------------------------------------------------------------------
  def vldt : Errs = Nil //TODO

  def _meta: Cls  =
    AtomsIX._JdbcInputZ2(connection, querying, None)
      .columns
      .pipe(utils.JdbcUtils.columnsToCls)
      .tap { c = _ }

  def atomiz: AtomIZ = AtomsIX._JdbcInputZ2(connection, querying, Some(c)) }

// ===========================================================================
