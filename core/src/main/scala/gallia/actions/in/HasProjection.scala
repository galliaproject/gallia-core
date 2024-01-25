package gallia
package actions
package in

// ===========================================================================
trait HasNoProjection extends HasProjection { final override val projectionOpt: Option[ReadProjection] = None }

// ---------------------------------------------------------------------------
trait HasProjection {
  val projectionOpt: Option[ReadProjection]

  // ---------------------------------------------------------------------------
  private def resolve(c: Cls): Renz =
    projectionOpt
      .map(_.resolve(c.keyz))
      .getOrElse    (c.keyz.renz)

  // ===========================================================================
  def projectMeta(c: Cls): Cls =
    c .pipe(resolve)
      .pipe { keyz => c.retain(keyz.froms).rename(keyz) }

  // ===========================================================================
  def projectData(c: Cls, o: Obj) : Obj = projectData(resolve(c))(o)
  def projectData(c: Cls)(z: Objs): Objs =
    resolve(c)
      .pipe { retainees =>
        z.map(projectData(retainees)) }

  // ---------------------------------------------------------------------------
  private def projectData(retainees: Renz)(o: Obj): Obj =
    o .retain(retainees.froms.values)
      .rename(retainees) // TODO: t210106120036 - pointless as it is, point will be to do that before creating obj
}

// ===========================================================================
