package gallia.actions.in

import gallia._

// ===========================================================================
trait HasProjection {
  val projectionOpt: Option[ReadProjection]

  // ---------------------------------------------------------------------------
  private def resolve(c: Cls): Renz =
    projectionOpt
      .map(_.resolve(c.keyz))
      .getOrElse    (c.keyz.renz)

  // ---------------------------------------------------------------------------
  def projectMeta(c: Cls): Cls =
    c .pipe(resolve)
      .pipe { keyz => c.retain(keyz.froms).rename(keyz) }

  // ---------------------------------------------------------------------------
  def projectData(c: Cls)(z: Objs): Objs =
    resolve(c)
      .pipe { retainees =>
        z.map(
          _ .retain(retainees.froms.values)
            .rename(retainees)) } // TODO: t210106120036 - pointless as it is, point will be to do that before creating obj
}

// ===========================================================================
