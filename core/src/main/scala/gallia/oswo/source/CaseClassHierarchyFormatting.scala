package gallia
package oswo
package source

import aptus._

// ===========================================================================
// TODO: adapt to use Source now
object CaseClassHierarchyFormatting { // 240111124647
  import Domain0._

  // ---------------------------------------------------------------------------
  def apply              (c: Cls): Seq[__Class] = apply("Root")(c)
  def apply(name: String)(c: Cls): Seq[__Class] = rec(name)(c)

    // ---------------------------------------------------------------------------
    private def rec(name: String)(c: Cls): Seq[__Class] = {
      val nestedClasses: Seq[__Class] =
        c .fields
          .flatMap { f =>
            f .nestedClassOpt
              .toSeq
              .flatMap(rec(name = f.skey /* TODO: combine same? how common? */)) }

      // ---------------------------------------------------------------------------
      val nestedClasses2: Seq[__Class] =
        nestedClasses
          .map { x => x.copy(name = "tmp") -> x.name }
          .groupByKeyWithListMap /* maintain order */
          .map { case (nc, name) =>
            // use first (arbitrarily yet predictably) - most of the time there will only be one anyway
            nc.copy(name = name.head) }
          .toList

      // ---------------------------------------------------------------------------
      val rootClass: __Class =
        c .fields
          .map (toField)
          .pipe(__Class(name, _))

      // ---------------------------------------------------------------------------
      rootClass +: nestedClasses2 }

  // ===========================================================================
  private def toField(f: Fld): __Field =
    f .forceInfo1
      .basicTypeOpt
      .map(_.formatScala)
      .getOrElse(f.skey)
      .pipe { n =>
        __Field(f.key.name, n) }

}

// ===========================================================================
