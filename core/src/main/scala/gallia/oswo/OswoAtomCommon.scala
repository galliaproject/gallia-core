package gallia
package oswo

import aptus._

// ===========================================================================
private object OswoAtomCommon {
  import OswoAtomFormatterUtils._

  // ===========================================================================
  implicit class AtomOswo_(atom: AtomOswo) {
    def formatCommon(ctx: OswoCtx)(methodFields: Seq[SourceString]): SourceString =
      formatCommon2(ctx)(name = common2(atom)(ctx.id)(_.keys))(methodFields)

    // ---------------------------------------------------------------------------
    def formatCommon2(ctx: OswoCtx)(name: Name)(methodFields: Seq[SourceString]): SourceString = { import ctx._
      val ccs = atom._metaIO.out.formatSourceLines(id)(name)

      // ---------------------------------------------------------------------------
      val method = // TODO: use source.x
        s"""|def ${id.method(name)}(x: ${from.cc(name)}): ${id.cc(name)} =
            |  ${id.cc(name)}(
            |${methodFields.map(_.surroundWith("    ", ",")).joinln}
            |  )""".stripMargin

      // ---------------------------------------------------------------------------
      format(ccs, method, _call1(from, id)(last)(name)) } }

}

// ===========================================================================
