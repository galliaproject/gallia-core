package gallia
package oswo

import aptus._

// ===========================================================================
/** see https://github.com/galliaproject/gallia-docs/blob/master/oswo.md */
trait OswoPrototype {
  private[gallia] var hack = false

  private[gallia] type C2C = Cls => Cls

  private[gallia] def oswoRun(head: heads.Head[_]): Any = { (); }

  // ---------------------------------------------------------------------------
  case class IntraActionClss(values: Seq[Cls])

  // ---------------------------------------------------------------------------
  implicit class OswoAtoms_(u: Seq[AtomOswo]) {
    /** sets _metaIO for each atom */
    def updateAtomMetas(_intras: IntraActionClss): Unit = {} }

  // ---------------------------------------------------------------------------
  implicit class OswoRPathz_(rpaths: RPathz) {
    def intraClss(c: Cls)(f: (Cls, KPath) => Cls): Seq[Cls] =
      rpaths
        .scanLeft(c.in.seq) { (cs, rpath) =>
          val renamingOpt: Option[C2C] = rpath.actualOpt.map(x => _.rename(x))
          val specific   :        C2C  = f(_, rpath.to)

          cs.last.scanLeft(renamingOpt.toList :+ specific) }
        .flatten
        .tail }

  // ===========================================================================
  implicit class OswoCls_(c: Cls) {
    def scanLeft(fs: Seq[C2C]): Seq[Cls] =
      fs.scanLeft(c) { (c2, f) => f.apply(c2) } }

}

// ===========================================================================