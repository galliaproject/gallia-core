package gallia.selection.untyped.processors

import aptus.Anything_
import aptus.finl

import gallia._
import gallia.target._

// ===========================================================================
trait SelectionValidation { def vldt(c: Cls): Errs }

  // ---------------------------------------------------------------------------
  trait Errorless extends SelectionValidation { final def vldt(c: Cls): Errs = Nil }

  // ===========================================================================
  // TODO: rename the *Selection part

  trait RPathzSelection extends SelectionValidation {
      def qpathz(c: Cls): RPathz

      def tqqpathz = new TqRPathz(this.vldt, this.qpathz) }

    // ---------------------------------------------------------------------------
    trait KPathzSelection extends RPathzSelection {
            def kpaths(c: Cls): Seq[KPath] // easier to fill than than Keyz
      final def kpathz(c: Cls): KPathz = KPathz(kpaths(c))

      @finl def qpathz(c: Cls): RPathz = kpathz(c).qpathz }

      // ---------------------------------------------------------------------------
      trait RPathSelection extends RPathzSelection {
                def qpath (c: Cls): RPath
          @finl def qpathz(c: Cls): RPathz = RPathz(Seq(qpath(c))) }

        // ---------------------------------------------------------------------------
        trait KPathSelection extends RPathSelection with KPathzSelection {
                def kpath(c: Cls): KPath

          @finl def qpath (c: Cls): RPath      = kpath(c).qpath
          @finl def kpaths(c: Cls): Seq[KPath] = kpath(c).in.seq

          @finl override def qpathz(c: Cls): RPathz = super[RPathSelection].qpathz(c)

          // ---------------------------------------------------------------------------
          def tqkpath = new TqKPath(this.vldt, this.kpath) }

        // ---------------------------------------------------------------------------
        trait RenzSelection extends RPathzSelection {
                  def renz  (c: Cls): Renz
            @finl def qpathz(c: Cls): RPathz = renz(c).values.map(_.qpath).pipe(RPathz.apply) }

          // ---------------------------------------------------------------------------
          trait KeyzSelection extends RenzSelection with KPathzSelection {
                  def keys (c: Cls): Seq[Key] // easier to fill than than Keyz
            final def keyz (c: Cls): Keyz     = Keyz(keys(c))

            @finl def renz  (c: Cls): Renz   = keyz(c).renz
            @finl def kpaths(c: Cls): Seq[KPath] = keyz(c).kpathz.values

            @finl override def qpathz(c: Cls): RPathz = super[KPathzSelection].qpathz(c) }

          // ---------------------------------------------------------------------------
          trait RenSelection extends RenzSelection {
            @finl def renz(c: Cls): Renz = Renz(Seq(ren(c)))

            @deprecated def ren (c: Cls):        Ren
                        def ren_(c: Cls): Option[Ren] = ???

            // ---------------------------------------------------------------------------
            def tqren = new TQRen(this.vldt, this.ren) }

          // ---------------------------------------------------------------------------
          trait KeySelection extends KeyzSelection with KPathSelection with RenSelection {
            @finl def keys (c: Cls): Seq[Key] = key_(c).toSeq
            @finl def kpath(c: Cls): KPath    = KPath.from(key(c))

            @finl override def ren   (c: Cls): Ren    = Ren.from(key(c))
            @finl override def renz  (c: Cls): Renz   = super[RenSelection  ].renz(c)
            @finl override def qpathz(c: Cls): RPathz = this                 .kpathz(c).qpathz
            @finl override def kpaths(c: Cls): KPaths = super[KPathSelection].kpaths(c)

            @deprecated def key(c: Cls):         Key
                        def key_(c: Cls): Option[Key] = Some(key(c)) }

// ===========================================================================
