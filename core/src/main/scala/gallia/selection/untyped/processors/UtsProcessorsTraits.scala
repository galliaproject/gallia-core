package gallia
package selection
package untyped
package processors

import aptus.Anything_
import aptus.finl

import trgt._

// ===========================================================================
trait SelectionValidation { def vldt(c: Cls): Errs }

  // ---------------------------------------------------------------------------
  trait Errorless extends SelectionValidation { final def vldt(c: Cls): Errs = Nil }

  // ===========================================================================
  // TODO: rename the *Selection part

  trait RPathzSelection extends SelectionValidation {
      def rpathz(c: Cls): RPathz

      def tqrpathz = new TqRPathz(this.vldt, this.rpathz) }

    // ---------------------------------------------------------------------------
    trait KPathzSelection extends RPathzSelection {
            def kpaths(c: Cls): Seq[KPath] // easier to fill than than Keyz
      final def kpathz(c: Cls): KPathz = KPathz(kpaths(c))

      @finl def rpathz(c: Cls): RPathz = kpathz(c).rpathz }

      // ---------------------------------------------------------------------------
      trait RPathSelection extends RPathzSelection {
                def rpath (c: Cls): RPath
          @finl def rpathz(c: Cls): RPathz = RPathz(Seq(rpath(c))) }

        // ---------------------------------------------------------------------------
        trait KPathSelection extends RPathSelection with KPathzSelection {
                def kpath(c: Cls): KPath

          @finl def rpath (c: Cls): RPath      = kpath(c).rpath
          @finl def kpaths(c: Cls): Seq[KPath] = kpath(c).in.seq

          @finl override def rpathz(c: Cls): RPathz = super[RPathSelection].rpathz(c)

          // ---------------------------------------------------------------------------
          def tqkpath = new TqKPath(this.vldt, this.kpath) }

        // ---------------------------------------------------------------------------
        trait RenzSelection extends RPathzSelection {
                  def renz  (c: Cls): Renz
            @finl def rpathz(c: Cls): RPathz = renz(c).values.map(_.rpath).pipe(RPathz.apply) }

          // ---------------------------------------------------------------------------
          trait KeyzSelection extends RenzSelection with KPathzSelection {
                  def keys (c: Cls): Seq[Key] // easier to fill than than Keyz
            final def keyz (c: Cls): Keyz     = Keyz(keys(c))

            @finl def renz  (c: Cls): Renz   = keyz(c).renz
            @finl def kpaths(c: Cls): Seq[KPath] = keyz(c).kpathz.values

            @finl override def rpathz(c: Cls): RPathz = super[KPathzSelection].rpathz(c) }

          // ---------------------------------------------------------------------------
          trait RenSelection extends RenzSelection {
            @finl def renz(c: Cls): Renz = Renz(Seq(ren(c)))

            @deprecated def ren (c: Cls):        Ren
                        def ren_(c: Cls): Option[Ren] = ???

            // ---------------------------------------------------------------------------
            def tqren = new TqRen(this.vldt, this.ren) }

          // ---------------------------------------------------------------------------
          trait KeySelection extends KeyzSelection with KPathSelection with RenSelection {
            @finl def keys (c: Cls): Seq[Key] = key_(c).toSeq
            @finl def kpath(c: Cls): KPath    = KPath.from(key(c))

            @finl override def ren   (c: Cls): Ren    = Ren.from(key(c))
            @finl override def renz  (c: Cls): Renz   = super[RenSelection  ].renz(c)
            @finl override def rpathz(c: Cls): RPathz = this                 .kpathz(c).rpathz
            @finl override def kpaths(c: Cls): KPaths = super[KPathSelection].kpaths(c)

            @deprecated def key(c: Cls):         Key
                        def key_(c: Cls): Option[Key] = Some(key(c)) }

// ===========================================================================
