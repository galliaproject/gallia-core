package gallia

// ===========================================================================
protected trait TargetWrappers[$Wrap] extends Seq[$Wrap] {
  val values: Seq[$Wrap]

  // ---------------------------------------------------------------------------
  def iterator       : Iterator[$Wrap] = values.iterator
  def apply(idx: Int): $Wrap           = values(idx)
  def length         : Int             = values.size
}

// ===========================================================================
// TODO: use Iterable instead? mostly for Map.values; for mixed (eg Key/SKey, must use explicit type tag)
case class KeyWz  (values: Seq[KeyW])   extends TargetWrappers[KeyW]   { def keyz   = Keyz  (values.map(_.value)); def keys = keyz.values }
case class KPathWz(values: Seq[KPathW]) extends TargetWrappers[KPathW] { def kpathz = KPathz(values.map(_.value)) }
case class RenWz  (values: Seq[RenW])   extends TargetWrappers[RenW]   {
  def renz  : Renz = Renz(values.map(_.value))
  def fromz : Keyz = Keyz(values.map(_.from))
  def toz   : Keyz = Keyz(values.flatMap(_.value.toOpt  ))
}

case class       RPathWz(values: Seq[RPathW])       extends TargetWrappers[RPathW] { def qpathz = RPathz(values.map(_.value)) }
case class ActualRPathWz(values: Seq[ActualRPathW]) extends TargetWrappers[ActualRPathW] { def qpathz = RPathz(values.map(_.value)) }

// ===========================================================================
object KeyWz {
    def from(value: KeyW) = KeyWz(Seq(value))

      // ---------------------------------------------------------------------------
      implicit def _to(tuple: (KeyW,       Seq[KeyW])): KeyWz = KeyWz(Seq(tuple._1          ) ++ tuple._2)
      implicit def _to(tuple: (KeyW, KeyW, Seq[KeyW])): KeyWz = KeyWz(Seq(tuple._1, tuple._2) ++ tuple._3)

      implicit def _to(values: Iterable[ Key ])(implicit di1: DI                           ): KeyWz = KeyWz(values.toSeq.map(KeyW.to))
      implicit def _to(values: Iterable[SKey ])(implicit di1: DI, di2: DI                  ): KeyWz = KeyWz(values.toSeq.map(KeyW.to))
      implicit def _to(values: Iterable[UKey ])(implicit di1: DI, di2: DI, di3: DI         ): KeyWz = KeyWz(values.toSeq.map(KeyW.to))
      implicit def _to(values: Iterable[EKey ])(implicit di1: DI, di2: DI, di3: DI, di4: DI): KeyWz = KeyWz(values.toSeq.map(KeyW.to))

      implicit def _to(values: Keyz): KeyWz = KeyWz(values.toSeq.map(KeyW.to))
  }

  // ===========================================================================
  object RenWz {

    def from(value: RenW) = RenWz(Seq(value))

      // ---------------------------------------------------------------------------
      implicit def _to(tuple: (RenW, RenW, Seq[RenW])): RenWz = RenWz(Seq(tuple._1, tuple._2) ++ tuple._3)

      implicit def _to(values: Iterable[ Key ])(implicit di1: DI                                    ): RenWz = RenWz(values.toSeq.map(RenW.to))
      implicit def _to(values: Iterable[SKey ])(implicit di1: DI, di2: DI                           ): RenWz = RenWz(values.toSeq.map(RenW.to))
      implicit def _to(values: Iterable[UKey ])(implicit di1: DI, di2: DI, di3: DI                  ): RenWz = RenWz(values.toSeq.map(RenW.to))
      implicit def _to(values: Iterable[EKey ])(implicit di1: DI, di2: DI, di3: DI, di4: DI         ): RenWz = RenWz(values.toSeq.map(RenW.to))

      implicit def _to(values: Iterable[Ren  ])(implicit di1: DI, di2: DI, di3: DI, di4: DI, di5: DI): RenWz = RenWz(values.toSeq.map(RenW.to))

      implicit def _to(values: Keyz): RenWz = RenWz(values.toSeq.map(RenW.to))
      implicit def _to(values: Renz): RenWz = RenWz(values.toSeq.map(RenW.to))
  }

  // ===========================================================================
  object KPathWz {
    def from(value: KPathW) = KPathWz(Seq(value))

      // ---------------------------------------------------------------------------
      implicit def _to(tuple: (KPathW        , Seq[KPathW])): KPathWz = KPathWz(Seq(tuple._1)           ++ tuple._2)
      implicit def _to(tuple: (KPathW, KPathW, Seq[KPathW])): KPathWz = KPathWz(Seq(tuple._1, tuple._2) ++ tuple._3)

      implicit def _to(values: Iterable[ Key ])(implicit di1: DI                                    ): KPathWz = KPathWz(values.toSeq.map(KPathW.to))
      implicit def _to(values: Iterable[SKey ])(implicit di1: DI, di2: DI                           ): KPathWz = KPathWz(values.toSeq.map(KPathW.to))
      implicit def _to(values: Iterable[UKey ])(implicit di1: DI, di2: DI, di3: DI                  ): KPathWz = KPathWz(values.toSeq.map(KPathW.to))
      implicit def _to(values: Iterable[EKey ])(implicit di1: DI, di2: DI, di3: DI, di4: DI         ): KPathWz = KPathWz(values.toSeq.map(KPathW.to))

      implicit def _to(values: Iterable[KPath])(implicit di1: DI, di2: DI, di3: DI, di4: DI, di5: DI): KPathWz = KPathWz(values.toSeq.map(KPathW.to))

      implicit def _to(values: Keyz)  : KPathWz = ???
      implicit def _to(values: Renz)  : KPathWz = ???
      implicit def _to(values: KPathz): KPathWz = KPathWz(values.values)
  }

  // ===========================================================================
  object RPathWz {
    def from(value: RPathW) = RPathWz(Seq(value))

      // ---------------------------------------------------------------------------
      implicit def _to(tuple: (RPathW        , Seq[RPathW])): RPathWz = RPathWz(Seq(tuple._1)           ++ tuple._2)
      implicit def _to(tuple: (RPathW, RPathW, Seq[RPathW])): RPathWz = RPathWz(Seq(tuple._1, tuple._2) ++ tuple._3)

      implicit def _to(values: Iterable[ Key ])(implicit di1: DI                                                      ): RPathWz = RPathWz(values.toSeq.map(RPathW.to))
      implicit def _to(values: Iterable[SKey ])(implicit di1: DI, di2: DI                                             ): RPathWz = RPathWz(values.toSeq.map(RPathW.to))
      implicit def _to(values: Iterable[UKey ])(implicit di1: DI, di2: DI, di3: DI                                    ): RPathWz = RPathWz(values.toSeq.map(RPathW.to))
      implicit def _to(values: Iterable[EKey ])(implicit di1: DI, di2: DI, di3: DI, di4: DI                           ): RPathWz = RPathWz(values.toSeq.map(RPathW.to))

      implicit def _to(values: Iterable[Ren  ])(implicit di1: DI, di2: DI, di3: DI, di4: DI, di5: DI                  ): RPathWz = RPathWz(values.toSeq.map(RPathW.to))
      implicit def _to(values: Iterable[KPath])(implicit di1: DI, di2: DI, di3: DI, di4: DI, di5: DI, di6: DI         ): RPathWz = RPathWz(values.toSeq.map(RPathW.to))
      implicit def _to(values: Iterable[RPath])(implicit di1: DI, di2: DI, di3: DI, di4: DI, di5: DI, di6: DI, di7: DI): RPathWz = RPathWz(values.toSeq.map(RPathW.to))

      implicit def _to(values: Keyz  ): RPathWz = RPathWz(values.values)
      implicit def _to(values: Renz  ): RPathWz = RPathWz(values.values)
      implicit def _to(values: KPathz): RPathWz = RPathWz(values.values)
      implicit def _to(values: RPathz): RPathWz = RPathWz(values.values)
  }

// ===========================================================================
