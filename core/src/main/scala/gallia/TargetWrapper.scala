package gallia

// ===========================================================================
/** has explicit key + all but keys + step 1 of 2-step-rename */
case class KeyW(value: Key) {
    def skey         : SKey  = value.name
    def ren          : Ren   = Ren(value, value)
    def ren(to: KeyW): Ren   = Ren(value, to.value)
    def kpath        : KPath = KPath.from(value)

    def keyz = Keyz(Seq(value)) }

  // ===========================================================================
  object KeyW {
    implicit def to(x:  Key ): KeyW = KeyW(x)
    implicit def to(x: SKey ): KeyW = KeyW(Symbol(x          ))
    implicit def to(x: UKey ): KeyW = KeyW(Symbol(x.entryName))
    implicit def to(x: EKey ): KeyW = KeyW(Symbol(x.toString )) }

// ===========================================================================
case class GenericEntry[T](value: (Key, T)) // for eg swapEntries (Key, Key), aggregateBy (Key, HeadV[T])

  // ---------------------------------------------------------------------------
  object GenericEntry {
    private implicit def _to(x: SKey): Key = Symbol(x)
    private implicit def _to(x: UKey): Key = Symbol(x.entryName)
    private implicit def _to(x: EKey): Key = Symbol(x.toString)

    // ---------------------------------------------------------------------------
    implicit def _toKK[T](x: ( Key, T)): GenericEntry[T] = GenericEntry(       x._1,            x._2)
    implicit def _toSK[T](x: (SKey, T)): GenericEntry[T] = GenericEntry(Symbol(x._1),           x._2)
    implicit def _toUK[T](x: (UKey, T)): GenericEntry[T] = GenericEntry(Symbol(x._1.entryName), x._2)
    implicit def _toEK[T](x: (EKey, T)): GenericEntry[T] = GenericEntry(Symbol(x._1.toString),  x._2) }

  // ---------------------------------------------------------------------------
  trait GenericEntryImplicits {
    private type P =       (SKey, HeadV[_])
    private type E = GenericEntry[HeadV[_]]

    // ---------------------------------------------------------------------------
    implicit def _genericEntry1(_f: HeadZ =>  P)             : HeadZ =>  E              = x => { val  (k1, v1)                                          = _f(x);  (k1, v1) }
    implicit def _genericEntry2(_f: HeadZ => (P, P))         : HeadZ => (E, E)          = x => { val ((k1, v1), (k2, v2))                               = _f(x); ((k1, v1), (k2, v2)) }
    implicit def _genericEntry3(_f: HeadZ => (P, P, P))      : HeadZ => (E, E, E)       = x => { val ((k1, v1), (k2, v2), (k3, v3))                     = _f(x); ((k1, v1), (k2, v2), (k3, v3)) }
    implicit def _genericEntry4(_f: HeadZ => (P, P, P, P))   : HeadZ => (E, E, E, E)    = x => { val ((k1, v1), (k2, v2), (k3, v3), (k4, v4))           = _f(x); ((k1, v1), (k2, v2), (k3, v3), (k4, v4)) }
    implicit def _genericEntry5(_f: HeadZ => (P, P, P, P, P)): HeadZ => (E, E, E, E, E) = x => { val ((k1, v1), (k2, v2), (k3, v3), (k4, v4), (k5, v5)) = _f(x); ((k1, v1), (k2, v2), (k3, v3), (k4, v4), (k5, v5)) } }

// ===========================================================================
/** for has explicit RKey(s) */
case class RenW(value: Ren) {
    @inline def ren = value

    def rpath: RPath = value.rpath

    def renz = Renz(Seq(value))

    def from = value.from
    def to   = value.to
  }

  // ---------------------------------------------------------------------------
  object RenW {
    implicit def to(key1:  Key ): RenW = RenW(Ren.from(key1))
    implicit def to(key1: SKey ): RenW = RenW(Ren.from(key1))
    implicit def to(key1: EKey ): RenW = RenW(Ren.from(key1))
    implicit def to(key1: UKey ): RenW = RenW(Ren.from(key1))

    implicit def to(key1: Ren  ): RenW = RenW(key1)
  }

// ===========================================================================
/** remove + step 2 of 2-step-rename, generate origin */
case class KPathW(value: KPath) {
    @inline def kpath = value
            def rpath(newKey: KeyW): RPath = value.rpath(newKey)

    def  key = value. key
    def skey = value.skey

    def kpathz = KPathz(Seq(value))
    def rpathz = RPathz(Seq(value.rpath))
  }

  // ---------------------------------------------------------------------------
  object KPathW {
    implicit def to(x:  Key ): KPathW = KPathW(KPath.from(x))
    implicit def to(x: SKey ): KPathW = KPathW(KPath.from(x))
    implicit def to(x: EKey ): KPathW = KPathW(KPath.from(x))
    implicit def to(x: UKey ): KPathW = KPathW(KPath.from(x))

    implicit def to(x: KPath): KPathW = KPathW(x)
  }

// ===========================================================================
case class RPathW(value: RPath) {
    @inline def rpath  = value
            def rpathz = RPathz(Seq(value))
  }

  // ---------------------------------------------------------------------------
  object RPathW {
    implicit def to(x:  Key ): RPathW = RPathW(RPath.from(x))
    implicit def to(x: SKey ): RPathW = RPathW(RPath.from(x))
    implicit def to(x: EKey ): RPathW = RPathW(RPath.from(x))
    implicit def to(x: UKey ): RPathW = RPathW(RPath.from(x))

    implicit def to(x: Ren  ): RPathW = RPathW(x.rpath)

    implicit def to(x: KPath): RPathW = RPathW(x.rpath)
    implicit def to(x: RPath): RPathW = RPathW(x)
  }

// ===========================================================================
/** mostly for 1-step rename */
case class ActualRPathW(value: RPath /* actual */) { def rpathz: RPathz = RPathz(Seq(value)) }

  // ---------------------------------------------------------------------------
  object ActualRPathW {
    implicit def to(x: Ren  ): ActualRPathW = ActualRPathW(x.rpath)
    implicit def to(x: RPath): ActualRPathW = ActualRPathW(x)
  }

// ===========================================================================
