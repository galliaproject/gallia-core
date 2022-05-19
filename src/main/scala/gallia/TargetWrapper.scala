package gallia

// ===========================================================================
/** has explicit key + all but keys + step 1 of 2-step-rename */
case class KeyW(value: Key) {
    def skey         : SKey  = value.name
    def ren          : Ren   = Ren(value, value)
    def ren(to: KeyW): Ren   = Ren(value, to.value)
    def kpath        : KPath = KPath.from(value)

    def keyz = Keyz(Seq(value))
  }

  // ===========================================================================
  object KeyW {
    implicit def to(x:  Key ): KeyW = KeyW(x)
    implicit def to(x: SKey ): KeyW = KeyW(Symbol(x          ))
    implicit def to(x: UKey ): KeyW = KeyW(Symbol(x.entryName))
    implicit def to(x: EKey ): KeyW = KeyW(Symbol(x.toString ))
  }

  // ===========================================================================
  case class KeyWPair(value: (Key, Key)) // for eg swapEntries

    // ---------------------------------------------------------------------------
    object KeyWPair {
      private implicit def _to(x: SKey): Key = Symbol(x)
      private implicit def _to(x: UKey): Key = Symbol(x.entryName)
      private implicit def _to(x: EKey): Key = Symbol(x.toString)

      // ---------------------------------------------------------------------------
      implicit def _toKK(x: ( Key,   Key)): KeyWPair = KeyWPair(x._1, x._2)
      implicit def _toKS(x: ( Key,  SKey)): KeyWPair = KeyWPair(x._1, x._2)
      implicit def _toKU(x: ( Key,  UKey)): KeyWPair = KeyWPair(x._1, x._2)
      implicit def _toKE(x: ( Key,  EKey)): KeyWPair = KeyWPair(x._1, x._2)

      implicit def _toSK(x: (SKey,   Key)): KeyWPair = KeyWPair(x._1, x._2)
      implicit def _toSS(x: (SKey,  SKey)): KeyWPair = KeyWPair(x._1, x._2)
      implicit def _toSU(x: (SKey,  UKey)): KeyWPair = KeyWPair(x._1, x._2)
      implicit def _toSE(x: (SKey,  EKey)): KeyWPair = KeyWPair(x._1, x._2)

      implicit def _toUK(x: (UKey,   Key)): KeyWPair = KeyWPair(x._1, x._2)
      implicit def _toUS(x: (UKey,  SKey)): KeyWPair = KeyWPair(x._1, x._2)
      implicit def _toUU(x: (UKey,  UKey)): KeyWPair = KeyWPair(x._1, x._2)
      implicit def _toUE(x: (UKey,  EKey)): KeyWPair = KeyWPair(x._1, x._2)

      implicit def _toEK(x: (EKey,   Key)): KeyWPair = KeyWPair(x._1, x._2)
      implicit def _toES(x: (EKey,  SKey)): KeyWPair = KeyWPair(x._1, x._2)
      implicit def _toEU(x: (EKey,  UKey)): KeyWPair = KeyWPair(x._1, x._2)
      implicit def _toEE(x: (EKey,  EKey)): KeyWPair = KeyWPair(x._1, x._2)
    }

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
