package gallia
package selection.untyped.fluency

import meta.ClsLike
import selection.untyped.processors._
import selection.untyped.processors.UtsProcessors._

// ===========================================================================
object UtsIndividuals { // mostly meant to convey intent

  // explicit
  trait HasSingleExplicitKeyW   { def explicit(x: KeyW)  : KeySelection   = ExplicitKey  (x.value) }
  trait HasSingleExplicitRenW   { def explicit(x: RenW)  : RenSelection   = ExplicitRen  (x.value) }
  trait HasSingleExplicitKPathW { def explicit(x: KPathW): KPathSelection = ExplicitKPath(x.value) }
  trait HasSingleExplicitRPathW { def explicit(x: RPathW): RPathSelection = ExplicitRPath(x.value) }

  // ---------------------------------------------------------------------------
  trait HasSingleExplicitIndex { def index(value : MIndex): KeySelection = ExplicitIndex(value, None) } // TODO: consider a 1.1.1 or 1/1/1 notation for path?

  // ---------------------------------------------------------------------------
  @deprecated  ("t210113162449 - phase out") trait _HasSingleExplicitKPathW {
    @deprecated("t210113162449 - phase out") private[gallia] def _explicit(x: KPathW): KPathSelection = ExplicitKPath(x.value) }

  // ===========================================================================
  trait HasRepeatedExplicitKeyW {
      @deprecated def explicitFX(x1: KeyW, more: Seq[KeyW]): KeyzSelection = explicit(KeyWz(Seq(x1) ++ more)) // FIXME

      def explicit(x1: KeyW, x2: KeyW, more: KeyW*): KeyzSelection = explicit(KeyWz(Seq(x1, x2) ++ more))
      def explicit(x : KeyWz)                      : KeyzSelection = ExplicitKeyz(x.keyz) }

    trait HasRepeatedExplicitRenW {
      @deprecated def explicitFX(x1: RenW, more: Seq[RenW]): RenzSelection = explicit(RenWz(Seq(x1) ++ more))

      def explicit(x1: RenW, x2: RenW, more: RenW*): RenzSelection = explicit(RenWz(Seq(x1, x2) ++ more))
      def explicit(x : RenWz)                      : RenzSelection = ExplicitRenz(x.renz) }

    trait HasRepeatedExplicitKPathW {
      def explicit(x1: KPathW, x2: KPathW, more: KPathW*): KPathzSelection = explicit(KPathWz(Seq(x1, x2) ++ more))
      def explicit(x : KPathWz)                          : KPathzSelection = ExplicitKPathz(x.kpathz) }

    trait HasRepeatedExplicitRPathW {
      def explicit(x1: RPathW, x2: RPathW, more: RPathW*): RPathzSelection = explicit(RPathWz(Seq(x1, x2) ++ more))
      def explicit(x : RPathWz)                          : RPathzSelection = ExplicitRPathz(x.rpathz) }

    trait HasRepeatedExplicitIndex {
      def indices(value1: MIndex, value2: MIndex, more: MIndex*): KeyzSelection = ExplicitIndices(Seq(value1, value2) ++ more)
      def indices(values: Seq[MIndex]                          ): KeyzSelection = ExplicitIndices(values) }

  // ===========================================================================
  // key

  trait NoSoleKey // 201123101449

  trait HasSoleKey         { def soleKey   : KeySelection = SoleKey }

  trait HasFirstKey        { def firstKey  : KeySelection = ExplicitIndex( 0, Some(ExplicitIndex.Special.First)) }
  trait HasSecondKey       { def secondKey : KeySelection = ExplicitIndex( 1, Some(ExplicitIndex.Special.Second)) }
  trait HasThirdKey        { def thirdKey  : KeySelection = ExplicitIndex( 2, Some(ExplicitIndex.Special.Third)) }
  trait HasLastKey         { def lastKey   : KeySelection = ExplicitIndex(-1, Some(ExplicitIndex.Special.Last )) }

  trait HasSecondToLastKey { def secondToLastKey : KeySelection = ExplicitIndex(-2, Some(ExplicitIndex.Special.SecondToLast)) }

  // ---------------------------------------------------------------------------
  trait HasSimpleCustomKey   { def     customKey(f: Seq[SKey] => SKey): KeySelection = new CustomSimpleKeyFunction    (f) }
  trait HasAdvancedCustomKey { def fullCustomKey(f: ClsLike   =>  Key): KeySelection = new CustomAdvancedToKeyFunction(f) }

  // ===========================================================================
  // keyz

  trait NoAllKeys // 201123101450

  // ---------------------------------------------------------------------------
  trait HasAllKeys     { def allKeys : KeyzSelection = AllKeys }
  trait HasInitKeys    { def initKeys: KeyzSelection = new CustomSimpleKeysFunction(_.init) } //TODO: dedicated?
  trait HasTailKeys    { def tailKeys: KeyzSelection = new CustomSimpleKeysFunction(_.tail) } //TODO: dedicated?

  // ===========================================================================
  trait HasSKeyMatches1 { def     findKey(pred: SKey  => Boolean): KeySelection    = KeyPredicate1(pred) }
  trait HasSKeyMatchesN { def  filterKeys(pred: SKey  => Boolean): KeyzSelection   = KeyPredicateN(pred) }
  trait HasPathMatches1 { def    findPath(pred: KPath => Boolean): KPathSelection  = PathPredicate1(pred) }
  trait HasPathMatchesN { def filterPaths(pred: KPath => Boolean): KPathzSelection = PathPredicateN(pred) }

  // ---------------------------------------------------------------------------
  trait HasAllButKeys {
    def allBut(w: KeyW )                       : KeyzSelection = AllButKeys(w.keyz)
    def allBut(w: KeyWz)                       : KeyzSelection = AllButKeys(w.keyz)
    def allBut(w1: KeyW, w2: KeyW, more: KeyW*): KeyzSelection = allBut(KeyWz(Seq(w1, w2) ++ more)) }

  // ---------------------------------------------------------------------------
  trait HasAllButIndices {
    def allBut(value1: MIndex, more: MIndex*): KeyzSelection = AllButIndices(value1 +: more)

    def allButFirst = allBut( 0)
    def allButLast  = allBut(-1) }

  // ---------------------------------------------------------------------------
  @PartialTypeMatching
  trait HasIfType {
    def ifType[T: WTT ]: KeyzSelection = new IfType[T]

    // ---------------------------------------------------------------------------
    // TODO: t210113114451 - these should also act as .string(...) since they can't be anything else
    final def ifString : KeyzSelection = ifType[String ]
    final def ifInt    : KeyzSelection = ifType[Int    ]
    final def ifDouble : KeyzSelection = ifType[Double ]
    final def ifBoolean: KeyzSelection = ifType[Boolean]

    // especially useful for setting default values
    final def ifOptionalString : KeyzSelection = ifType[Option[String ]]
    final def ifOptionalInt    : KeyzSelection = ifType[Option[Int    ]]
    final def ifOptionalDouble : KeyzSelection = ifType[Option[Double ]]
    final def ifOptionalBoolean: KeyzSelection = ifType[Option[Boolean]]
  }

  // ---------------------------------------------------------------------------
  trait HasSimpleCustomKeys   { def     customKeys(f: Seq[SKey] => Seq[SKey]): KeyzSelection = new CustomSimpleKeysFunction    (f) }
  trait HasAdvancedCustomKeys { def fullCustomKeys(f: ClsLike   => Seq[ Key]): KeyzSelection = new CustomAdvancedToKeysFunction(f) }

  // ===========================================================================
  // paths

  trait HasLeafPaths { def leafPaths: KPathzSelection = LeafPaths }
  trait HasAllPaths  { def allPaths : KPathzSelection = AllPaths  }

  // ---------------------------------------------------------------------------
  trait HasSimpleCustomLeafPaths { def     customLeafPaths(f: Seq[KPath] => Seq[KPath]): KPathzSelection = new CustomSimpleLeafPathsFunction(f) }
  trait HasSimpleCustomAllPaths  { def     customAllPaths (f: Seq[KPath] => Seq[KPath]): KPathzSelection = new CustomSimpleAllPathsFunction (f) }
  trait HasAdvancedCustomPaths   { def fullCustomPaths    (f: ClsLike    => Seq[KPath]): KPathzSelection = new CustomAdvancedToPathsFunction(f) }

  // ---------------------------------------------------------------------------
  @PartialTypeMatching
  trait HasIfTypeRecursively {
    def ifTypeRecursively[T: WTT]: KPathzSelection = new IfTypeRecursively[T]

    // ---------------------------------------------------------------------------
    // TODO: t210113114451 - these should also act as .string(...) since they can't be anything else
    final def ifStringRecursively  = ifTypeRecursively[String ]
    final def ifIntRecursively     = ifTypeRecursively[Int    ]
    final def ifDoubleRecursively  = ifTypeRecursively[Double ]
    final def ifBooleanRecursively = ifTypeRecursively[Boolean]

    // especially useful for setting default values
    final def ifOptionalStringRecursively : KPathzSelection = ifTypeRecursively[Option[String ]]
    final def ifOptionalIntRecursively    : KPathzSelection = ifTypeRecursively[Option[Int    ]]
    final def ifOptionalDoubleRecursively : KPathzSelection = ifTypeRecursively[Option[Double ]]
    final def ifOptionalBooleanRecursively: KPathzSelection = ifTypeRecursively[Option[Boolean]]
  }

}

// ===========================================================================
