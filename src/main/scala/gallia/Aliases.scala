package gallia

// ===========================================================================
trait Aliases {

  private[gallia] type DI     = DummyImplicit

  private[gallia] type Vle = Any // as in HeadV's during data phase

    // two versions:
    // - t210104164037 - ListMap[Key, AnyValue] if standalone
    // - t210104164036 - Vector[List[Any]] if has schema
    //   - t210110095423 - the outer part (Vector) - rely on Cls' fields order synchrony
    //     - t210121095207 - also look into scala-native
    //   - t210110095424 - the inner part (List) - (+ performance cost of having the List?)
    //   - t210110095425 - the Any part: consider an wrapping ADT (cost of wrapping vs pattern matching on Any)
    private[gallia] type UEntry = (Key, AnyValue) /* TODO: t210107094213 - ensure if multiple then use List */
    private[gallia] type UData  = Array[UEntry] // formerly: collection.immutable.ListMap[Key, AnyValue]

  // ---------------------------------------------------------------------------
  // until sure stick with them
  private[gallia] type      Coll[A] = collection.GenTraversable    [A]
  private[gallia] type SparkColl[A] = collection.   TraversableOnce[A]

  // ---------------------------------------------------------------------------
  private[gallia] type ClsName    =        String
  private[gallia] type ClsNameOpt = Option[String]

  // ---------------------------------------------------------------------------
  private[gallia] type UnionObjectDisambiguatorOpt = Option[UnionObjectDisambiguator]

  // ---------------------------------------------------------------------------
  private[gallia] type EnmName    =        String
  private[gallia] type EnmNameOpt = Option[String]
  private[gallia] type EnumStringValue = String

  // ---------------------------------------------------------------------------
  private[gallia] type Optional = Boolean
  private[gallia] type Required = Boolean

  private[gallia] type Multiple = Boolean
  private[gallia] type Single   = Boolean

  // ---------------------------------------------------------------------------
  private[gallia] val _Optional = true
  private[gallia] val _Multiple = true

  private[gallia] val _Required = false
  private[gallia] val _Single   = false

  // ---------------------------------------------------------------------------
  def formatOptional(value: Optional): String = if (value) "_Optional" else "_Required"
  def formatMultiple(value: Multiple): String = if (value) "_Multiple" else "_Single"

  // ===========================================================================
  type Regex   = scala.util.matching.Regex
  type Pattern = java.util.regex.Pattern

  type UFunction = HeadU => HeadU
  type ZFunction = HeadZ => HeadZ

  // ---------------------------------------------------------------------------
  type  Key = Symbol
  type SKey = String
  type EKey = Enumeration#Value    // dotty will hopefully help with this - TODO: t220921125205 - phase out support for EKey
  type UKey = enumeratum.EnumEntry // dotty will hopefully help with this
}

// ===========================================================================
trait DomainAliases {
  type KPath  = gallia.KPath
  type Keyz   = gallia.Keyz
  val  Keyz   = gallia.Keyz

  type KeyW   = gallia.KeyW
  type KPathW = gallia.KPathW

  type KeyWz  = gallia.KeyWz

  type ActualRen = gallia.ActualRen
  val  ActualRen = gallia.ActualRen
}

// ===========================================================================
