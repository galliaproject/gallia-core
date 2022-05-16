package gallia
package selection.untyped

// ===========================================================================
/** must repeat for object else error "class type required but X with Y found" (TODO:?) */
object UtsOps { // this is a big mess... FIXME: t210107203932
  import fluency.UtsIndividuals._
  import fluency.UtsBundle._
  val Bundle = fluency.UtsBundle

  // ===========================================================================
  trait  ForKey      extends Core1 with HasSoleKey                        with HasSingleExplicitKeyW
  trait  ForPath     extends Core1 with HasSoleKey                        with HasSingleExplicitKPathW

  //TODO: allow 1?
  trait  ForEachKey  extends CoreN                 with        HasAllKeys with HasRepeatedExplicitKeyW
  trait  ForEachPath extends CoreN                 with Bundle.HasAllAlls with HasRepeatedExplicitKPathW

  // ===========================================================================
  trait  CommonTyped extends Core1 with NoSoleKey with _HasSingleExplicitKPathW

  // ---------------------------------------------------------------------------
  trait  ReorderAsX          extends Core1N with NoSoleKey  with NoAllKeys with CantRenameKeys

  // ---------------------------------------------------------------------------
  trait  ModifyEnumValuesFor extends Core1N with HasSoleKey with HasAllKeys with CanRenamePaths

// ---------------------------------------------------------------------------
  trait  Rename extends Core1 with HasSoleKey with HasSingleExplicitKPathW

  // ---------------------------------------------------------------------------
  trait  Remove extends Core1N with NoSoleKey with NoAllKeys with CantRenamePaths
  trait  Retain extends Core1N with NoSoleKey with NoAllKeys with  CanRenamePaths

  // ===========================================================================
  trait  SetDefault extends Core1N with NoSoleKey with HasIfType

  trait  Translate  extends Core1N with HasSoleKey with Bundle.HasAllAlls with Bundle.HasExplicitKPathW
  trait  Convert    extends Core1N with HasSoleKey with Bundle.HasAllAlls with Bundle.HasExplicitKPathW

  trait  Nest        extends Core1N with HasSoleKey with HasAllKeys with Bundle.HasExplicitRenW // TODO: must distinguish under and into for sole/all keys
  trait  Renesting   extends CoreN with NoSoleKey  with HasAllKeys with Bundle.HasExplicitKeyW
  trait  UnnestFrom  extends CoreN with HasSoleKey with HasAllKeys with Bundle.HasExplicitRenW

  trait  Custom     extends Core1N with HasSoleKey with Bundle.HasAllAlls with Bundle.HasExplicitKPathW

  // ===========================================================================
  trait  RemoveIf       extends Core1N with NoSoleKey  with Bundle.HasAllAlls /* All* ok because conditional */
  trait  RemoveValueFor extends Core1  with HasSoleKey with CantRenameKey
  trait  SetDefaultFor  extends Core1  with HasSoleKey with CantRenameKey

  trait  Transform extends Core1N with HasSoleKey with Bundle.HasAllAlls with _HasSingleExplicitKPathW // ...; UtsBundle.Indices
  trait  FilterBy  extends Core1  with HasSoleKey                        with _HasSingleExplicitKPathW

  // ===========================================================================
  trait Reducing extends Core1N with HasSoleKey with HasAllKeys with HasRepeatedExplicitKeyW

  // ---------------------------------------------------------------------------
  trait SortingSingle   extends Core1  with HasSoleKey                 with HasExplicitKPathW
  trait SortingMultiple extends Core1N with HasSoleKey with HasAllAlls with HasExplicitKPathW

  // ---------------------------------------------------------------------------
  trait Groupee1        extends Core1K with  NoSoleKey                  with HasSingleExplicitRenW
  trait GroupeeN        extends Core1N with  NoSoleKey with  NoAllKeys  with HasRepeatedExplicitRenW
  trait Groupers        extends Core1N with  NoSoleKey with  NoAllKeys  with HasSingleExplicitRenW with HasRepeatedExplicitRenW

  // ---------------------------------------------------------------------------
  trait Merging  extends Core1K with HasSoleKey with HasAllKeys with CantRenameKeys
}

// ===========================================================================
