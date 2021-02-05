package gallia.selection.typed

import gallia._
import gallia.selection.untyped.{UtsBoilerplate => Untyped}

// ===========================================================================
object TsOps { // this is a big mess... TODO: t210107203932
  import fluency._

  // ---------------------------------------------------------------------------
  object    RemoveIf
    extends RemoveIf
    trait   RemoveIf // Ren/RPath may not be a good idea though
        extends TsSingleBundles.HasSingleRequiredBasic[RPathW]

        with    TsRepeatedIndividuals.HasRepeatedOneString[RPathW, RPathWz]
        with    TsRepeatedIndividuals.HasRepeatedNesString[RPathW, RPathWz]
        with    TsRepeatedIndividuals.HasRepeatedXString  [RPathW, RPathWz]

        with    TsByTypeBundles.HasAllOneKeys
        with    TsByTypeBundles.HasAllOnePaths

        with    Untyped.RemoveIf.Origin  // firstKey, allKeys, ...
        with    Untyped.RemoveIf.HasSels // for e.g. .removeIfValueFor(_.string(_.allKeys)).is("foo") - FIXME: issues if optional...

  // ===========================================================================
  object    Transform
    extends Transform
    trait   Transform
        // notes: exclude Whatever?
        extends TsSingleBundles.HasSingleRequiredBasic[RPathW]
        with    TsSingleBundles.HasSingleOptionalBasic[RPathW] /* after much hesitation, leave access to optionality here; maybe create a special action for simplest case, like "morph" for "transmute" (arbitrarily) - t210202085958 */
        with    TsSingleBundles.HasSingleXBasic[RPathW]

        with    TsSingleBundles.HasSingleRequiredTyped[KeyW]

        with    Untyped.Transform.Origin
      //with    Untyped.Transform.HasOneSels

        with    TsNestingIndividuals.HasOneObj [RPathW]
        with    TsNestingIndividuals.HasOneObjz[RPathW]

        with    TsRepeatedIndividuals.HasRepeatedOneString[RPathW, RPathWz]
        //with HasXTypedSel[RPathW]

  // ===========================================================================
  trait GenerateBase
        extends TsSingleBundles.HasSingleRequiredBasic[KPathW]
        with    TsSingleBundles.HasSingleOptionalBasic[KPathW]
        with    TsSingleBundles.HasSingleTyped[KeyW]

        with    Untyped.CommonTyped.Origin
        with    Untyped.CommonTyped.HasOneSels //TODO: opt

    // ---------------------------------------------------------------------------
    object    Generate1
      extends Generate1
      trait   Generate1
          extends GenerateBase

          with TsNestingIndividuals.HasOneObj [KPathW]
          with TsNestingIndividuals.HasOneObjz[KPathW]

    // ---------------------------------------------------------------------------
    object    Generate2
      extends Generate2
      trait   Generate2
          extends GenerateBase

  // ===========================================================================
  object    FilterByT
    extends FilterByT
    trait   FilterByT // excludes obj(z) for T > 1
          extends TsSingleBundles.HasSingleRequiredBasic [RPathW]
          with    TsSingleBundles.HasSingleOptionalBasic [RPathW]
          with    TsSingleBundles.HasSingleXBasic[RPathW]

          with    TsSingleBundles.HasSingleTyped[KeyW]

          with    Untyped.Transform.Origin
          with    Untyped.Transform.HasOneSels

          with    TsRepeatedIndividuals.HasRepeatedOneString[RPathW, RPathWz] // TODO: more + bundle

    // ---------------------------------------------------------------------------
    object    FilterBy1
      extends FilterBy1
      trait   FilterBy1
          extends FilterByT
          with    TsNestingIndividuals.HasOneObj[RPathW]
          with    TsNestingIndividuals.HasOneObjz[RPathW]

  // ===========================================================================
  object    Cotransform
    extends Cotransform
    trait   Cotransform
        extends TsSingleBundles   .HasSingleOneBasic[KPathW]
        with    TsSingleIndividual.HasSingleOptString[KPathW]

  // ===========================================================================
  protected trait _Typed
      extends TsSingleBundles.HasSingleTyped[KeyW]

      with    TsNestingIndividuals.HasOneObj [KPathW]
      with    TsNestingIndividuals.HasOneObjz[KPathW]

      with    Untyped.CommonTyped.Origin
      with    Untyped.CommonTyped.HasOneSels // for eg fuse(_.string(_.firstKey), ...); TODO: exclude opt?

    // ---------------------------------------------------------------------------
    // TODO: customise
    object FuseFission extends FuseFission; trait FuseFission extends _Typed with TsSingleBundles.HasSingleRequiredBasic[KPathW] with TsSingleBundles.HasSingleOptionalBasic[KPathW]
    object AssertData  extends AssertData ; trait AssertData  extends _Typed with TsSingleBundles.HasSingleRequiredBasic[KPathW] with TsSingleBundles.HasSingleOptionalBasic[KPathW]
    object Sorting     extends Sorting    ; trait Sorting     extends _Typed with TsSingleBundles.HasSingleRequiredBasic[KPathW] with TsSingleBundles.HasSingleOptionalBasic[KPathW]
    object Squash      extends Squash     ; trait Squash      extends _Typed with TsSingleBundles.HasSingleRequiredBasic[KPathW] with TsSingleBundles.HasSingleOptionalBasic[KPathW]
      //pivoting: must prevent opt for pivot (c200930125015)

}

// ===========================================================================

