package gallia
package selection.typed

import selection.untyped.{UtsBoilerplate => Untyped}

// ===========================================================================
object TsOps { // this is a big mess... TODO: t210107203932
  import fluency._

  // ---------------------------------------------------------------------------
  object    RemoveIf
    extends RemoveIf
    trait   RemoveIf extends // Ren/RPath may not be a good idea though
             TsBundles.HasRequiredBasic[RPathW, RPathWz]

        with TsByTypeBundles.HasAllOneKeys
        with TsByTypeBundles.HasAllOnePaths

        with Untyped.RemoveIf.Origin      // firstKey, allKeys, ...
        with Untyped.RemoveIf.HasSelBasic // for e.g. .removeIfValueFor(_.string(_.allKeys)).is("foo") - FIXME: issues if optional...

  // ===========================================================================
  object    Transform
    extends Transform
    trait   Transform extends // notes: exclude Whatever?
             Untyped.Transform.Origin
        with _Typed[RPathW]
        with TsSingleBundles  .HasSingleBasic  [RPathW] /* after much hesitation, leave access to optionality here; maybe create a special action for simplest case, like "morph" for "transmute" (arbitrarily) - t210202085958 */
        with TsRepeatedBundles.HasRepeatedBasic[RPathW, RPathWz]

  // ===========================================================================
  trait GenerateBase extends 
             TsSingleBundles.HasSingleBasic[KPathW]
      //with TsSingleBundles.HasSingleTyped[KeyW]
        with Untyped.CommonTyped.Origin
        with Untyped.CommonTyped.HasSelBasic

    // ---------------------------------------------------------------------------
    object    Generate1
      extends Generate1
      trait   Generate1 extends 
               GenerateBase
          with TsSingleBundles.HasSingleNesting[KPathW]

    // ---------------------------------------------------------------------------
    object    Generate2
      extends Generate2
      trait   Generate2 extends 
           GenerateBase

  // ===========================================================================
  object    FilterByT
    extends FilterByT
    trait   FilterByT extends // excludes obj(z) for T > 1
               TsSingleBundles.HasSingleBasic[RPathW]
          with TsSingleBundles.HasSingleXBasic[RPathW]

          //with TsSingleBundles.HasSingleTyped[KeyW]

          with Untyped.Transform.Origin
          with Untyped.Transform.HasOneSelBasic

          with TsRepeatedIndividual.HasRepeatedOneString[RPathW, RPathWz] // TODO: more + bundle

    // ---------------------------------------------------------------------------
    object    FilterBy1
      extends FilterBy1
      trait   FilterBy1 extends 
               FilterByT
          with TsSingleBundles.HasSingleNesting[RPathW]

  // ===========================================================================
  object    Cotransform
    extends Cotransform
    trait   Cotransform extends 
             TsSingleBundles   .HasSingleOneBasic [KPathW]
        with TsSingleIndividual.HasSingleOptString[KPathW]

  // ===========================================================================
  protected trait _Typed[$Wrap] extends 
           TsSingleBundles.HasSingleBasic  [$Wrap] // no support for Repeated
      with TsSingleBundles.HasSingleNesting[$Wrap]

      with Untyped.CommonTyped.Origin
      with Untyped.CommonTyped.HasOneSelBasic // for eg fuse(_.string(_.firstKey), ...); TODO: exclude opt?

    // ---------------------------------------------------------------------------
    // TODO: customise
    object FuseFission extends FuseFission; trait FuseFission extends _Typed[KPathW]
    object AssertData  extends AssertData ; trait AssertData  extends _Typed[KPathW]
    object Sorting     extends Sorting    ; trait Sorting     extends _Typed[KPathW]
    object Squash      extends Squash     ; trait Squash      extends _Typed[KPathW]
      //pivoting: must prevent opt for pivot (c200930125015)

}

// ===========================================================================

