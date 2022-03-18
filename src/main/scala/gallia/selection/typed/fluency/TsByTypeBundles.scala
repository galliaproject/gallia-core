package gallia
package selection.typed.fluency

// ===========================================================================
@TypeMatching
private[gallia] object TsByTypeBundles {

  trait HasAllTypedKeys
        extends TsByTypeBundles.HasAllOneKeys
        with    TsByTypeBundles.HasAllOptKeys

    // ---------------------------------------------------------------------------
    trait HasAllTypedPaths
        extends TsByTypeBundles.HasAllOnePaths
        with    TsByTypeBundles.HasAllOptPaths

    // ===========================================================================
    trait HasAllOneKeys
          extends  TsByTypeIndividual.HasAllOneStringKeys
          with     TsByTypeIndividual.HasAllOneIntKeys
          with     TsByTypeIndividual.HasAllOneDoubleKeys
          with     TsByTypeIndividual.HasAllOneBooleanKeys

      trait HasAllOptKeys
          extends TsByTypeIndividual.HasAllOptStringKeys
          with    TsByTypeIndividual.HasAllOptIntKeys
          with    TsByTypeIndividual.HasAllOptDoubleKeys
          with    TsByTypeIndividual.HasAllOptBooleanKeys

    // ---------------------------------------------------------------------------
    trait HasAllOnePaths
          extends TsByTypeIndividual.HasAllOneStringPaths
          with    TsByTypeIndividual.HasAllOneIntPaths
          with    TsByTypeIndividual.HasAllOneDoublePaths
          with    TsByTypeIndividual.HasAllOneBooleanPaths

      trait HasAllOptPaths
          extends  TsByTypeIndividual.HasAllOptStringPaths
          with     TsByTypeIndividual.HasAllOptIntPaths
          with     TsByTypeIndividual.HasAllOptDoublePaths
          with     TsByTypeIndividual.HasAllOptBooleanPaths
}

// ===========================================================================
