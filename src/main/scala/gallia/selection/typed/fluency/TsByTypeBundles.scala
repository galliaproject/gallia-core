package gallia.selection.typed.fluency

// ===========================================================================
@gallia.TypeMatching
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
          extends  TsByTypeIndividuals.HasAllOneStringKeys
          with     TsByTypeIndividuals.HasAllOneIntKeys
          with     TsByTypeIndividuals.HasAllOneDoubleKeys
          with     TsByTypeIndividuals.HasAllOneBooleanKeys

      trait HasAllOptKeys
          extends TsByTypeIndividuals.HasAllOptStringKeys
          with    TsByTypeIndividuals.HasAllOptIntKeys
          with    TsByTypeIndividuals.HasAllOptDoubleKeys
          with    TsByTypeIndividuals.HasAllOptBooleanKeys

    // ---------------------------------------------------------------------------
    trait HasAllOnePaths
          extends TsByTypeIndividuals.HasAllOneStringPaths
          with    TsByTypeIndividuals.HasAllOneIntPaths
          with    TsByTypeIndividuals.HasAllOneDoublePaths
          with    TsByTypeIndividuals.HasAllOneBooleanPaths

      trait HasAllOptPaths
          extends  TsByTypeIndividuals.HasAllOptStringPaths
          with     TsByTypeIndividuals.HasAllOptIntPaths
          with     TsByTypeIndividuals.HasAllOptDoublePaths
          with     TsByTypeIndividuals.HasAllOptBooleanPaths
}

// ===========================================================================
