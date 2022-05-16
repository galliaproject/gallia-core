package gallia
package selection.untyped.fluency

// ===========================================================================
object UtsBundle {
  val Individual = UtsIndividuals
  val Bundle     = this

  // ===========================================================================
  trait Core1K
        extends Bundle    .OneIndex
        with    Bundle    .HasCustomKey
        with    Individual.HasSKeyMatches1

    // ---------------------------------------------------------------------------
  trait Core1 // sole/all/renaming must be handled explicitly
        extends Bundle    .Core1K
        with    Individual.HasPathMatches1

    // ===========================================================================
    trait CoreN // All* must be handled explicitly
        extends Bundle.RepeatedIndices

        // ---------------------------------------------------------------------------
        with    Individual.HasIfType
        with    Individual.HasIfTypeRecursively

        // ---------------------------------------------------------------------------
        with    Individual.HasInitKeys
        with    Individual.HasTailKeys

        with    Individual.HasSKeyMatchesN
        with    Individual.HasPathMatchesN

        with    Individual.HasAllButKeys

        // ---------------------------------------------------------------------------
        with    Bundle.HasCustomKeys
        with    Bundle.HasCustomPaths

  // ===========================================================================
  trait Core1N
      extends Bundle.Core1
      with    Bundle.CoreN

  // ---------------------------------------------------------------------------
  trait Core1R
      extends Bundle.Core1
      with    Individual.HasSingleExplicitRenW

  // ---------------------------------------------------------------------------
  trait CoreNR
      extends Bundle.CoreN
      with    Individual.HasRepeatedExplicitRenW

  // ===========================================================================
  trait HasCustomKey
      extends Individual.HasSimpleCustomKey
      with    Individual.HasAdvancedCustomKey

  // ---------------------------------------------------------------------------
  trait HasCustomKeys
      extends Individual.HasSimpleCustomKeys
      with    Individual.HasAdvancedCustomKeys

  // ---------------------------------------------------------------------------
  trait HasCustomPaths
      extends Individual.HasSimpleCustomLeafPaths
      with    Individual.HasSimpleCustomAllPaths
      with    Individual.HasAdvancedCustomPaths

  // ===========================================================================
  // indices

  trait IndexAndIndices
    extends Bundle.OneIndex
    with    Bundle.RepeatedIndices

    // ---------------------------------------------------------------------------
    trait OneIndex
        extends Individual.HasSingleExplicitIndex

        with    Individual.HasFirstKey
        with    Individual.HasSecondKey
        with    Individual.HasThirdKey

        with    Individual.HasSecondToLastKey
        with    Individual.HasLastKey

    // ---------------------------------------------------------------------------
    trait RepeatedIndices
        extends Individual.HasRepeatedExplicitIndex
        with    Individual.HasAllButIndices

    // ---------------------------------------------------------------------------
    trait HasAllAlls
      extends Individual.HasAllKeys
      with    Individual.HasAllPaths
      with    Individual.HasLeafPaths

  // ===========================================================================
  // explicits

  trait HasExplicitKeyW   extends Individual.HasSingleExplicitKeyW   with Individual.HasRepeatedExplicitKeyW
  trait HasExplicitRenW   extends Individual.HasSingleExplicitRenW   with Individual.HasRepeatedExplicitRenW
  trait HasExplicitKPathW extends Individual.HasSingleExplicitKPathW with Individual.HasRepeatedExplicitKPathW
  trait HasExplicitRPathW extends Individual.HasSingleExplicitRPathW with Individual.HasRepeatedExplicitRPathW

  trait HasExplicitIndex extends Individual.HasSingleExplicitIndex with Individual.HasRepeatedExplicitIndex

  // ===========================================================================
  type  CantRenameKey   =       Individual.HasSingleExplicitKeyW // for consistency only
  trait CantRenameKeys  extends Individual.HasSingleExplicitKeyW   with Individual.HasRepeatedExplicitKeyW
  trait  CanRenameKeys  extends Individual.HasSingleExplicitRenW   with Individual.HasRepeatedExplicitRenW
  trait CantRenamePaths extends Individual.HasSingleExplicitKPathW with Individual.HasRepeatedExplicitKPathW
  trait  CanRenamePaths extends Individual.HasSingleExplicitRPathW with Individual.HasRepeatedExplicitRPathW
}

// ===========================================================================
