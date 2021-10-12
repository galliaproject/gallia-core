package gallia
package selection.typed.fluency

// ===========================================================================
@gallia.TypeMatching
private[gallia] object TsSingleBundles {

  trait HasSingleRequiredBasic[$Wrap] extends
        TsSingleBundles.HasSingleOneBasic[$Wrap] with
        TsSingleBundles.HasSingleNesBasic[$Wrap]

      trait HasSingleOptionalBasic[$Wrap] extends
        TsSingleBundles.HasSingleOptBasic[$Wrap] with
        TsSingleBundles.HasSinglePesBasic[$Wrap]

  // ===========================================================================
  trait HasSingleTyped[$Wrap] extends
           TsSingleBundles.HasSingleRequiredTyped[$Wrap]
      with TsSingleBundles.HasSingleOptionalTyped[$Wrap]

    // ---------------------------------------------------------------------------
    trait HasSingleRequiredTyped[$Wrap] extends
           TsSingleIndividual.HasSingleOneTyped[$Wrap]
      with TsSingleIndividual.HasSingleNesTyped[$Wrap]

    // ---------------------------------------------------------------------------
    trait HasSingleOptionalTyped[$Wrap] extends
           TsSingleIndividual.HasSingleOptTyped[$Wrap]
      with TsSingleIndividual.HasSinglePesTyped[$Wrap]

  // ===========================================================================
    //TODO: enum - t210201095414
    trait HasSingleOneBasic[$Wrap] extends
        TsSingleIndividual.HasSingleOneString [$Wrap] with
        TsSingleIndividual.HasSingleOneInt    [$Wrap] with
        TsSingleIndividual.HasSingleOneDouble [$Wrap] with
        TsSingleIndividual.HasSingleOneBoolean[$Wrap] with

        TsSingleIndividual.HasSingleOneByte [$Wrap] with
        TsSingleIndividual.HasSingleOneShort[$Wrap] with
        TsSingleIndividual.HasSingleOneLong [$Wrap] with

        TsSingleIndividual.HasSingleOneFloat[$Wrap] with

        TsSingleIndividual.HasSingleOneBigInt    [$Wrap] with
        TsSingleIndividual.HasSingleOneBigDecimal[$Wrap] with

        TsSingleIndividual.HasSingleOneLocalDate    [$Wrap] with
        TsSingleIndividual.HasSingleOneLocalDateTime[$Wrap]

      // ---------------------------------------------------------------------------
      trait HasSingleOptBasic[$Wrap] extends

        TsSingleIndividual.HasSingleOptString [$Wrap] with
        TsSingleIndividual.HasSingleOptInt    [$Wrap] with
        TsSingleIndividual.HasSingleOptDouble [$Wrap] with
        TsSingleIndividual.HasSingleOptBoolean[$Wrap] with

        TsSingleIndividual.HasSingleOptByte [$Wrap] with
        TsSingleIndividual.HasSingleOptShort[$Wrap] with
        TsSingleIndividual.HasSingleOptLong [$Wrap] with

        TsSingleIndividual.HasSingleOptFloat[$Wrap] with

        TsSingleIndividual.HasSingleOptBigInt    [$Wrap] with
        TsSingleIndividual.HasSingleOptBigDecimal[$Wrap] with

        TsSingleIndividual.HasSingleOptLocalDate    [$Wrap] with
        TsSingleIndividual.HasSingleOptLocalDateTime[$Wrap]

      // ---------------------------------------------------------------------------
      trait HasSingleNesBasic[$Wrap] extends
        TsSingleIndividual.HasSingleNesString [$Wrap] with
        TsSingleIndividual.HasSingleNesInt    [$Wrap] with
        TsSingleIndividual.HasSingleNesDouble [$Wrap] with
        TsSingleIndividual.HasSingleNesBoolean[$Wrap] with

        TsSingleIndividual.HasSingleNesByte [$Wrap] with
        TsSingleIndividual.HasSingleNesShort[$Wrap] with
        TsSingleIndividual.HasSingleNesLong [$Wrap] with

        TsSingleIndividual.HasSingleNesFloat[$Wrap] with

        TsSingleIndividual.HasSingleNesBigInt    [$Wrap] with
        TsSingleIndividual.HasSingleNesBigDecimal[$Wrap] with

        TsSingleIndividual.HasSingleNesLocalDate    [$Wrap] with
        TsSingleIndividual.HasSingleNesLocalDateTime[$Wrap]

      // ---------------------------------------------------------------------------
      trait HasSinglePesBasic[$Wrap] extends
        TsSingleIndividual.HasSinglePesString [$Wrap] with
        TsSingleIndividual.HasSinglePesInt    [$Wrap] with
        TsSingleIndividual.HasSinglePesDouble [$Wrap] with
        TsSingleIndividual.HasSinglePesBoolean[$Wrap] with

        TsSingleIndividual.HasSinglePesByte [$Wrap] with
        TsSingleIndividual.HasSinglePesShort[$Wrap] with
        TsSingleIndividual.HasSinglePesLong [$Wrap] with

        TsSingleIndividual.HasSinglePesFloat[$Wrap] with

        TsSingleIndividual.HasSinglePesBigInt    [$Wrap] with
        TsSingleIndividual.HasSinglePesBigDecimal[$Wrap] with

        TsSingleIndividual.HasSinglePesLocalDate    [$Wrap] with
        TsSingleIndividual.HasSinglePesLocalDateTime[$Wrap]
        // TODO: more...

    // ---------------------------------------------------------------------------
    trait HasSingleXBasic[$Wrap] extends
      TsSingleIndividual.HasSingleXTyped  [$Wrap] with

      TsSingleIndividual.HasSingleXString [$Wrap] with
      TsSingleIndividual.HasSingleXInt    [$Wrap] with
      TsSingleIndividual.HasSingleXDouble [$Wrap] with
      TsSingleIndividual.HasSingleXBoolean[$Wrap] with

      TsSingleIndividual.HasSingleXByte [$Wrap] with
      TsSingleIndividual.HasSingleXShort[$Wrap] with
      TsSingleIndividual.HasSingleXLong [$Wrap] with

      TsSingleIndividual.HasSingleXFloat[$Wrap] with

      TsSingleIndividual.HasSingleXBigInt    [$Wrap] with
      TsSingleIndividual.HasSingleXBigDecimal[$Wrap] with

      TsSingleIndividual.HasSingleXLocalDate    [$Wrap] with
      TsSingleIndividual.HasSingleXLocalDateTime[$Wrap] with

      TsSingleIndividual.HasSingleXEnum[$Wrap]
}

// ===========================================================================
