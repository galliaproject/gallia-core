package gallia
package selection.typed.fluency

// ===========================================================================
@TypeMatching
private[gallia] object TsSingleBundles { //TODO: enum - t210201095414

  // ===========================================================================
  trait HasSingleNesting[$Wrap] extends
           TsSingleIndividual.HasSingleObj [$Wrap]
      with TsSingleIndividual.HasSingleObjz[$Wrap]

  // ---------------------------------------------------------------------------
  trait HasSingleBasic[$Wrap] extends
      TsSingleBundles.HasSingleRequiredBasic[$Wrap] with
      TsSingleBundles.HasSingleOptionalBasic[$Wrap] with
      TsSingleBundles.HasSingleXBasic       [$Wrap] with
      TsSingleBundles.HasSingleTypedBasic   [$Wrap]

    // ---------------------------------------------------------------------------        
    trait HasSingleRequiredBasic[$Wrap] extends
      TsSingleBundles.HasSingleOneBasic[$Wrap] with
      TsSingleBundles.HasSingleNesBasic[$Wrap]

    trait HasSingleOptionalBasic[$Wrap] extends
      TsSingleBundles.HasSingleOptBasic[$Wrap] with
      TsSingleBundles.HasSinglePesBasic[$Wrap]  
      
    trait HasSingleTypedBasic[$Wrap] extends
      TsSingleIndividual.HasSingleNonXTyped[$Wrap] with  
      TsSingleIndividual.HasSingleXTyped   [$Wrap]
  
  // ===========================================================================
  trait HasSingleOneBasic[$Wrap] extends
      TsSingleIndividual.HasSingleOneString [$Wrap] with
      TsSingleIndividual.HasSingleOneInt    [$Wrap] with
      TsSingleIndividual.HasSingleOneDouble [$Wrap] with
      TsSingleIndividual.HasSingleOneBoolean[$Wrap] with

      TsSingleIndividual.HasSingleOneByte [$Wrap] with
      TsSingleIndividual.HasSingleOneShort[$Wrap] with
      TsSingleIndividual.HasSingleOneLong [$Wrap] with
      TsSingleIndividual.HasSingleOneFloat[$Wrap] with

      TsSingleIndividual.HasSingleOneBigInt[$Wrap] with
      TsSingleIndividual.HasSingleOneBigDec[$Wrap] with

      TsSingleIndividual.HasSingleOneLocalDate     [$Wrap] with
      TsSingleIndividual.HasSingleOneLocalTime     [$Wrap] with
      TsSingleIndividual.HasSingleOneLocalDateTime [$Wrap] with
      TsSingleIndividual.HasSingleOneOffsetDateTime[$Wrap] with
      TsSingleIndividual.HasSingleOneZonedDateTime [$Wrap] with
      TsSingleIndividual.HasSingleOneInstant       [$Wrap]

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

      TsSingleIndividual.HasSingleOptBigInt[$Wrap] with
      TsSingleIndividual.HasSingleOptBigDec[$Wrap] with

      TsSingleIndividual.HasSingleOptLocalDate     [$Wrap] with
      TsSingleIndividual.HasSingleOptLocalTime     [$Wrap] with
      TsSingleIndividual.HasSingleOptLocalDateTime [$Wrap] with
      TsSingleIndividual.HasSingleOptOffsetDateTime[$Wrap] with
      TsSingleIndividual.HasSingleOptZonedDateTime [$Wrap] with
      TsSingleIndividual.HasSingleOptInstant       [$Wrap]


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

      TsSingleIndividual.HasSingleNesBigInt[$Wrap] with
      TsSingleIndividual.HasSingleNesBigDec[$Wrap] with

      TsSingleIndividual.HasSingleNesLocalDate     [$Wrap] with
      TsSingleIndividual.HasSingleNesLocalTime     [$Wrap] with
      TsSingleIndividual.HasSingleNesLocalDateTime [$Wrap] with
      TsSingleIndividual.HasSingleNesOffsetDateTime[$Wrap] with
      TsSingleIndividual.HasSingleNesZonedDateTime [$Wrap] with
      TsSingleIndividual.HasSingleNesInstant       [$Wrap]

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

      TsSingleIndividual.HasSinglePesBigInt[$Wrap] with
      TsSingleIndividual.HasSinglePesBigDec[$Wrap] with

      TsSingleIndividual.HasSinglePesLocalDate     [$Wrap] with
      TsSingleIndividual.HasSinglePesLocalTime     [$Wrap] with
      TsSingleIndividual.HasSinglePesLocalDateTime [$Wrap] with
      TsSingleIndividual.HasSinglePesOffsetDateTime[$Wrap] with
      TsSingleIndividual.HasSinglePesZonedDateTime [$Wrap] with
      TsSingleIndividual.HasSinglePesInstant       [$Wrap]

  // ---------------------------------------------------------------------------
  trait HasSingleXBasic[$Wrap] extends  
    TsSingleIndividual.HasSingleXString [$Wrap] with
    TsSingleIndividual.HasSingleXInt    [$Wrap] with
    TsSingleIndividual.HasSingleXDouble [$Wrap] with
    TsSingleIndividual.HasSingleXBoolean[$Wrap] with

    TsSingleIndividual.HasSingleXByte [$Wrap] with
    TsSingleIndividual.HasSingleXShort[$Wrap] with
    TsSingleIndividual.HasSingleXLong [$Wrap] with
    TsSingleIndividual.HasSingleXFloat[$Wrap] with

    TsSingleIndividual.HasSingleXBigInt[$Wrap] with
    TsSingleIndividual.HasSingleXBigDec[$Wrap] with

    TsSingleIndividual.HasSingleXLocalDate     [$Wrap] with
    TsSingleIndividual.HasSingleXLocalTime     [$Wrap] with
    TsSingleIndividual.HasSingleXLocalDateTime [$Wrap] with
    TsSingleIndividual.HasSingleXOffsetDateTime[$Wrap] with
    TsSingleIndividual.HasSingleXZonedDateTime [$Wrap] with
    TsSingleIndividual.HasSingleXInstant       [$Wrap]  
}

// ===========================================================================
