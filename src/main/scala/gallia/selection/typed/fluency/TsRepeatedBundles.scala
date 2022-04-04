package gallia.selection.typed.fluency

// ===========================================================================
object TsRepeatedBundles {

  // ---------------------------------------------------------------------------
  trait HasRepeatedBasic[$Wrap, $Wrapz <: Seq[$Wrap]] extends
      TsRepeatedBundles.HasRepeatedRequiredBasic[$Wrap, $Wrapz] with
      TsRepeatedBundles.HasRepeatedOptionalBasic[$Wrap, $Wrapz] with
      TsRepeatedBundles.HasRepeatedXBasic       [$Wrap, $Wrapz] with
      TsRepeatedBundles.HasRepeatedTypedBasic   [$Wrap, $Wrapz]

    // ---------------------------------------------------------------------------        
    trait HasRepeatedRequiredBasic[$Wrap, $Wrapz <: Seq[$Wrap]] extends
      TsRepeatedBundles.HasRepeatedOneBasic[$Wrap, $Wrapz] with
      TsRepeatedBundles.HasRepeatedNesBasic[$Wrap, $Wrapz]

    trait HasRepeatedOptionalBasic[$Wrap, $Wrapz <: Seq[$Wrap]] extends
      TsRepeatedBundles.HasRepeatedOptBasic[$Wrap, $Wrapz] with
      TsRepeatedBundles.HasRepeatedPesBasic[$Wrap, $Wrapz]
  
    trait HasRepeatedTypedBasic[$Wrap, $Wrapz <: Seq[$Wrap]] extends
      TsRepeatedIndividual.HasRepeatedNonXTyped[$Wrap, $Wrapz] with  
      TsRepeatedIndividual.HasRepeatedXTyped   [$Wrap, $Wrapz]

  // ===========================================================================
  trait HasRepeatedOneBasic[$Wrap, $Wrapz <: Seq[$Wrap]] extends
      TsRepeatedIndividual.HasRepeatedOneString [$Wrap, $Wrapz] with
      TsRepeatedIndividual.HasRepeatedOneInt    [$Wrap, $Wrapz] with
      TsRepeatedIndividual.HasRepeatedOneDouble [$Wrap, $Wrapz] with
      TsRepeatedIndividual.HasRepeatedOneBoolean[$Wrap, $Wrapz] with

      TsRepeatedIndividual.HasRepeatedOneByte [$Wrap, $Wrapz] with
      TsRepeatedIndividual.HasRepeatedOneShort[$Wrap, $Wrapz] with
      TsRepeatedIndividual.HasRepeatedOneLong [$Wrap, $Wrapz] with
      TsRepeatedIndividual.HasRepeatedOneFloat[$Wrap, $Wrapz] with

      TsRepeatedIndividual.HasRepeatedOneBigInt[$Wrap, $Wrapz] with
      TsRepeatedIndividual.HasRepeatedOneBigDec[$Wrap, $Wrapz] with

      TsRepeatedIndividual.HasRepeatedOneLocalDate     [$Wrap, $Wrapz] with
      TsRepeatedIndividual.HasRepeatedOneLocalTime     [$Wrap, $Wrapz] with
      TsRepeatedIndividual.HasRepeatedOneLocalDateTime [$Wrap, $Wrapz] with
      TsRepeatedIndividual.HasRepeatedOneOffsetDateTime[$Wrap, $Wrapz] with
      TsRepeatedIndividual.HasRepeatedOneZonedDateTime [$Wrap, $Wrapz] with
      TsRepeatedIndividual.HasRepeatedOneInstant       [$Wrap, $Wrapz] with
      
      TsRepeatedIndividual.HasRepeatedOneBinary        [$Wrap, $Wrapz]

    // ---------------------------------------------------------------------------
    trait HasRepeatedOptBasic[$Wrap, $Wrapz <: Seq[$Wrap]] extends

      TsRepeatedIndividual.HasRepeatedOptString [$Wrap, $Wrapz] with
      TsRepeatedIndividual.HasRepeatedOptInt    [$Wrap, $Wrapz] with
      TsRepeatedIndividual.HasRepeatedOptDouble [$Wrap, $Wrapz] with
      TsRepeatedIndividual.HasRepeatedOptBoolean[$Wrap, $Wrapz] with

      TsRepeatedIndividual.HasRepeatedOptByte [$Wrap, $Wrapz] with
      TsRepeatedIndividual.HasRepeatedOptShort[$Wrap, $Wrapz] with
      TsRepeatedIndividual.HasRepeatedOptLong [$Wrap, $Wrapz] with
      TsRepeatedIndividual.HasRepeatedOptFloat[$Wrap, $Wrapz] with

      TsRepeatedIndividual.HasRepeatedOptBigInt[$Wrap, $Wrapz] with
      TsRepeatedIndividual.HasRepeatedOptBigDec[$Wrap, $Wrapz] with

      TsRepeatedIndividual.HasRepeatedOptLocalDate     [$Wrap, $Wrapz] with
      TsRepeatedIndividual.HasRepeatedOptLocalTime     [$Wrap, $Wrapz] with
      TsRepeatedIndividual.HasRepeatedOptLocalDateTime [$Wrap, $Wrapz] with
      TsRepeatedIndividual.HasRepeatedOptOffsetDateTime[$Wrap, $Wrapz] with
      TsRepeatedIndividual.HasRepeatedOptZonedDateTime [$Wrap, $Wrapz] with
      TsRepeatedIndividual.HasRepeatedOptInstant       [$Wrap, $Wrapz] with
      
      TsRepeatedIndividual.HasRepeatedOptBinary        [$Wrap, $Wrapz]


    // ---------------------------------------------------------------------------
    trait HasRepeatedNesBasic[$Wrap, $Wrapz <: Seq[$Wrap]] extends
      TsRepeatedIndividual.HasRepeatedNesString [$Wrap, $Wrapz] with
      TsRepeatedIndividual.HasRepeatedNesInt    [$Wrap, $Wrapz] with
      TsRepeatedIndividual.HasRepeatedNesDouble [$Wrap, $Wrapz] with
      TsRepeatedIndividual.HasRepeatedNesBoolean[$Wrap, $Wrapz] with

      TsRepeatedIndividual.HasRepeatedNesByte [$Wrap, $Wrapz] with
      TsRepeatedIndividual.HasRepeatedNesShort[$Wrap, $Wrapz] with
      TsRepeatedIndividual.HasRepeatedNesLong [$Wrap, $Wrapz] with
      TsRepeatedIndividual.HasRepeatedNesFloat[$Wrap, $Wrapz] with

      TsRepeatedIndividual.HasRepeatedNesBigInt[$Wrap, $Wrapz] with
      TsRepeatedIndividual.HasRepeatedNesBigDec[$Wrap, $Wrapz] with

      TsRepeatedIndividual.HasRepeatedNesLocalDate     [$Wrap, $Wrapz] with
      TsRepeatedIndividual.HasRepeatedNesLocalTime     [$Wrap, $Wrapz] with
      TsRepeatedIndividual.HasRepeatedNesLocalDateTime [$Wrap, $Wrapz] with
      TsRepeatedIndividual.HasRepeatedNesOffsetDateTime[$Wrap, $Wrapz] with
      TsRepeatedIndividual.HasRepeatedNesZonedDateTime [$Wrap, $Wrapz] with
      TsRepeatedIndividual.HasRepeatedNesInstant       [$Wrap, $Wrapz] with
      
      TsRepeatedIndividual.HasRepeatedNesBinary        [$Wrap, $Wrapz]

    // ---------------------------------------------------------------------------
    trait HasRepeatedPesBasic[$Wrap, $Wrapz <: Seq[$Wrap]] extends
      TsRepeatedIndividual.HasRepeatedPesString [$Wrap, $Wrapz] with
      TsRepeatedIndividual.HasRepeatedPesInt    [$Wrap, $Wrapz] with
      TsRepeatedIndividual.HasRepeatedPesDouble [$Wrap, $Wrapz] with
      TsRepeatedIndividual.HasRepeatedPesBoolean[$Wrap, $Wrapz] with

      TsRepeatedIndividual.HasRepeatedPesByte [$Wrap, $Wrapz] with
      TsRepeatedIndividual.HasRepeatedPesShort[$Wrap, $Wrapz] with
      TsRepeatedIndividual.HasRepeatedPesLong [$Wrap, $Wrapz] with
      TsRepeatedIndividual.HasRepeatedPesFloat[$Wrap, $Wrapz] with

      TsRepeatedIndividual.HasRepeatedPesBigInt[$Wrap, $Wrapz] with
      TsRepeatedIndividual.HasRepeatedPesBigDec[$Wrap, $Wrapz] with

      TsRepeatedIndividual.HasRepeatedPesLocalDate     [$Wrap, $Wrapz] with
      TsRepeatedIndividual.HasRepeatedPesLocalTime     [$Wrap, $Wrapz] with
      TsRepeatedIndividual.HasRepeatedPesLocalDateTime [$Wrap, $Wrapz] with
      TsRepeatedIndividual.HasRepeatedPesOffsetDateTime[$Wrap, $Wrapz] with
      TsRepeatedIndividual.HasRepeatedPesZonedDateTime [$Wrap, $Wrapz] with
      TsRepeatedIndividual.HasRepeatedPesInstant       [$Wrap, $Wrapz] with
      
      TsRepeatedIndividual.HasRepeatedPesBinary        [$Wrap, $Wrapz]

  // ---------------------------------------------------------------------------
  trait HasRepeatedXBasic[$Wrap, $Wrapz <: Seq[$Wrap]] extends
    TsRepeatedIndividual.HasRepeatedXTyped  [$Wrap, $Wrapz] with

    TsRepeatedIndividual.HasRepeatedXString [$Wrap, $Wrapz] with
    TsRepeatedIndividual.HasRepeatedXInt    [$Wrap, $Wrapz] with
    TsRepeatedIndividual.HasRepeatedXDouble [$Wrap, $Wrapz] with
    TsRepeatedIndividual.HasRepeatedXBoolean[$Wrap, $Wrapz] with

    TsRepeatedIndividual.HasRepeatedXByte [$Wrap, $Wrapz] with
    TsRepeatedIndividual.HasRepeatedXShort[$Wrap, $Wrapz] with
    TsRepeatedIndividual.HasRepeatedXLong [$Wrap, $Wrapz] with
    TsRepeatedIndividual.HasRepeatedXFloat[$Wrap, $Wrapz] with

    TsRepeatedIndividual.HasRepeatedXBigInt[$Wrap, $Wrapz] with
    TsRepeatedIndividual.HasRepeatedXBigDec[$Wrap, $Wrapz] with

    TsRepeatedIndividual.HasRepeatedXLocalDate     [$Wrap, $Wrapz] with
    TsRepeatedIndividual.HasRepeatedXLocalTime     [$Wrap, $Wrapz] with
    TsRepeatedIndividual.HasRepeatedXLocalDateTime [$Wrap, $Wrapz] with
    TsRepeatedIndividual.HasRepeatedXOffsetDateTime[$Wrap, $Wrapz] with
    TsRepeatedIndividual.HasRepeatedXZonedDateTime [$Wrap, $Wrapz] with    
    TsRepeatedIndividual.HasRepeatedXInstant       [$Wrap, $Wrapz] with
    
    TsRepeatedIndividual.HasRepeatedXBinary        [$Wrap, $Wrapz]

}

// ===========================================================================
