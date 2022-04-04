package gallia.selection.typed.fluency

// ===========================================================================
object TsSelBundles {

  // ---------------------------------------------------------------------------
  trait HasSelBasic[Origin, Target] extends
      TsSelBundles.HasSelRequiredBasic[Origin, Target] with
      TsSelBundles.HasSelOptionalBasic[Origin, Target] with
      TsSelBundles.HasSelXBasic       [Origin, Target] with
      TsSelBundles.HasSelTypedBasic   [Origin, Target]

    // ---------------------------------------------------------------------------        
    trait HasSelRequiredBasic[Origin, Target] extends
      TsSelBundles.HasSelOneBasic[Origin, Target] with
      TsSelBundles.HasSelNesBasic[Origin, Target]

    trait HasSelOptionalBasic[Origin, Target] extends
      TsSelBundles.HasSelOptBasic[Origin, Target] with
      TsSelBundles.HasSelPesBasic[Origin, Target]
  
    trait HasSelTypedBasic[Origin, Target] extends
      TsSelIndividual.HasSelNonXTyped[Origin, Target] with  
      TsSelIndividual.HasSelXTyped   [Origin, Target]

  // ===========================================================================
  trait HasSelOneBasic[Origin, Target] extends
      TsSelIndividual.HasSelOneString [Origin, Target] with
      TsSelIndividual.HasSelOneInt    [Origin, Target] with
      TsSelIndividual.HasSelOneDouble [Origin, Target] with
      TsSelIndividual.HasSelOneBoolean[Origin, Target] with

      TsSelIndividual.HasSelOneByte [Origin, Target] with
      TsSelIndividual.HasSelOneShort[Origin, Target] with
      TsSelIndividual.HasSelOneLong [Origin, Target] with
      TsSelIndividual.HasSelOneFloat[Origin, Target] with

      TsSelIndividual.HasSelOneBigInt[Origin, Target] with
      TsSelIndividual.HasSelOneBigDec[Origin, Target] with

      TsSelIndividual.HasSelOneLocalDate     [Origin, Target] with
      TsSelIndividual.HasSelOneLocalTime     [Origin, Target] with
      TsSelIndividual.HasSelOneLocalDateTime [Origin, Target] with
      TsSelIndividual.HasSelOneOffsetDateTime[Origin, Target] with
      TsSelIndividual.HasSelOneZonedDateTime [Origin, Target] with
      TsSelIndividual.HasSelOneInstant       [Origin, Target] with
      
      TsSelIndividual.HasSelOneBinary        [Origin, Target]

    // ---------------------------------------------------------------------------
    trait HasSelOptBasic[Origin, Target] extends
      TsSelIndividual.HasSelOptString [Origin, Target] with
      TsSelIndividual.HasSelOptInt    [Origin, Target] with
      TsSelIndividual.HasSelOptDouble [Origin, Target] with
      TsSelIndividual.HasSelOptBoolean[Origin, Target] with

      TsSelIndividual.HasSelOptByte [Origin, Target] with
      TsSelIndividual.HasSelOptShort[Origin, Target] with
      TsSelIndividual.HasSelOptLong [Origin, Target] with
      TsSelIndividual.HasSelOptFloat[Origin, Target] with

      TsSelIndividual.HasSelOptBigInt[Origin, Target] with
      TsSelIndividual.HasSelOptBigDec[Origin, Target] with

      TsSelIndividual.HasSelOptLocalDate     [Origin, Target] with
      TsSelIndividual.HasSelOptLocalTime     [Origin, Target] with
      TsSelIndividual.HasSelOptLocalDateTime [Origin, Target] with
      TsSelIndividual.HasSelOptOffsetDateTime[Origin, Target] with
      TsSelIndividual.HasSelOptZonedDateTime [Origin, Target] with
      TsSelIndividual.HasSelOptInstant       [Origin, Target] with
      
      TsSelIndividual.HasSelOptBinary        [Origin, Target]

    // ---------------------------------------------------------------------------
    trait HasSelNesBasic[Origin, Target] extends
      TsSelIndividual.HasSelNesString [Origin, Target] with
      TsSelIndividual.HasSelNesInt    [Origin, Target] with
      TsSelIndividual.HasSelNesDouble [Origin, Target] with
      TsSelIndividual.HasSelNesBoolean[Origin, Target] with

      TsSelIndividual.HasSelNesByte [Origin, Target] with
      TsSelIndividual.HasSelNesShort[Origin, Target] with
      TsSelIndividual.HasSelNesLong [Origin, Target] with
      TsSelIndividual.HasSelNesFloat[Origin, Target] with

      TsSelIndividual.HasSelNesBigInt[Origin, Target] with
      TsSelIndividual.HasSelNesBigDec[Origin, Target] with

      TsSelIndividual.HasSelNesLocalDate     [Origin, Target] with
      TsSelIndividual.HasSelNesLocalTime     [Origin, Target] with
      TsSelIndividual.HasSelNesLocalDateTime [Origin, Target] with
      TsSelIndividual.HasSelNesOffsetDateTime[Origin, Target] with
      TsSelIndividual.HasSelNesZonedDateTime [Origin, Target] with
      TsSelIndividual.HasSelNesInstant       [Origin, Target] with
      
      TsSelIndividual.HasSelNesBinary        [Origin, Target]

    // ---------------------------------------------------------------------------
    trait HasSelPesBasic[Origin, Target] extends
      TsSelIndividual.HasSelPesString [Origin, Target] with
      TsSelIndividual.HasSelPesInt    [Origin, Target] with
      TsSelIndividual.HasSelPesDouble [Origin, Target] with
      TsSelIndividual.HasSelPesBoolean[Origin, Target] with

      TsSelIndividual.HasSelPesByte [Origin, Target] with
      TsSelIndividual.HasSelPesShort[Origin, Target] with
      TsSelIndividual.HasSelPesLong [Origin, Target] with
      TsSelIndividual.HasSelPesFloat[Origin, Target] with

      TsSelIndividual.HasSelPesBigInt[Origin, Target] with
      TsSelIndividual.HasSelPesBigDec[Origin, Target] with

      TsSelIndividual.HasSelPesLocalDate     [Origin, Target] with
      TsSelIndividual.HasSelPesLocalTime     [Origin, Target] with
      TsSelIndividual.HasSelPesLocalDateTime [Origin, Target] with
      TsSelIndividual.HasSelPesOffsetDateTime[Origin, Target] with
      TsSelIndividual.HasSelPesZonedDateTime [Origin, Target] with
      TsSelIndividual.HasSelPesInstant       [Origin, Target] with
      
      TsSelIndividual.HasSelPesBinary        [Origin, Target]

  // ---------------------------------------------------------------------------
  trait HasSelXBasic[Origin, Target] extends
    TsSelIndividual.HasSelXTyped  [Origin, Target] with

    TsSelIndividual.HasSelXString [Origin, Target] with
    TsSelIndividual.HasSelXInt    [Origin, Target] with
    TsSelIndividual.HasSelXDouble [Origin, Target] with
    TsSelIndividual.HasSelXBoolean[Origin, Target] with

    TsSelIndividual.HasSelXByte [Origin, Target] with
    TsSelIndividual.HasSelXShort[Origin, Target] with
    TsSelIndividual.HasSelXLong [Origin, Target] with
    TsSelIndividual.HasSelXFloat[Origin, Target] with

    TsSelIndividual.HasSelXBigInt[Origin, Target] with
    TsSelIndividual.HasSelXBigDec[Origin, Target] with

    TsSelIndividual.HasSelXLocalDate     [Origin, Target] with
    TsSelIndividual.HasSelXLocalTime     [Origin, Target] with
    TsSelIndividual.HasSelXLocalDateTime [Origin, Target] with
    TsSelIndividual.HasSelXOffsetDateTime[Origin, Target] with
    TsSelIndividual.HasSelXZonedDateTime [Origin, Target] with
    TsSelIndividual.HasSelXInstant       [Origin, Target] with
    
    TsSelIndividual.HasSelXBinary        [Origin, Target]
    
}

// ===========================================================================
