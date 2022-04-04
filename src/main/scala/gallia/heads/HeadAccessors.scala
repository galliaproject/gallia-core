package gallia
package heads

import aptus.{One, Opt, Pes, Nes}

// ===========================================================================
@TypeMatching
private[gallia] trait HeadUAccessors { ignored: HeadU =>
  def forceAny(key: KPathW): One[Any] = typed(key, checkOrigin = false).forceValues[Any]

  // ===========================================================================
  def string  (key: KPathW): HeadV[One[String]] = typed  [String](key)
  def string_ (key: KPathW): HeadV[Opt[String]] = typed_ [String](key)
  def strings (key: KPathW): HeadV[Nes[String]] = typeds [String](key)
  def strings_(key: KPathW): HeadV[Pes[String]] = typeds_[String](key)

  def int     (key: KPathW): HeadV[One[Int]] = typed  [Int](key)
  def int_    (key: KPathW): HeadV[Opt[Int]] = typed_ [Int](key)
  def ints    (key: KPathW): HeadV[Nes[Int]] = typeds [Int](key)
  def ints_   (key: KPathW): HeadV[Pes[Int]] = typeds_[Int](key)

  def double  (key: KPathW): HeadV[One[Double]] = typed  [Double](key)
  def double_ (key: KPathW): HeadV[Opt[Double]] = typed_ [Double](key)
  def doubles (key: KPathW): HeadV[Nes[Double]] = typeds [Double](key)
  def doubles_(key: KPathW): HeadV[Pes[Double]] = typeds_[Double](key)

  def boolean  (key: KPathW): HeadV[One[Boolean]] = typed  [Boolean](key)
  def boolean_ (key: KPathW): HeadV[Opt[Boolean]] = typed_ [Boolean](key)
  def booleans (key: KPathW): HeadV[Nes[Boolean]] = typeds [Boolean](key)
  def booleans_(key: KPathW): HeadV[Pes[Boolean]] = typeds_[Boolean](key)

  // ---------------------------------------------------------------------------
  def byte    (key: KPathW): HeadV[One[Byte]] = typed  [Byte](key)
  def byte_   (key: KPathW): HeadV[Opt[Byte]] = typed_ [Byte](key)
  def bytes   (key: KPathW): HeadV[Nes[Byte]] = typeds [Byte](key)
  def bytes_  (key: KPathW): HeadV[Pes[Byte]] = typeds_[Byte](key)

  def short   (key: KPathW): HeadV[One[Short]] = typed  [Short](key)
  def short_  (key: KPathW): HeadV[Opt[Short]] = typed_ [Short](key)
  def shorts  (key: KPathW): HeadV[Nes[Short]] = typeds [Short](key)
  def shorts_ (key: KPathW): HeadV[Pes[Short]] = typeds_[Short](key)

  def long    (key: KPathW): HeadV[One[Long]] = typed  [Long](key)
  def long_   (key: KPathW): HeadV[Opt[Long]] = typed_ [Long](key)
  def longs   (key: KPathW): HeadV[Nes[Long]] = typeds [Long](key)
  def longs_  (key: KPathW): HeadV[Pes[Long]] = typeds_[Long](key)

  def float   (key: KPathW): HeadV[One[Float]] = typed  [Float](key)
  def float_  (key: KPathW): HeadV[Opt[Float]] = typed_ [Float](key)
  def floats  (key: KPathW): HeadV[Nes[Float]] = typeds [Float](key)
  def floats_ (key: KPathW): HeadV[Pes[Float]] = typeds_[Float](key)

  // ---------------------------------------------------------------------------
  def bigInt    (key: KPathW): HeadV[One[BigInt]] = typed  [BigInt](key)
  def bigInt_   (key: KPathW): HeadV[Opt[BigInt]] = typed_ [BigInt](key)
  def bigInts   (key: KPathW): HeadV[Nes[BigInt]] = typeds [BigInt](key)
  def bigInts_  (key: KPathW): HeadV[Pes[BigInt]] = typeds_[BigInt](key)

  def bigDec    (key: KPathW): HeadV[One[BigDec]] = typed  [BigDec](key)
  def bigDec_   (key: KPathW): HeadV[Opt[BigDec]] = typed_ [BigDec](key)
  def bigDecs   (key: KPathW): HeadV[Nes[BigDec]] = typeds [BigDec](key)
  def bigDecs_  (key: KPathW): HeadV[Pes[BigDec]] = typeds_[BigDec](key)

  // ---------------------------------------------------------------------------
  def localDate    (key: KPathW): HeadV[One[LocalDate]] = typed  [LocalDate](key)
  def localDate_   (key: KPathW): HeadV[Opt[LocalDate]] = typed_ [LocalDate](key)
  def localDates   (key: KPathW): HeadV[Nes[LocalDate]] = typeds [LocalDate](key)
  def localDates_  (key: KPathW): HeadV[Pes[LocalDate]] = typeds_[LocalDate](key)

  def localTime    (key: KPathW): HeadV[One[LocalTime]] = typed  [LocalTime](key)
  def localTime_   (key: KPathW): HeadV[Opt[LocalTime]] = typed_ [LocalTime](key)
  def localTimes   (key: KPathW): HeadV[Nes[LocalTime]] = typeds [LocalTime](key)
  def localTimes_  (key: KPathW): HeadV[Pes[LocalTime]] = typeds_[LocalTime](key)
  
  def localDateTime    (key: KPathW): HeadV[One[LocalDateTime]] = typed  [LocalDateTime](key)
  def localDateTime_   (key: KPathW): HeadV[Opt[LocalDateTime]] = typed_ [LocalDateTime](key)
  def localDateTimes   (key: KPathW): HeadV[Nes[LocalDateTime]] = typeds [LocalDateTime](key)
  def localDateTimes_  (key: KPathW): HeadV[Pes[LocalDateTime]] = typeds_[LocalDateTime](key)

  def offsetDateTime    (key: KPathW): HeadV[One[OffsetDateTime]] = typed  [OffsetDateTime](key)
  def offsetDateTime_   (key: KPathW): HeadV[Opt[OffsetDateTime]] = typed_ [OffsetDateTime](key)
  def offsetDateTimes   (key: KPathW): HeadV[Nes[OffsetDateTime]] = typeds [OffsetDateTime](key)
  def offsetDateTimes_  (key: KPathW): HeadV[Pes[OffsetDateTime]] = typeds_[OffsetDateTime](key)

  def zonedDateTime    (key: KPathW): HeadV[One[ZonedDateTime]] = typed  [ZonedDateTime](key)
  def zonedDateTime_   (key: KPathW): HeadV[Opt[ZonedDateTime]] = typed_ [ZonedDateTime](key)
  def zonedDateTimes   (key: KPathW): HeadV[Nes[ZonedDateTime]] = typeds [ZonedDateTime](key)
  def zonedDateTimes_  (key: KPathW): HeadV[Pes[ZonedDateTime]] = typeds_[ZonedDateTime](key)

  def instant    (key: KPathW): HeadV[One[Instant]] = typed  [Instant](key)
  def instant_   (key: KPathW): HeadV[Opt[Instant]] = typed_ [Instant](key)
  def instants   (key: KPathW): HeadV[Nes[Instant]] = typeds [Instant](key)
  def instants_  (key: KPathW): HeadV[Pes[Instant]] = typeds_[Instant](key)

  // ---------------------------------------------------------------------------
  def binary    (key: KPathW): HeadV[One[ByteBuffer]] = typed  [ByteBuffer](key)
  def binary_   (key: KPathW): HeadV[Opt[ByteBuffer]] = typed_ [ByteBuffer](key)
  def binarys   (key: KPathW): HeadV[Nes[ByteBuffer]] = typeds [ByteBuffer](key)
  def binarys_  (key: KPathW): HeadV[Pes[ByteBuffer]] = typeds_[ByteBuffer](key)
  
  // ===========================================================================
  def forceString  (key: KPathW): One[String] = forceTyped  [String](key)
  def forceString_ (key: KPathW): Opt[String] = forceTyped_ [String](key)
  def forceStrings (key: KPathW): Nes[String] = forceTypeds [String](key)
  def forceStrings_(key: KPathW): Pes[String] = forceTypeds_[String](key)

  def forceInt  (key: KPathW): One[Int] = forceTyped  [Int](key)
  def forceInt_ (key: KPathW): Opt[Int] = forceTyped_ [Int](key)
  def forceInts (key: KPathW): Nes[Int] = forceTypeds [Int](key)
  def forceInts_(key: KPathW): Pes[Int] = forceTypeds_[Int](key)

  def forceDouble  (key: KPathW): One[Double] = forceTyped  [Double](key)
  def forceDouble_ (key: KPathW): Opt[Double] = forceTyped_ [Double](key)
  def forceDoubles (key: KPathW): Nes[Double] = forceTypeds [Double](key)
  def forceDoubles_(key: KPathW): Pes[Double] = forceTypeds_[Double](key)

  def forceBoolean  (key: KPathW): One[Boolean] = forceTyped  [Boolean](key)
  def forceBoolean_ (key: KPathW): Opt[Boolean] = forceTyped_ [Boolean](key)
  def forceBooleans (key: KPathW): Nes[Boolean] = forceTypeds [Boolean](key)
  def forceBooleans_(key: KPathW): Pes[Boolean] = forceTypeds_[Boolean](key)

  // ---------------------------------------------------------------------------
  def forceByte  (key: KPathW): One[Byte] = forceTyped  [Byte](key)
  def forceByte_ (key: KPathW): Opt[Byte] = forceTyped_ [Byte](key)
  def forceBytes (key: KPathW): Nes[Byte] = forceTypeds [Byte](key)
  def forceBytes_(key: KPathW): Pes[Byte] = forceTypeds_[Byte](key)

  def forceShort  (key: KPathW): One[Short] = forceTyped  [Short](key)
  def forceShort_ (key: KPathW): Opt[Short] = forceTyped_ [Short](key)
  def forceShorts (key: KPathW): Nes[Short] = forceTypeds [Short](key)
  def forceShorts_(key: KPathW): Pes[Short] = forceTypeds_[Short](key)

  def forceLong  (key: KPathW): One[Long] = forceTyped  [Long](key)
  def forceLong_ (key: KPathW): Opt[Long] = forceTyped_ [Long](key)
  def forceLongs (key: KPathW): Nes[Long] = forceTypeds [Long](key)
  def forceLongs_(key: KPathW): Pes[Long] = forceTypeds_[Long](key)

  def forceFloat  (key: KPathW): One[Float] = forceTyped  [Float](key)
  def forceFloat_ (key: KPathW): Opt[Float] = forceTyped_ [Float](key)
  def forceFloats (key: KPathW): Nes[Float] = forceTypeds [Float](key)
  def forceFloats_(key: KPathW): Pes[Float] = forceTypeds_[Float](key)

  // ---------------------------------------------------------------------------
  def forceBigInt  (key: KPathW): One[BigInt] = forceTyped  [BigInt](key)
  def forceBigInt_ (key: KPathW): Opt[BigInt] = forceTyped_ [BigInt](key)
  def forceBigInts (key: KPathW): Nes[BigInt] = forceTypeds [BigInt](key)
  def forceBigInts_(key: KPathW): Pes[BigInt] = forceTypeds_[BigInt](key)

  def forceBigDec  (key: KPathW): One[BigDec] = forceTyped  [BigDec](key)
  def forceBigDec_ (key: KPathW): Opt[BigDec] = forceTyped_ [BigDec](key)
  def forceBigDecs (key: KPathW): Nes[BigDec] = forceTypeds [BigDec](key)
  def forceBigDecs_(key: KPathW): Pes[BigDec] = forceTypeds_[BigDec](key)

  // ---------------------------------------------------------------------------
  def forceLocalDate  (key: KPathW): One[LocalDate] = forceTyped  [LocalDate](key)
  def forceLocalDate_ (key: KPathW): Opt[LocalDate] = forceTyped_ [LocalDate](key)
  def forceLocalDates (key: KPathW): Nes[LocalDate] = forceTypeds [LocalDate](key)
  def forceLocalDates_(key: KPathW): Pes[LocalDate] = forceTypeds_[LocalDate](key)

  def forceLocalTime  (key: KPathW): One[LocalTime] = forceTyped  [LocalTime](key)
  def forceLocalTime_ (key: KPathW): Opt[LocalTime] = forceTyped_ [LocalTime](key)
  def forceLocalTimes (key: KPathW): Nes[LocalTime] = forceTypeds [LocalTime](key)
  def forceLocalTimes_(key: KPathW): Pes[LocalTime] = forceTypeds_[LocalTime](key)
  
  def forceLocalDateTime  (key: KPathW): One[LocalDateTime] = forceTyped  [LocalDateTime](key)
  def forceLocalDateTime_ (key: KPathW): Opt[LocalDateTime] = forceTyped_ [LocalDateTime](key)
  def forceLocalDateTimes (key: KPathW): Nes[LocalDateTime] = forceTypeds [LocalDateTime](key)
  def forceLocalDateTimes_(key: KPathW): Pes[LocalDateTime] = forceTypeds_[LocalDateTime](key)

  def forceOffsetDateTime  (key: KPathW): One[OffsetDateTime] = forceTyped  [OffsetDateTime](key)
  def forceOffsetDateTime_ (key: KPathW): Opt[OffsetDateTime] = forceTyped_ [OffsetDateTime](key)
  def forceOffsetDateTimes (key: KPathW): Nes[OffsetDateTime] = forceTypeds [OffsetDateTime](key)
  def forceOffsetDateTimes_(key: KPathW): Pes[OffsetDateTime] = forceTypeds_[OffsetDateTime](key)

  def forceZonedDateTime  (key: KPathW): One[ZonedDateTime] = forceTyped  [ZonedDateTime](key)
  def forceZonedDateTime_ (key: KPathW): Opt[ZonedDateTime] = forceTyped_ [ZonedDateTime](key)
  def forceZonedDateTimes (key: KPathW): Nes[ZonedDateTime] = forceTypeds [ZonedDateTime](key)
  def forceZonedDateTimes_(key: KPathW): Pes[ZonedDateTime] = forceTypeds_[ZonedDateTime](key)

  def forceInstant  (key: KPathW): One[Instant] = forceTyped  [Instant](key)
  def forceInstant_ (key: KPathW): Opt[Instant] = forceTyped_ [Instant](key)
  def forceInstants (key: KPathW): Nes[Instant] = forceTypeds [Instant](key)
  def forceInstants_(key: KPathW): Pes[Instant] = forceTypeds_[Instant](key)

  // ---------------------------------------------------------------------------
  def forceBinary  (key: KPathW): One[ByteBuffer] = forceTyped  [ByteBuffer](key)
  def forceBinary_ (key: KPathW): Opt[ByteBuffer] = forceTyped_ [ByteBuffer](key)
  def forceBinarys (key: KPathW): Nes[ByteBuffer] = forceTypeds [ByteBuffer](key)
  def forceBinarys_(key: KPathW): Pes[ByteBuffer] = forceTypeds_[ByteBuffer](key)
  
  // ---------------------------------------------------------------------------
  //TODO: enum - t210201095414
}

// ===========================================================================
@TypeMatching
private[gallia] trait HeadZAccessors { ignored: HeadZ => // TODO: t210202101142 - keep "accessor"?
  def forceAnys(key: KPathW): Nes[Any] = typeds(key, checkOrigin = false).forceValues[Any]

  // ===========================================================================
  def strings   (key: KPathW): HeadV[Nes[String]] = typeds [String](key)
  def strings_  (key: KPathW): HeadV[Pes[String]] = typeds_[String](key)

  def ints      (key: KPathW): HeadV[Nes[Int]] = typeds [Int](key)
  def ints_     (key: KPathW): HeadV[Pes[Int]] = typeds_[Int](key)

  def doubles    (key: KPathW): HeadV[Nes[Double]] = typeds [Double](key)
  def doubles_   (key: KPathW): HeadV[Pes[Double]] = typeds_[Double](key)

  def booleans    (key: KPathW): HeadV[Nes[Boolean]] = typeds [Boolean](key)
  def booleans_   (key: KPathW): HeadV[Pes[Boolean]] = typeds_[Boolean](key)

  // ---------------------------------------------------------------------------
  def bytes   (key: KPathW): HeadV[Nes[Byte]] = typeds [Byte](key)
  def bytes_  (key: KPathW): HeadV[Pes[Byte]] = typeds_[Byte](key)

  def shorts  (key: KPathW): HeadV[Nes[Short]] = typeds [Short](key)
  def shorts_ (key: KPathW): HeadV[Pes[Short]] = typeds_[Short](key)

  def longs   (key: KPathW): HeadV[Nes[Long]] = typeds [Long](key)
  def longs_  (key: KPathW): HeadV[Pes[Long]] = typeds_[Long](key)

  def floats  (key: KPathW): HeadV[Nes[Float]] = typeds [Float](key)
  def floats_ (key: KPathW): HeadV[Pes[Float]] = typeds_[Float](key)

  // ---------------------------------------------------------------------------
  def bigInts      (key: KPathW): HeadV[Nes[BigInt]] = typeds [BigInt](key)
  def bigInts_     (key: KPathW): HeadV[Pes[BigInt]] = typeds_[BigInt](key)

  def bigDecs  (key: KPathW): HeadV[Nes[BigDec]] = typeds [BigDec](key)
  def bigDecs_ (key: KPathW): HeadV[Pes[BigDec]] = typeds_[BigDec](key)

  // ---------------------------------------------------------------------------
  def localDates        (key: KPathW): HeadV[Nes[LocalDate]] = typeds [LocalDate](key)
  def localDates_       (key: KPathW): HeadV[Pes[LocalDate]] = typeds_[LocalDate](key)

  def localTimes        (key: KPathW): HeadV[Nes[LocalTime]] = typeds [LocalTime](key)
  def localTimes_       (key: KPathW): HeadV[Pes[LocalTime]] = typeds_[LocalTime](key)
  
  def localDateTimes    (key: KPathW): HeadV[Nes[LocalDateTime]] = typeds [LocalDateTime](key)
  def localDateTimes_   (key: KPathW): HeadV[Pes[LocalDateTime]] = typeds_[LocalDateTime](key)

  def offsetDateTimes    (key: KPathW): HeadV[Nes[OffsetDateTime]] = typeds [OffsetDateTime](key)
  def offsetDateTimes_   (key: KPathW): HeadV[Pes[OffsetDateTime]] = typeds_[OffsetDateTime](key)

  def zonedDateTimes    (key: KPathW): HeadV[Nes[ZonedDateTime]] = typeds [ZonedDateTime](key)
  def zonedDateTimes_   (key: KPathW): HeadV[Pes[ZonedDateTime]] = typeds_[ZonedDateTime](key)

  def instants    (key: KPathW): HeadV[Nes[Instant]] = typeds [Instant](key)
  def instants_   (key: KPathW): HeadV[Pes[Instant]] = typeds_[Instant](key)
  
  // ---------------------------------------------------------------------------
  def binarys    (key: KPathW): HeadV[Nes[ByteBuffer]] = typeds [ByteBuffer](key)
  def binarys_   (key: KPathW): HeadV[Pes[ByteBuffer]] = typeds_[ByteBuffer](key)

  // ---------------------------------------------------------------------------
  //TODO: enum - t210201095414

  // ===========================================================================
  def forceStrings (key: KPathW): Nes[String] = forceTypeds [String](key)
  def forceStrings_(key: KPathW): Pes[String] = forceTypeds_[String](key)

  def forceInts    (key: KPathW): Nes[Int]    = forceTypeds [Int](key)
  def forceInts_   (key: KPathW): Pes[Int]    = forceTypeds_[Int](key)

  def forceDoubles (key: KPathW): Nes[Double] = forceTypeds [Double](key)
  def forceDoubles_(key: KPathW): Pes[Double] = forceTypeds_[Double](key)

  def forceBooleans (key: KPathW): Nes[Boolean] = forceTypeds [Boolean](key)
  def forceBooleans_(key: KPathW): Pes[Boolean] = forceTypeds_[Boolean](key)

  // ---------------------------------------------------------------------------
  def forceBytes   (key: KPathW): Nes[Byte] = forceTypeds [Byte](key)
  def forceBytes_  (key: KPathW): Pes[Byte] = forceTypeds_[Byte](key)

  def forceShorts  (key: KPathW): Nes[Short] = forceTypeds [Short](key)
  def forceShorts_ (key: KPathW): Pes[Short] = forceTypeds_[Short](key)

  def forceLongs   (key: KPathW): Nes[Long] = forceTypeds [Long](key)
  def forceLongs_  (key: KPathW): Pes[Long] = forceTypeds_[Long](key)

  def forceFloats  (key: KPathW): Nes[Float] = forceTypeds [Float](key)
  def forceFloats_ (key: KPathW): Pes[Float] = forceTypeds_[Float](key)

  // ---------------------------------------------------------------------------
  def forceBigInts (key: KPathW): Nes[BigInt] = forceTypeds [BigInt](key)
  def forceBigInts_(key: KPathW): Pes[BigInt] = forceTypeds_[BigInt](key)

  def forceBigDecs    (key: KPathW): Nes[BigDec] = forceTypeds [BigDec](key)
  def forceBigDecs_   (key: KPathW): Pes[BigDec] = forceTypeds_[BigDec](key)

  // ---------------------------------------------------------------------------
  def forceLocalDates (key: KPathW): Nes[LocalDate] = forceTypeds [LocalDate](key)
  def forceLocalDates_(key: KPathW): Pes[LocalDate] = forceTypeds_[LocalDate](key)

  def forceLocalTimes (key: KPathW): Nes[LocalTime] = forceTypeds [LocalTime](key)
  def forceLocalTimes_(key: KPathW): Pes[LocalTime] = forceTypeds_[LocalTime](key)
  
  def forceLocalDateTimes (key: KPathW): Nes[LocalDateTime] = forceTypeds [LocalDateTime](key)
  def forceLocalDateTimes_(key: KPathW): Pes[LocalDateTime] = forceTypeds_[LocalDateTime](key)

  def forceOffsetDateTimes (key: KPathW): Nes[OffsetDateTime] = forceTypeds [OffsetDateTime](key)
  def forceOffsetDateTimes_(key: KPathW): Pes[OffsetDateTime] = forceTypeds_[OffsetDateTime](key)

  def forceZonedDateTimes (key: KPathW): Nes[ZonedDateTime] = forceTypeds [ZonedDateTime](key)
  def forceZonedDateTimes_(key: KPathW): Pes[ZonedDateTime] = forceTypeds_[ZonedDateTime](key)

  def forceInstants (key: KPathW): Nes[Instant] = forceTypeds [Instant](key)
  def forceInstants_(key: KPathW): Pes[Instant] = forceTypeds_[Instant](key)

  // ---------------------------------------------------------------------------
  def forceBinarys (key: KPathW): Nes[ByteBuffer] = forceTypeds [ByteBuffer](key)
  def forceBinarys_(key: KPathW): Pes[ByteBuffer] = forceTypeds_[ByteBuffer](key)

  // ---------------------------------------------------------------------------
  //TODO: enum - t210201095414   
}

// ===========================================================================
