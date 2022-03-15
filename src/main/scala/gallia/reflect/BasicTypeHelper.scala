package gallia
package reflect

import atoms.utils.SuperMetaPair

// ===========================================================================
private[reflect] trait BasicTypeHelper { ignored: BasicType =>

  def _superPair(container: Container, descending: Boolean, missingLast: Boolean) =
      container match {
        case Container._One => SuperMetaPair( ctag, (if (descending)  ordD else  ordA))
        case Container._Nes => SuperMetaPair(nctag, (if (descending) nordD else nordA))
        case Container._Opt => SuperMetaPair(octag,
          ((descending, missingLast) match {
            case (true , true ) => oordDL
            case (true , false) => oordDF
            case (false, true ) => oordAL
            case (false, false) => oordAF }))
        case Container._Pes => SuperMetaPair(pctag,
          ((descending, missingLast) match {
            case (true , true ) => pordDL
            case (true , false) => pordDF
            case (false, true ) => pordAL
            case (false, false) => pordAF })) }

  // ===========================================================================
  def _compare(container: Container, descending: Boolean, missingLast: Boolean)(x: AnyValue, y: AnyValue): Int =
    container match {
      case Container._One => (if (descending)  ordD else  ordA).compare(x.asInstanceOf[         T ], y.asInstanceOf[         T ])
      case Container._Nes => (if (descending) nordD else nordA).compare(x.asInstanceOf[Iterable[T]], y.asInstanceOf[Iterable[T]])

      // ---------------------------------------------------------------------------
      case Container._Opt => ((descending, missingLast) match {
          case (true , true ) => oordDL
          case (true , false) => oordDF
          case (false, true ) => oordAL
          case (false, false) => oordAF })
        .compare(x.asInstanceOf[Option[T]], y.asInstanceOf[Option[T]])
      // ---------------------------------------------------------------------------
      case Container._Pes => ((descending, missingLast) match {
          case (true , true ) => pordDL
          case (true , false) => pordDF
          case (false, true ) => pordAL
          case (false, false) => pordAF })
        .compare(x.asInstanceOf[Option[Iterable[T]]], y.asInstanceOf[Option[Iterable[T]]]) }
}

// ===========================================================================
object CustomOrdering { 
  private implicit val _arrayOrdering = aptus.arrayOrdering[Byte]
  
  // ---------------------------------------------------------------------------
  def localDate    : Ordering[LocalDate]       = Ordering.by(_.toEpochDay)
  def localTime    : Ordering[LocalTime]       = Ordering.by(_.toNanoOfDay)

  // ---------------------------------------------------------------------------
  def  localDateTime: Ordering[ LocalDateTime] = Ordering.by(_.toInstant(java.time.ZoneOffset.UTC))
  def offsetDateTime: Ordering[OffsetDateTime] = Ordering.by(_.toInstant)
  def  zonedDateTime: Ordering[ ZonedDateTime] = Ordering.by(_.toInstant)   
  
  // ---------------------------------------------------------------------------
  def byteBuffer    : Ordering[ByteBuffer]     = Ordering.by(_.array)
  def standAloneObj : Ordering[Obj]            = Ordering.by(_.formatCompactJson)

  // ---------------------------------------------------------------------------
  def enumEntry     : Ordering[EnumEntry]      = Ordering.by(_.entryName)
}

// ===========================================================================
