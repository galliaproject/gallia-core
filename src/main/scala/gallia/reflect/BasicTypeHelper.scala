package gallia.reflect

import gallia.AnyValue
import gallia.atoms.utils.SuperMetaPair

// ===========================================================================
private[reflect] trait BasicTypeHelper { _: BasicType =>

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
