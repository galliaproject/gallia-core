package gallia.heads.reducing

import aptus.Anything_

import gallia._

// ===========================================================================
sealed trait ReducingDataTriplet {
    val key: Key
    def data(values: Values): AnyValue

    // ---------------------------------------------------------------------------
    final def dataEntry(values: Values): (Key, AnyValue) = key -> data(values)
  }

  // ===========================================================================
  // note: optional is only for stats... pass Fld if need more?

  case class ReducingDataTriplet1(key: Key, rtipe: ReducingType, optional: Boolean, ntipeOpt: Option[NumericalType]) extends ReducingDataTriplet {
    def data(values: Values): AnyValue = rtipe.data(optional, ntipeOpt)(values) }

  case class ReducingDataTripletN(key: Key, rtipes: Seq[ReducingType], optional: Boolean, ntipeOpt: Option[NumericalType]) extends ReducingDataTriplet {
    final def data(values: Values): AnyValue = rtipes.map { rtipe => rtipe.defaultKey -> rtipe.data(optional, ntipeOpt)(values) }.pipe(gallia.obj) }

  // ===========================================================================
  case class ReducingDataTriplet1s(values: Seq[ReducingDataTriplet1]) {
    def keys = values.map(_.key).pipe(Keyz.apply)
  }

// ===========================================================================
