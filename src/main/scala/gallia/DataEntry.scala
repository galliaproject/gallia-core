package gallia

import aptus.String_

// ===========================================================================
/** for use by .obj(...) */ case class DataEntry(key: Key, value: AnyValue) { def pair = (key, value) }

  // ---------------------------------------------------------------------------
  object DataEntry {
    implicit def toDataEntryK(pair: ( Key , AnyValue)): DataEntry = DataEntry(pair._1                 , pair._2)
    implicit def toDataEntryS(pair: (SKey , AnyValue)): DataEntry = DataEntry(pair._1.symbol          , pair._2)
    implicit def toDataEntryU(pair: (UKey , AnyValue)): DataEntry = DataEntry(pair._1.entryName.symbol, pair._2)
    implicit def toDataEntryE(pair: (EKey , AnyValue)): DataEntry = DataEntry(pair._1.toString .symbol, pair._2)

    implicit def toDataEntry(pair: (KeyW, AnyValue)): DataEntry = DataEntry(pair._1.value            , pair._2)
  }

// ---------------------------------------------------------------------------
/** for use by .obj(...) */ case class DataEntryP(path: KPath, value: AnyValue)

// ===========================================================================