package gallia.io.in

import aptus.Anything_
import aptus.MirrorIndex

import gallia._

// ===========================================================================
case class MIndexEntry(mindex: MirrorIndex, keyOpt: Option[Key]) { // TODO: t210122135629 - implement mirror index

      def resolve(allKeys: Keyz): Ren =
        allKeys.values.apply(mindex)
          .thn { fromKey =>
            keyOpt match {
              case None        => Ren(fromKey, fromKey)
              case Some(toKey) => Ren(fromKey,   toKey) } }

    }

    // ===========================================================================
    object MIndexEntry {
      implicit def toI(value: MirrorIndex): MIndexEntry = MIndexEntry(value, None)

      implicit def toK(pair: (MirrorIndex,  Key)): MIndexEntry = MIndexEntry(pair._1, Some(pair._2))
      implicit def toS(pair: (MirrorIndex, SKey)): MIndexEntry = toW((pair._1, pair._2))
      implicit def toU(pair: (MirrorIndex, UKey)): MIndexEntry = toW((pair._1, pair._2))
      implicit def toE(pair: (MirrorIndex, EKey)): MIndexEntry = toW((pair._1, pair._2))

      implicit def toW(pair: (MirrorIndex, KeyW)): MIndexEntry = MIndexEntry(pair._1, Some(pair._2.value))
    }

  // ===========================================================================
  case class MIndexEntries(values: Seq[MIndexEntry])

    // ---------------------------------------------------------------------------
    object MIndexEntries {
      implicit def toI(values: Seq[MirrorIndex]): MIndexEntries = values.map(MIndexEntry.toI).thn(MIndexEntries.apply)

      implicit def toK(values: Seq[(MirrorIndex,   Key)]): MIndexEntries = values.map(MIndexEntry.toK).thn(MIndexEntries.apply)
      implicit def toS(values: Seq[(MirrorIndex,  SKey)]): MIndexEntries = values.map(MIndexEntry.toS).thn(MIndexEntries.apply)
      implicit def toU(values: Seq[(MirrorIndex,  UKey)]): MIndexEntries = values.map(MIndexEntry.toU).thn(MIndexEntries.apply)
      implicit def toE(values: Seq[(MirrorIndex,  EKey)]): MIndexEntries = values.map(MIndexEntry.toE).thn(MIndexEntries.apply)

      implicit def toW(values: Seq[(MirrorIndex, KeyW)]): MIndexEntries = values.map(MIndexEntry.toW).thn(MIndexEntries.apply)
    }

// ===========================================================================
