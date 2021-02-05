package gallia.heads.pivoting

import gallia._

// ===========================================================================
object Data { val InitValue = null /* for now... */ }
  import Data.InitValue
  import gallia.selection.typed.TsBoilerplate.Squash.TSelector // see 200924162200

  // ===========================================================================
  private[pivoting] case class Data1[O1: WTT, D: WTT](
      target1: TSelector[O1],

      aggOpt   : Option[Seq[O1] => D] = None /* can only be missing for N = 1 in DataN */,
      rows     : Renz = InitValue,
      columns  : Keyz = InitValue,
      newKeys  : Keyz = InitValue)

  // ---------------------------------------------------------------------------
  private[pivoting] case class Data2[O1: WTT, O2: WTT, D: WTT](
      target1: TSelector[O1], target2: TSelector[O2],

      agg      : Seq[(O1, O2)] => D = InitValue,

      // TODO: common interface for those three
      rows     : Renz = InitValue,
      columns  : Keyz = InitValue,
      newKeys  : Keyz = InitValue)

  // ---------------------------------------------------------------------------
  private[pivoting] case class Data3[O1: WTT, O2: WTT, O3: WTT, D: WTT](
      target1: TSelector[O1], target2: TSelector[O2], target3: TSelector[O3],

      agg      : Seq[(O1, O2, O3)] => D = InitValue,
      rows     : Renz = InitValue,
      columns  : Keyz = InitValue,
      newKeys  : Keyz = InitValue)

  // ---------------------------------------------------------------------------
  private[pivoting] case class Data4[O1: WTT, O2: WTT, O3: WTT, O4: WTT, D: WTT](
      target1: TSelector[O1], target2: TSelector[O2], target3: TSelector[O3], target4: TSelector[O4],

      agg      : Seq[(O1, O2, O3, O4)] => D = InitValue,

      rows     : Renz = InitValue,
      columns  : Keyz = InitValue,
      newKeys  : Keyz = InitValue)

  // ---------------------------------------------------------------------------
  private[pivoting] case class Data5[O1: WTT, O2: WTT, O3: WTT, O4: WTT, O5: WTT, D: WTT](
      target1  : TSelector[O1], target2  : TSelector[O2], target3  : TSelector[O3], target4  : TSelector[O4], target5  : TSelector[O5],

      agg      : Seq[(O1, O2, O3, O4, O5)] => D = InitValue,

      rows     : Renz = InitValue,
      columns  : Keyz = InitValue,
      newKeys  : Keyz = InitValue)

// ===========================================================================
