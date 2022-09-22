package gallia
package heads
package pivoting

import aptus.Separator
import reflect.ReflectUtils

import DataN._

// ===========================================================================
@Max5
@deprecated final class PivotingHelperN(head: HeadZ) {
  private type P[T] = gallia.heads.TSL.Squash.TSelector[T] // 200924162200
  private type WV   = gallia.Whatever   
  
  // ===========================================================================
    // TODO: keep these or expect preproc?
//  @Max5 protected val _pivot = new PivotingHelper(self)

/*
    def pivot[O1: WTT, O2: WTT                           ](x1: Pivot[O1], x2: Pivot[O2]                                             ) = _pivot.pivot2(x1, x2)
    def pivot[O1: WTT, O2: WTT, O3: WTT                  ](x1: Pivot[O1], x2: Pivot[O2], x3: Pivot[O3]                              ) = _pivot.pivot3(x1, x2, x3)
    def pivot[O1: WTT, O2: WTT, O3: WTT, O4: WTT         ](x1: Pivot[O1], x2: Pivot[O2], x3: Pivot[O3], x4: Pivot[O4]               ) = _pivot.pivot4(x1, x2, x3, x4)
    def pivot[O1: WTT, O2: WTT, O3: WTT, O4: WTT, O5: WTT](x1: Pivot[O1], x2: Pivot[O2], x3: Pivot[O3], x4: Pivot[O4], x5: Pivot[O5]) = _pivot.pivot5(x1, x2, x3, x4, x5)
*/
  
  // ---------------------------------------------------------------------------
  def pivot2[O1: WTT, O2: WTT                           ](x1: P[O1], x2: P[O2]                                 ) = new Agg2[O1, O2            ](Data2[O1, O2,             Nothing](x1, x2))
  def pivot3[O1: WTT, O2: WTT, O3: WTT                  ](x1: P[O1], x2: P[O2], x3: P[O3]                      ) = new Agg3[O1, O2, O3        ](Data3[O1, O2, O3,         Nothing](x1, x2, x3))
  def pivot4[O1: WTT, O2: WTT, O3: WTT, O4: WTT         ](x1: P[O1], x2: P[O2], x3: P[O3], x4: P[O4]           ) = new Agg4[O1, O2, O3, O4    ](Data4[O1, O2, O3, O4,     Nothing](x1, x2, x3, x4))
  def pivot5[O1: WTT, O2: WTT, O3: WTT, O4: WTT, O5: WTT](x1: P[O1], x2: P[O2], x3: P[O3], x4: P[O4], x5: P[O5]) = new Agg5[O1, O2, O3, O4, O5](Data5[O1, O2, O3, O4, O5, Nothing](x1, x2, x3, x4, x5))
  
  // ===========================================================================
  //TODO: t210124092716 - codegen (very boilerplaty)

  class Agg2[O1: WTT, O2: WTT] private[pivoting] (data: Data2[O1, O2, Nothing]) {
    def noAggregation = using(x => x)

    def using[D: WTT](f: Seq[(O1, O2)] => D) =
      new Rows2[O1, O2, D](Data2[O1, O2, D](data.target1, data.target2, f)) }

    class Rows2[O1: WTT, O2: WTT, D: WTT] private[pivoting] (data: Data2[O1, O2, D]) {
        def rows(target: RenW)                             : Columns2[O1, O2, D] = rows(Renz.from(target))
        def rows(target1: RenW, target2: RenW, more: RenW*): Columns2[O1, O2, D] = rows(Renz.from(target1, target2, more))
        def rows(targets: RenWz)                           : Columns2[O1, O2, D] = new Columns2[O1, O2, D](data.copy(rows = targets.renz)) }

      class Columns2[O1: WTT, O2: WTT, D: WTT] private[pivoting] (data: Data2[O1, O2, D]) {
          def column (target: KeyW)                             : NewKeys1b[O1, O2, D] = new NewKeys1b(data.copy(columns = target.keyz))
          def columns(target1: KeyW, target2: KeyW, more: KeyW*): NewKeys2b[O1, O2, D] = columns(Keyz.from(target1, target2, more))
          def columns(targets: KeyWz)                           : NewKeys2b[O1, O2, D] = new NewKeys2b(data.copy(columns = targets.keyz)) }

        class NewKeys1b[O1: WTT, O2: WTT, D: WTT] private[pivoting] (data: Data2[O1, O2, D]) {
            def asNewKeys[E <: EnumEntry : WTT]                   : HeadZ = asNewKeys(ReflectUtils.enumValueNames[E])
            def asNewKeys(value : KeyW)                           : HeadZ = asNewKeys(Keyz.from(value))
            def asNewKeys(value1: KeyW, value2: KeyW, more: KeyW*): HeadZ = asNewKeys(Keyz.from(value1, value2, more))
            def asNewKeys(values: KeyWz)                          : HeadZ = PivotingCombosN.apply2(head, data.copy(newKeys = values.keyz))
        }
//to max 5
        class NewKeys2b[O1: WTT, O2: WTT, D: WTT] private[pivoting] (data: Data2[O1, O2, D]) {
            def asNewKeys[E <: EnumEntry : WTT]                   : KeySeparator2[O1, O2, D] = asNewKeys(ReflectUtils.enumValueNames[E])
            def asNewKeys(value : KeyW)                           : KeySeparator2[O1, O2, D] = asNewKeys(Keyz.from(value))
            def asNewKeys(value1: KeyW, value2: KeyW, more: KeyW*): KeySeparator2[O1, O2, D] = asNewKeys(Keyz.from(value1, value2, more))
            def asNewKeys(values: KeyWz)                          : KeySeparator2[O1, O2, D] = new KeySeparator2(data.copy(newKeys = values.keyz))

            //def asNewKeys0(value : KeyW)                             : HeadZ = asNewKeys0(Keyz.from(value))
            //def asNewKeys0(value1: KeyW, value2: KeyW, more: KeyW*): HeadZ = asNewKeys0(Keyz.from(value1, value2, more))
            //def asNewKeys0(values: KeyWz)                            : HeadZ = PivotingCombosN.apply2(head, data.copy(newKeys = values.keyz))
            //def asNewKeys2(value : KeyW)                             : KeySeparator2[O1, O2, D] = asNewKeys2(Keyz.from(value))
            //def asNewKeys2(value1: KeyW, value2: KeyW, more: KeyW*): KeySeparator2[O1, O2, D] = asNewKeys2(Keyz.from(value1, value2, more))
            //def asNewKeys2(values: KeyWz)                            : KeySeparator2[O1, O2, D] = new KeySeparator2(data.copy(newKeys = values.keyz))
        }

          // TODO: make that optional, like "as" (see t210116192032)
          // TODO: t210228112738 - keep these or force manual fuse ahead of time?        
          class KeySeparator2[O1: WTT, O2: WTT, D: WTT] private[pivoting] (data: Data2[O1, O2, D]) {
            def withDefaultKeySeparator           : HeadZ = withKeySeparator("_")
            def withKeySeparator(value: Separator): HeadZ = PivotingCombosN.apply2(head, data.copy()) } // FIXME: use separator

  // ---------------------------------------------------------------------------
  class Agg3[O1: WTT, O2: WTT, O3: WTT] private[pivoting] (data: Data3[O1, O2, O3, Nothing]) {
      def using[D: WTT](f: Seq[(O1, O2, O3)] => D) =
        new Rows3[O1, O2, O3, D](Data3[O1, O2, O3, D](data.target1, data.target2, data.target3, f)) }

    class Rows3[O1: WTT, O2: WTT, O3: WTT, D: WTT] private[pivoting] (data: Data3[O1, O2, O3, D]) {
        def rows(target: RenW)                             : Columns3[O1, O2, O3, D] = rows(Renz.from(target))
        def rows(target1: RenW, target2: RenW, more: RenW*): Columns3[O1, O2, O3, D] = rows(Renz.from(target1, target2, more))
        def rows(targets: RenWz)                           : Columns3[O1, O2, O3, D] = new Columns3[O1, O2, O3, D](data.copy(rows = targets.renz)) }

      class Columns3[O1: WTT, O2: WTT, O3: WTT, D: WTT] private[pivoting] (data: Data3[O1, O2, O3, D]) {
          def columns(target: KeyW)                             : NewKeys3[O1, O2, O3, D] = columns(Keyz.from(target))
          def columns(target1: KeyW, target2: KeyW, more: KeyW*): NewKeys3[O1, O2, O3, D] = columns(Keyz.from(target1, target2, more))
          def columns(targets: KeyWz)                           : NewKeys3[O1, O2, O3, D] = new NewKeys3(data.copy(columns = targets.keyz)) }

        class NewKeys3[O1: WTT, O2: WTT, O3: WTT, D: WTT] private[pivoting] (data: Data3[O1, O2, O3, D]) {
            def asNewKeys[E <: EnumEntry : WTT]                   : HeadZ = asNewKeys(ReflectUtils.enumValueNames[E])
            def asNewKeys(value : KeyW)                           : HeadZ = asNewKeys(Keyz.from(value))
            def asNewKeys(value1: KeyW, value2: KeyW, more: KeyW*): HeadZ = asNewKeys(Keyz.from(value1, value2, more))
            def asNewKeys(values: KeyWz)                          : HeadZ = PivotingCombosN.apply3(head, data.copy(newKeys = values.keyz)) }

  // ---------------------------------------------------------------------------
  class Agg4[O1: WTT, O2: WTT, O3: WTT, O4: WTT] private[pivoting] (data: Data4[O1, O2, O3, O4, Nothing]) {
      def using[D: WTT](f: Seq[(O1, O2, O3, O4)] => D) =
        new Rows4[O1, O2, O3, O4, D](Data4[O1, O2, O3, O4, D](data.target1, data.target2, data.target3, data.target4, f)) }

    class Rows4[O1: WTT, O2: WTT, O3: WTT, O4: WTT, D: WTT] private[pivoting] (data: Data4[O1, O2, O3, O4, D]) {
        def rows(target: RenW)                             : Columns4[O1, O2, O3, O4, D] = rows(Renz.from(target))
        def rows(target1: RenW, target2: RenW, more: RenW*): Columns4[O1, O2, O3, O4, D] = rows(Renz.from(target1, target2, more))
        def rows(targets: RenWz)                           : Columns4[O1, O2, O3, O4, D] = new Columns4[O1, O2, O3, O4, D](data.copy(rows = targets.renz)) }

      class Columns4[O1: WTT, O2: WTT, O3: WTT, O4: WTT, D: WTT] private[pivoting] (data: Data4[O1, O2, O3, O4, D]) {
          def columns(target: KeyW)                             : NewKeys4[O1, O2, O3, O4, D] = columns(Keyz.from(target))
          def columns(target1: KeyW, target2: KeyW, more: KeyW*): NewKeys4[O1, O2, O3, O4, D] = columns(Keyz.from(target1, target2, more))
          def columns(targets: KeyWz)                           : NewKeys4[O1, O2, O3, O4, D] = new NewKeys4(data.copy(columns = targets.keyz)) }

        class NewKeys4[O1: WTT, O2: WTT, O3: WTT, O4: WTT, D: WTT] private[pivoting] (data: Data4[O1, O2, O3, O4, D]) {
            def asNewKeys[E <: EnumEntry : WTT]                   : HeadZ = asNewKeys(ReflectUtils.enumValueNames[E])
            def asNewKeys(value : KeyW)                           : HeadZ = asNewKeys(Keyz.from(value))
            def asNewKeys(value1: KeyW, value2: KeyW, more: KeyW*): HeadZ = asNewKeys(Keyz.from(value1, value2, more))
            def asNewKeys(values: KeyWz)                          : HeadZ = PivotingCombosN.apply4(head, data.copy(newKeys = values.keyz)) }

  // ---------------------------------------------------------------------------
  class Agg5[O1: WTT, O2: WTT, O3: WTT, O4: WTT, O5: WTT] private[pivoting] (data: Data5[O1, O2, O3, O4, O5, Nothing]) {
      def using[D: WTT](f: Seq[(O1, O2, O3, O4, O5)] => D) =
        new Rows5[O1, O2, O3, O4, O5, D](Data5[O1, O2, O3, O4, O5, D](data.target1, data.target2, data.target3, data.target4, data.target5, f)) }

    class Rows5[O1: WTT, O2: WTT, O3: WTT, O4: WTT, O5: WTT, D: WTT] private[pivoting] (data: Data5[O1, O2, O3, O4, O5, D]) {
        def rows(target: RenW)                             : Columns5[O1, O2, O3, O4, O5, D] = rows(Renz.from(target))
        def rows(target1: RenW, target2: RenW, more: RenW*): Columns5[O1, O2, O3, O4, O5, D] = rows(Renz.from(target1, target2, more))
        def rows(targets: RenWz)                           : Columns5[O1, O2, O3, O4, O5, D] = new Columns5[O1, O2, O3, O4, O5, D](data.copy(rows = targets.renz)) }

      class Columns5[O1: WTT, O2: WTT, O3: WTT, O4: WTT, O5: WTT, D: WTT] private[pivoting] (data: Data5[O1, O2, O3, O4, O5, D]) {
          def columns(target: KeyW)                             : NewKeys5[O1, O2, O3, O4, O5, D] = columns(Keyz.from(target))
          def columns(target1: KeyW, target2: KeyW, more: KeyW*): NewKeys5[O1, O2, O3, O4, O5, D] = columns(Keyz.from(target1, target2, more))
          def columns(targets: KeyWz)                           : NewKeys5[O1, O2, O3, O4, O5, D] = new NewKeys5(data.copy(columns = targets.keyz)) }

        class NewKeys5[O1: WTT, O2: WTT, O3: WTT, O4: WTT, O5: WTT, D: WTT] private[pivoting] (data: Data5[O1, O2, O3, O4, O5, D]) {
            def asNewKeys[E <: EnumEntry : WTT]                   : HeadZ = asNewKeys(ReflectUtils.enumValueNames[E])
            def asNewKeys(value : KeyW)                           : HeadZ = asNewKeys(Keyz.from(value))
            def asNewKeys(value1: KeyW, value2: KeyW, more: KeyW*): HeadZ = asNewKeys(Keyz.from(value1, value2, more))
            def asNewKeys(values: KeyWz)                          : HeadZ = PivotingCombosN.apply5(head, data.copy(newKeys = values.keyz)) }

}

// ===========================================================================
@deprecated object DataN { val InitValue = null /* for now... */ 
  import gallia.selection.typed.TsBoilerplate.Squash.TSelector // see 200924162200
  
  //  // ===========================================================================
  //  private[pivoting] case class Data1[O1: WTT, D: WTT](
  //      target1: TSelector[O1],
  //
  //      aggOpt   : Option[Seq[O1] => D] = None /* can only be missing for N = 1 in DataN */,
  //      rows     : Renz = InitValue,
  //      columns  : Keyz = InitValue,
  //      newKeys  : Keyz = InitValue)

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
}

// ===========================================================================
@deprecated private[pivoting] object PivotingCombosN {
  val DefaultKeySeparator = "_" //TODO: t210117192221 - configurable key separator

  //  // ---------------------------------------------------------------------------
  //  def apply1[O1: WTT, D: WTT](self: HeadZ, data: Data1[O1, D]): HeadZ =
  //    _pivoting(self)(data.rows, data.columns, data.newKeys) { key =>
  //      headZ =>
  //        data.aggOpt match {
  //          case None      => headZ.unnestOOO(key)
  //          case Some(agg) =>
  //            headZ
  //              .transform(_.objz(key)).using {
  //              _.squash(data.target1) // compatible because of 200924162200
  //                .using(agg) } } }

  // ---------------------------------------------------------------------------
  def apply2[O1: WTT, O2: WTT, D: WTT](self: HeadZ, data: Data2[O1, O2, D]): HeadZ =
    _pivoting(self)(data.rows, data.columns, data.newKeys) { key =>
      _.transform(_.objz(key)).using {
        _.squash(data.target1, data.target2) // compatible because of 200924162200
          .using(data.agg) } }

  // ---------------------------------------------------------------------------
  def apply3[O1: WTT, O2: WTT, O3: WTT, D: WTT](self: HeadZ, data: Data3[O1, O2, O3, D]): HeadZ =
    _pivoting(self)(data.rows, data.columns, data.newKeys) { key =>
      _.transform(_.objz(key)).using {
        _.squash(data.target1, data.target2, data.target3) // compatible because of 200924162200
          .using(data.agg) } }

  // ---------------------------------------------------------------------------
  def apply4[O1: WTT, O2: WTT, O3: WTT, O4: WTT, D: WTT](self: HeadZ, data: Data4[O1, O2, O3, O4, D]): HeadZ =
    _pivoting(self)(data.rows, data.columns, data.newKeys) { key =>
      _.transform(_.objz(key)).using {
        _.squash(data.target1, data.target2, data.target3, data.target4) // compatible because of 200924162200
          .using(data.agg) } }

  // ---------------------------------------------------------------------------
  def apply5[O1: WTT, O2: WTT, O3: WTT, O4: WTT, O5: WTT, D: WTT](self: HeadZ, data: Data5[O1, O2, O3, O4, O5, D]): HeadZ =
    _pivoting(self)(data.rows, data.columns, data.newKeys) { key =>
      _.transform(_.objz(key)).using {
        _.squash(data.target1, data.target2, data.target3, data.target4, data.target5) // compatible because of 200924162200
          .using(data.agg) } }

  // ===========================================================================
  private[heads] def _pivoting(self: HeadZ)
        (rows    : Renz,
         columns : Keyz,
         newKeys : Keyz)
        (agg: Key => HeadZ => HeadZ)
      : HeadZ = {
    val keySeparator = DefaultKeySeparator

    self // TODO: use cascade group by rather (once done)
      .groupBy(rows).as( _tmp1)
      .transform(_.objz(_tmp1)).using {
        _ .groupBy(columns).as(_tmp2) // 200930125015 - this flattens, so must set defaults ahead of time if needed
          .pipe(agg(            _tmp2))
          .unarrayEntries(columns)
            .asNewKeys(newKeys.values)
                .withKeySeparator(keySeparator)
              .valueKey(_tmp2) }
      .unnestAllFrom(_tmp1)
  }

}

// ===========================================================================
