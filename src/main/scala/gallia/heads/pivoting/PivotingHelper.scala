package gallia.heads.pivoting

import aptus.Seq_
import aptus.Separator

import gallia._

// ===========================================================================
@Max5
final class PivotingHelper(head: HeadZ) {
  private type P[T] = gallia.heads.TSL.Squash.TSelector[T] // 200924162200
  private type WV   = gallia.Whatever

  // ---------------------------------------------------------------------------
  def pivot1[O1: WTT](x1: P[O1]) = new Agg1[O1](Data1[O1, Nothing](x1))

    def pivot2[O1: WTT, O2: WTT                           ](x1: P[O1], x2: P[O2]                                 ) = new Agg2[O1, O2            ](Data2[O1, O2,             Nothing](x1, x2))
    def pivot3[O1: WTT, O2: WTT, O3: WTT                  ](x1: P[O1], x2: P[O2], x3: P[O3]                      ) = new Agg3[O1, O2, O3        ](Data3[O1, O2, O3,         Nothing](x1, x2, x3))
    def pivot4[O1: WTT, O2: WTT, O3: WTT, O4: WTT         ](x1: P[O1], x2: P[O2], x3: P[O3], x4: P[O4]           ) = new Agg4[O1, O2, O3, O4    ](Data4[O1, O2, O3, O4,     Nothing](x1, x2, x3, x4))
    def pivot5[O1: WTT, O2: WTT, O3: WTT, O4: WTT, O5: WTT](x1: P[O1], x2: P[O2], x3: P[O3], x4: P[O4], x5: P[O5]) = new Agg5[O1, O2, O3, O4, O5](Data5[O1, O2, O3, O4, O5, Nothing](x1, x2, x3, x4, x5))

  // ===========================================================================
  //TODO: t210124092716 - codegen (very boilerplaty)

  class Agg1[O1: WTT] private[pivoting] (data: Data1[O1, Nothing]) {

        // only available for Agg1, not Agg > 1; would be nice if could hide if Whatever (t210117192437 - the opposite of <:< basically)
        // alternatively do a separate version for whatever
        def noAggregation                    = new Rows1[O1, Seq[O1]](Data1[O1, Seq[O1]](data.target1, None))
        def using[D: WTT](agg: Seq[O1] => D) = new Rows1[O1,     D  ](Data1[O1,     D  ](data.target1, Some(agg)))

        def usingMean(implicit num: Numeric[O1]): Rows1[O1, Double] = using(_.mean)
        //TODO: usingMedian, ..., ... (see reducer)

        // ---------------------------------------------------------------------------
        // whatever shortcut
        def rows(target: RenW)                             (implicit ev: O1 <:< WV) : Columns1[WV, WV] = rows(Renz.from(target))
        def rows(target1: RenW, target2: RenW, more: RenW*)(implicit ev: O1 <:< WV) : Columns1[WV, WV] = rows(Renz.from(target1, target2, more))
        def rows(targets: RenWz)                           (implicit ev: O1 <:< WV) : Columns1[WV, WV] =
          new Columns1[WV, WV](Data1[WV, WV](data.target1.asInstanceOf[P[WV]], rows = targets.renz) )
      }

      // ---------------------------------------------------------------------------
      class Rows1[O1: WTT, D: WTT] private[pivoting] (data: Data1[O1, D]) {
          def rows(target: RenW)                             : Columns1[O1, D] = rows(Renz.from(target))
          def rows(target1: RenW, target2: RenW, more: RenW*): Columns1[O1, D] = rows(Renz.from(target1, target2, more))
          def rows(targets: RenWz)                           : Columns1[O1, D] = new Columns1[O1, D](data.copy(rows = targets.renz)) }

        class Columns1[O1: WTT, D: WTT] private[pivoting] (data: Data1[O1, D]) {
            def columns(target: KeyW)                             : NewKeys1[O1, D] = columns(Keyz.from(target))
            def columns(target1: KeyW, target2: KeyW, more: KeyW*): NewKeys1[O1, D] = columns(Keyz.from(target1, target2, more))
            def columns(targets: KeyWz)                           : NewKeys1[O1, D] = new NewKeys1(data.copy(columns = targets.keyz)) }

        class NewKeys1[O1: WTT, D: WTT] private[pivoting] (data: Data1[O1, D]) {
            def asNewKeys(value : KeyW)                           : HeadZ = asNewKeys(Keyz.from(value))
            def asNewKeys(value1: KeyW, value2: KeyW, more: KeyW*): HeadZ = asNewKeys(Keyz.from(value1, value2, more))
            def asNewKeys(values: KeyWz)                          : HeadZ = PivotingCombos.apply1(head, data.copy(newKeys = values.keyz)) }

  // ===========================================================================
  class Agg2[O1: WTT, O2: WTT] private[pivoting] (data: Data2[O1, O2, Nothing]) {
    def using[D: WTT](f: Seq[(O1, O2)] => D) =
      new Rows2[O1, O2, D](Data2[O1, O2, D](data.target1, data.target2, f)) }

    class Rows2[O1: WTT, O2: WTT, D: WTT] private[pivoting] (data: Data2[O1, O2, D]) {
        def rows(target: RenW)                             : Columns2[O1, O2, D] = rows(Renz.from(target))
        def rows(target1: RenW, target2: RenW, more: RenW*): Columns2[O1, O2, D] = rows(Renz.from(target1, target2, more))
        def rows(targets: RenWz)                           : Columns2[O1, O2, D] = new Columns2[O1, O2, D](data.copy(rows = targets.renz)) }

      class Columns2[O1: WTT, O2: WTT, D: WTT] private[pivoting] (data: Data2[O1, O2, D]) {
          def columns(target: KeyW)                             : NewKeys2[O1, O2, D] = columns(Keyz.from(target))
          def columns(target1: KeyW, target2: KeyW, more: KeyW*): NewKeys2[O1, O2, D] = columns(Keyz.from(target1, target2, more))
          def columns(targets: KeyWz)                           : NewKeys2[O1, O2, D] = new NewKeys2(data.copy(columns = targets.keyz)) }

        class NewKeys2[O1: WTT, O2: WTT, D: WTT] private[pivoting] (data: Data2[O1, O2, D]) {
            def asNewKeys(value : KeyW)                           : KeySeparator2[O1, O2, D] = asNewKeys(Keyz.from(value))
            def asNewKeys(value1: KeyW, value2: KeyW, more: KeyW*): KeySeparator2[O1, O2, D] = asNewKeys(Keyz.from(value1, value2, more))
            def asNewKeys(values: KeyWz)                          : KeySeparator2[O1, O2, D] = new KeySeparator2(data.copy(newKeys = values.keyz))

            //def asNewKeys0(value : KeyW)                             : HeadZ = asNewKeys0(Keyz.from(value))
            //def asNewKeys0(value1: KeyW, value2: KeyW, more: KeyW*): HeadZ = asNewKeys0(Keyz.from(value1, value2, more))
            //def asNewKeys0(values: KeyWz)                            : HeadZ = PivotingCombos.apply2(head, data.copy(newKeys = values.keyz))
            //def asNewKeys2(value : KeyW)                             : KeySeparator2[O1, O2, D] = asNewKeys2(Keyz.from(value))
            //def asNewKeys2(value1: KeyW, value2: KeyW, more: KeyW*): KeySeparator2[O1, O2, D] = asNewKeys2(Keyz.from(value1, value2, more))
            //def asNewKeys2(values: KeyWz)                            : KeySeparator2[O1, O2, D] = new KeySeparator2(data.copy(newKeys = values.keyz))
        }

          // TODO: make that optional, like "as" (see t210116192032)
          class KeySeparator2[O1: WTT, O2: WTT, D: WTT] private[pivoting] (data: Data2[O1, O2, D]) {
            def withDefaultKeySeparator           : HeadZ = withKeySeparator("_")
            def withKeySeparator(value: Separator): HeadZ = PivotingCombos.apply2(head, data.copy()) }

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
            def asNewKeys(value : KeyW)                           : HeadZ = asNewKeys(Keyz.from(value))
            def asNewKeys(value1: KeyW, value2: KeyW, more: KeyW*): HeadZ = asNewKeys(Keyz.from(value1, value2, more))
            def asNewKeys(values: KeyWz)                          : HeadZ = PivotingCombos.apply3(head, data.copy(newKeys = values.keyz)) }

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
            def asNewKeys(value : KeyW)                           : HeadZ = asNewKeys(Keyz.from(value))
            def asNewKeys(value1: KeyW, value2: KeyW, more: KeyW*): HeadZ = asNewKeys(Keyz.from(value1, value2, more))
            def asNewKeys(values: KeyWz)                          : HeadZ = PivotingCombos.apply4(head, data.copy(newKeys = values.keyz)) }

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
            def asNewKeys(value : KeyW)                           : HeadZ = asNewKeys(Keyz.from(value))
            def asNewKeys(value1: KeyW, value2: KeyW, more: KeyW*): HeadZ = asNewKeys(Keyz.from(value1, value2, more))
            def asNewKeys(values: KeyWz)                          : HeadZ = PivotingCombos.apply5(head, data.copy(newKeys = values.keyz)) }

}

// ===========================================================================
