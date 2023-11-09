package gallia
package heads
package pivoting

import aptus.{String_, Seq_}
import heads.common.Pivot

// ===========================================================================
trait HeadZPivoting { self: HeadZ =>
  //TODO: t210110094829 - opaque nesting: accept Obj as value (standalone version), so may not provide newKeys when unwanted

  def pivoneEnum[E <: EnumEntry : WTT](k: KeyW): HeadU = pivone(k).asNewKeys[E]
  def pivoneEnum[E <: EnumEntry : WTT]         : HeadU = pivone(typeNode[E].leaf.fullName.inScopeName).asNewKeys[E]

  // ---------------------------------------------------------------------------
  def pivone(k: KeyW) = new Pivone(k)

    class Pivone (k: KeyW) {
      def asNewKeys[E <: EnumEntry : WTT]     : HeadU = asNewKeys(low.enumValueNames[E])
      def asNewKeys(value1: KeyW, more: KeyW*): HeadU = asNewKeys(Keyz.from(value1, more))
      def asNewKeys(values: KeyWz)            : HeadU = self.groupBy(k.value).as(_tmp).pivot(_tmp).column(k.value).asNewKeys(values) }

  // ===========================================================================
  def pivot         (x1: KPathW)    = new PivotStartU    (PivotingData[WV, Nothing](_._explicit(x1) /* TODO: see t210303111953 */))
  def pivot[O1: WTT](x1: Pivot[O1]) = new PivotStartT[O1](PivotingData[O1, Nothing](x1))
  
  // ===========================================================================
  class PivotStartU private[pivoting] (data: PivotingData[WV, Nothing] /* TODO: t210303111953 - use different structure now */) {
      def column(target: KeyW): NewKeysU[WV] = new NewKeysU(data.copy(column = target.value))
  
      // ---------------------------------------------------------------------------
      //TODO: keep or just expect pre-grouping?      
      def rows(target: RenW)                             : Columns[WV, WV] = rows(Renz.from(target))
      def rows(target1: RenW, target2: RenW, more: RenW*): Columns[WV, WV] = rows(Renz.from(target1, target2, more))
      def rows(targets: RenWz)                           : Columns[WV, WV] = new Columns[WV, WV](PivotingData[WV, WV](data.target.asInstanceOf[Pivot[WV]], rows = targets.renz) ) }
    
    // ===========================================================================  
    class NewKeysU[T: WTT] private[pivoting] (data: PivotingData[WV, Nothing]) {
      def asNewKeys[E <: EnumEntry : WTT]     : HeadU = asNewKeys(low.enumValueNames[E])
      def asNewKeys(value1: KeyW, more: KeyW*): HeadU = asNewKeys(Keyz.from(value1, more))
      def asNewKeys(values: KeyWz)            : HeadU = data.copy(newKeys = values.keyz).pivone(self) }

  // ===========================================================================  
  class PivotStartT[O1: WTT] private[pivoting] (data: PivotingData[O1, Nothing]) {    
    def noAggregation                       : Rows[O1, Seq[O1]] = new Rows[O1, Seq[O1]](PivotingData[O1, Seq[O1]](data.target, None))      
    def using[D: WTT](agg: Seq[O1] => D)    : Rows[O1, D]       = new Rows[O1,     D  ](PivotingData[O1,     D  ](data.target, Some(agg)))
    def usingCount                          : Rows[O1, Int]     = using(_.size)
    def usingSum (implicit num: Numeric[O1]): Rows[O1, O1]      = using(_.sum)
    def usingMean(implicit num: Numeric[O1]): Rows[O1, Double]  = using(_.mean)
    //TODO: usingMedian, ..., ... (see reducer)
  }

  // ===========================================================================
  class Rows[O1: WTT, D: WTT] private[pivoting] (data: PivotingData[O1, D]) {
      def rows(target: RenW)                             : Columns[O1, D] = rows(Renz.from(target))
      def rows(target1: RenW, target2: RenW, more: RenW*): Columns[O1, D] = rows(Renz.from(target1, target2, more))
      def rows(targets: RenWz)                           : Columns[O1, D] = new Columns[O1, D](data.copy(rows = targets.renz)) }

    // ---------------------------------------------------------------------------    
    class Columns[O1: WTT, D: WTT] private[pivoting] (data: PivotingData[O1, D]) {          
        def column(target: KeyW): NewKeysT[O1, D] = new NewKeysT(data.copy(column = target.value)) }

      // ---------------------------------------------------------------------------
      class NewKeysT[O1: WTT, D: WTT] private[pivoting] (data: PivotingData[O1, D]) {
          def asNewKeys[E <: EnumEntry : WTT]     : HeadZ = asNewKeys(low.enumValueNames[E])
          def asNewKeys(value1: KeyW, more: KeyW*): HeadZ = asNewKeys(Keyz.from(value1, more))
          def asNewKeys(values: KeyWz)            : HeadZ = data.copy(newKeys = values.keyz).pivot[O1, D](self) }

}

// ===========================================================================
