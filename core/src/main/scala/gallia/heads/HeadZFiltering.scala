package gallia
package heads

import aptus.Anything_ // pipeIf
import actions.ActionsZZFiltering._

// ===========================================================================
trait HeadZFiltering { ignored: HeadZ => // pretty messy, need to find a cleaner way to keep find and filter more in sync

  // TODO: t201023083923 - add find/filter to HeadU as well, to make it an "option" (relates to t201021120753 - HeadOptionU)

  private type FilterBy1[T] = TSL.FilterBy1.TSelector[T]
  private type FilterByT[T] = TSL.FilterByT.TSelector[T]

  private type FindBy1  [T] = FilterBy1[T]
  private type FindByT  [T] = FilterByT[T]

  import TSL.FilterBy1._

  // ===========================================================================
  // must match filter below other than the "asFind" flag:
  // TODO: t201021120753 - offer proper optional head

                  // ===========================================================================  
                  def findUnsafe(pred: Obj => Boolean): Self = zz(FilterUnsafe(pred, asFind = true))

                  // ===========================================================================
                  def findByPresent(target: KPathW) = findBy(target).isPresent
                  def findByMissing(target: KPathW) = findBy(target).isMissing

                  // ---------------------------------------------------------------------------
                  def findBy        (target: KPathW)    : __FindBy1WV   = new __FindBy1WV (target)
                  def findBy[O: WTT](target: FindBy1[O]): __FindBy1[O]  = new __FindBy1[O](target)
                
                      // ---------------------------------------------------------------------------
                      //def findBy2(k: KeyW) = new {  } // eg "init" def findByInclusion[T: WTT](k: KeyW, coll: Iterable[T]): HeadZ
                      //  .findBy(_.string('SIG)).matches { _.containedIn(csts.SigSet) } }
                
                      class __FindBy1[O: WTT](target: FindBy1[O]) {
                        def hasValue(a: O): Self2 = matches(_ == a)
                        def notValue(a: O): Self2 = matches(_ != a)
                        def matches(f: O => Boolean): Self = zz(FilterByV(resolve(target), f, asFind = true)) }
                
                      // ---------------------------------------------------------------------------
                      class __FindBy1WV(target: KPathW) { import gallia.trgt.utils.TargetQueryUtils.tqkpath

                        def isPresent: Self = zz(FilterByPresence(tqkpath(target.value), negate = false, asFind = true))
                        def isMissing: Self = zz(FilterByPresence(tqkpath(target.value), negate = true,  asFind = true))

                        // ---------------------------------------------------------------------------
                        def matches(f: WV => TWV[Boolean])                 : Self = zz(FilterByWV(tqkpath(target.value), f(_).typed, asFind = true))
                        def matches(f: WV =>     Boolean) (implicit di: DI): Self = zz(FilterByWV(tqkpath(target.value), f,          asFind = true))
                
                        // ---------------------------------------------------------------------------
                        /** similar to SQL's WHERE X IN Y clause */ // TODO: toSet if big enough? use bloom if very big?
                        def    in[T:WTT](coll: Seq[T])       : Self = matches({ wv =>  coll.contains(wv.any) })
                        def notIn[T:WTT](coll: Seq[T])       : Self = matches({ wv => !coll.contains(wv.any) })
                        def    in[T:WTT](value1: T, more: T*): Self =    in(value1 +: more)
                        def notIn[T:WTT](value1: T, more: T*): Self = notIn(value1 +: more)

                        def hasValue(value: Any): Self2 = matches(_.any == value)
                        def notValue(value: Any): Self2 = matches(_.any != value)
                
                        def hasSizeString(n: Int): Self = matches(_.sizeString == n)
                        def hasSizeList  (n: Int): Self = matches(_.sizeList   == n)
                
                        // ---------------------------------------------------------------------------
                        def greaterThan     [N: Numeric](value: N): Self = matches(_ >  value.asInstanceOf[Number])
                        def greaterOrEqualTo[N: Numeric](value: N): Self = matches(_ >= value.asInstanceOf[Number])
                
                        def lessThan        [N: Numeric](value: N): Self = matches(_ <  value.asInstanceOf[Number])
                        def lessOrEqualTo   [N: Numeric](value: N): Self = matches(_ <= value.asInstanceOf[Number])

                        // ---------------------------------------------------------------------------
                        def isCurrentDate: Self = hasValue(java.time.LocalDate.now()) }

                      // ---------------------------------------------------------------------------
                      def findBy(target: FindBy1[HeadU])(implicit di: DI) = new _FindByU1(target) // trade-off: pre-process for more more than 1
                       final class _FindByU1 private[heads] (target: FindBy1[HeadU]) {
                        //TODO: add isPresent/...
                        def matches(f: HeadU => HeadV[Boolean]): Self =
                          zz(FilterByU(resolve(target), f, asFind = true)) }

                      // ---------------------------------------------------------------------------
                      def findBy(target: FindBy1[HeadZ])(implicit di: DI, di2: DI) = new _FindByU2(target) // trade-off: pre-process for more more than 1
                       final class _FindByU2 private[heads] (target: FindBy1[HeadZ]) {
                        def matches(f: HeadZ => HeadV[Boolean]): Self =
                          zz(FilterByZ(resolve(target), f, asFind = true)) }

                  // ===========================================================================
                  @Max5
                  def findBy(f1: KPathW, f2: KPathW)            : __FindBy2WV = new __FindBy2WV(f1, f2)
                  def findBy(f1: KPathW, f2: KPathW, f3: KPathW): __FindBy3WV = new __FindBy3WV(f1, f2, f3)
                
                    // ---------------------------------------------------------------------------
                    @Max5
                    def findBy[O1: WTT, O2: WTT         ](f1: FindByT[O1], f2: FindByT[O2])                 : _FindBy2[O1, O2]     = new _FindBy2(f1, f2)
                    def findBy[O1: WTT, O2: WTT, O3: WTT](f1: FindByT[O1], f2: FindByT[O2], f3: FindByT[O3]): _FindBy3[O1, O2, O3] = new _FindBy3(f1, f2, f3)
                
                    // ===========================================================================
                    class __FindBy2WV(target1: KPathW, target2: KPathW) { import gallia.trgt.utils.TargetQueryUtils.tqkpath2
                        def hasValues(value1: Any, value2: Any): Self = matches { (x, y) => x.any == value1 && y.any == value2 }
                        def notValues(value1: Any, value2: Any): Self = matches { (x, y) => x.any != value1 && y.any != value2 }
                    
                        // ---------------------------------------------------------------------------
                        def matches(f: (WV, WV) => TWV[Boolean])                 : Self = zz(FilterByWV2(tqkpath2(target1.value, target2.value), f(_, _).typed, asFind = true))
                        def matches(f: (WV, WV) =>     Boolean) (implicit di: DI): Self = zz(FilterByWV2(tqkpath2(target1.value, target2.value), f,             asFind = true)) }
                  
                      // ---------------------------------------------------------------------------
                      class __FindBy3WV(target1: KPathW, target2: KPathW, target3: KPathW) { import gallia.trgt.utils.TargetQueryUtils.tqkpath3
                        def hasValues(value1: Any, value2: Any, value3: Any): Self = matches { (x, y, z) => x.any == value1 && y.any == value2 && z.any == value3 }
                        def notValues(value1: Any, value2: Any, value3: Any): Self = matches { (x, y, z) => x.any != value1 && y.any != value2 && z.any != value3 }
                
                        // ---------------------------------------------------------------------------
                        def matches(f: (WV, WV, WV) => TWV[Boolean])                 : Self = zz(FilterByWV3(tqkpath3(target1.value, target2.value, target3.value), f(_, _, _).typed, asFind = true))
                        def matches(f: (WV, WV, WV) =>     Boolean) (implicit di: DI): Self = zz(FilterByWV3(tqkpath3(target1.value, target2.value, target3.value), f,                asFind = true)) }
                
                    // ===========================================================================  
                    class _FindBy2[O1: WTT, O2: WTT](f1: FindByT[O1], f2: FindByT[O2]) {
                      //TODO: areAllMissing/areAllPresent, ...
                      def matches(f: (O1, O2) => Boolean): Self =
                        zz(FilterByV2(resolve2(f1, f2), f, asFind = true)) }

                    // ---------------------------------------------------------------------------
                    class _FindBy3[O1: WTT, O2: WTT, O3: WTT](f1: FindByT[O1], f2: FindByT[O2], f3: FindByT[O3]) {
                      def matches(f: (O1, O2, O3) => Boolean): Self =
                        zz(FilterByV3(resolve3(f1, f2, f3), f, asFind = true)) }

  // ===========================================================================
  def filterUnsafe(pred: Obj => Boolean): Self = zz(FilterUnsafe(pred, asFind = false))

  // ===========================================================================
  def filterByPresent(target: KPathW) = filterBy(target).isPresent
  def filterByMissing(target: KPathW) = filterBy(target).isMissing

  // ---------------------------------------------------------------------------
  // TODO: t221004095843 - improve filterByAndRemove + .filterBy(...).matches(...).andRemove

  def filterByThenRemove(target: KPathW)      : __FilterBy1WV   = new __FilterBy1WV (target.value, alsoRemove = true)
  def filterBy          (target: KPathW)      : __FilterBy1WV   = new __FilterBy1WV (target.value, alsoRemove = false)
  def filterBy[O: WTT]  (target: FilterBy1[O]): __FilterBy1[O]  = new __FilterBy1[O](target)

      // ---------------------------------------------------------------------------
      //def filterBy2(k: KeyW) = new {  } // eg "init" def filterByInclusion[T: WTT](k: KeyW, coll: Iterable[T]): HeadZ
      //  .filterBy(_.string('SIG)).matches { _.containedIn(csts.SigSet) } }

      class __FilterBy1[O: WTT](target: FilterBy1[O]) {
        def hasValue(a: O): Self2 = matches(_ == a)
        def notValue(a: O): Self2 = matches(_ != a)
        def matches(f: O => Boolean): Self = zz(FilterByV(resolve(target), f, asFind = false)) }

      // ---------------------------------------------------------------------------
      class __FilterBy1WV(target: KPath, alsoRemove: Boolean) { import gallia.trgt.utils.TargetQueryUtils.tqkpath

        def isPresent: Self = zz(FilterByPresence(tqkpath(target), negate = false, asFind = false)).pipeIf(alsoRemove)(_.remove(target))
        def isMissing: Self = zz(FilterByPresence(tqkpath(target), negate = true,  asFind = false)).pipeIf(alsoRemove)(_.remove(target))

        // ---------------------------------------------------------------------------
        def matches(f: WV => TWV[Boolean])                 : Self = zz(FilterByWV(tqkpath(target), f(_).typed, asFind = false)).pipeIf(alsoRemove)(_.remove(target))
        def matches(f: WV =>     Boolean) (implicit di: DI): Self = zz(FilterByWV(tqkpath(target), f,          asFind = false)).pipeIf(alsoRemove)(_.remove(target))

        // ---------------------------------------------------------------------------
        /** similar to SQL's WHERE X IN Y clause */ // TODO: toSet if big enough? use bloom if very big?
        def    in[T:WTT](coll: Seq[T])       : Self = matches({ wv =>  coll.contains(wv.any) })
        def notIn[T:WTT](coll: Seq[T])       : Self = matches({ wv => !coll.contains(wv.any) })
        def    in[T:WTT](value1: T, more: T*): Self =    in(value1 +: more)
        def notIn[T:WTT](value1: T, more: T*): Self = notIn(value1 +: more)

        def hasValue(value: Any): Self2 = matches(_.any == value)
        def notValue(value: Any): Self2 = matches(_.any != value)

        def hasSizeString(n: Int): Self = matches(_.sizeString == n)
        def hasSizeList  (n: Int): Self = matches(_.sizeList   == n)

        // ---------------------------------------------------------------------------
        def greaterThan     [N: Numeric](value: N): Self = matches(_ >  value.asInstanceOf[Number])
        def greaterOrEqualTo[N: Numeric](value: N): Self = matches(_ >= value.asInstanceOf[Number])

        def lessThan        [N: Numeric](value: N): Self = matches(_ <  value.asInstanceOf[Number])
        def lessOrEqualTo   [N: Numeric](value: N): Self = matches(_ <= value.asInstanceOf[Number])

        // ---------------------------------------------------------------------------
        def isCurrentDate: Self = hasValue(java.time.LocalDate.now()) }

      // ===========================================================================
      def filterBy(target: FilterBy1[HeadU])(implicit di: DI) = new _FilterByU1(target) // trade-off: pre-process for more more than 1
       final class _FilterByU1 private[heads] (target: FilterBy1[HeadU]) {
        //TODO: add isPresent/...
        def matches(f: HeadU => HeadV[Boolean]): Self =
          zz(FilterByU(resolve(target), f, asFind = false)) }

      // ---------------------------------------------------------------------------
      def filterBy(target: FilterBy1[HeadZ])(implicit di: DI, di2: DI) = new _FilterByU2(target) // trade-off: pre-process for more more than 1
       final class _FilterByU2 private[heads] (target: FilterBy1[HeadZ]) {
        def matches(f: HeadZ => HeadV[Boolean]): Self =
          zz(FilterByZ(resolve(target), f, asFind = false)) }

  // ===========================================================================
  @Max5
  def filterBy(f1: KPathW, f2: KPathW)            : __FilterBy2WV = new __FilterBy2WV(f1, f2)
  def filterBy(f1: KPathW, f2: KPathW, f3: KPathW): __FilterBy3WV = new __FilterBy3WV(f1, f2, f3)

    // ---------------------------------------------------------------------------
    @Max5
    def filterBy[O1: WTT, O2: WTT         ](f1: FilterByT[O1], f2: FilterByT[O2])                   : _FilterBy2[O1, O2]     = new _FilterBy2(f1, f2)
    def filterBy[O1: WTT, O2: WTT, O3: WTT](f1: FilterByT[O1], f2: FilterByT[O2], f3: FilterByT[O3]): _FilterBy3[O1, O2, O3] = new _FilterBy3(f1, f2, f3)

    // ===========================================================================
    class __FilterBy2WV(target1: KPathW, target2: KPathW) { import gallia.trgt.utils.TargetQueryUtils.tqkpath2    
        def hasValues(value1: Any, value2: Any): Self = matches { (x, y) => x.any == value1 && y.any == value2 }
        def notValues(value1: Any, value2: Any): Self = matches { (x, y) => x.any != value1 && y.any != value2 }
    
        // ---------------------------------------------------------------------------
        def matches(f: (WV, WV) => TWV[Boolean])                 : Self = zz(FilterByWV2(tqkpath2(target1.value, target2.value), f(_, _).typed, asFind = false))
        def matches(f: (WV, WV) =>     Boolean) (implicit di: DI): Self = zz(FilterByWV2(tqkpath2(target1.value, target2.value), f,             asFind = false)) }
  
      // ---------------------------------------------------------------------------
      class __FilterBy3WV(target1: KPathW, target2: KPathW, target3: KPathW) { import gallia.trgt.utils.TargetQueryUtils.tqkpath3
        def hasValues(value1: Any, value2: Any, value3: Any): Self = matches { (x, y, z) => x.any == value1 && y.any == value2 && z.any == value3 }
        def notValues(value1: Any, value2: Any, value3: Any): Self = matches { (x, y, z) => x.any != value1 && y.any != value2 && z.any != value3 }

        // ---------------------------------------------------------------------------
        def matches(f: (WV, WV, WV) => TWV[Boolean])                 : Self = zz(FilterByWV3(tqkpath3(target1.value, target2.value, target3.value), f(_, _, _).typed, asFind = false))
        def matches(f: (WV, WV, WV) =>     Boolean) (implicit di: DI): Self = zz(FilterByWV3(tqkpath3(target1.value, target2.value, target3.value), f,                asFind = false)) }

    // ===========================================================================  
    class _FilterBy2[O1: WTT, O2: WTT](f1: FilterByT[O1], f2: FilterByT[O2]) { 
      //TODO: areAllMissing/areAllPresent, ...
      def matches(f: (O1, O2) => Boolean): Self =
        zz(FilterByV2(resolve2(f1, f2), f, asFind = false)) }

    // ---------------------------------------------------------------------------
    class _FilterBy3[O1: WTT, O2: WTT, O3: WTT](f1: FilterByT[O1], f2: FilterByT[O2], f3: FilterByT[O3]) {
      def matches(f: (O1, O2, O3) => Boolean): Self =
        zz(FilterByV3(resolve3(f1, f2, f3), f, asFind = false)) }

  // ===========================================================================
  def filterOutEmptyLines              : Self = filterOutEmptyLines(_line)
  def filterOutEmptyLines(target: KeyW): Self = filterBy(_.string(target.value)).matches(_.trim.nonEmpty)

}

// ===========================================================================
