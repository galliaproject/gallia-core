package gallia
package heads

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

                  def findUnsafe(pred: Obj => Boolean): Self = zz(FilterUnsafe(pred, asFind = true))

                  // ===========================================================================
                  def findBy        (target: KPathW)    : __FindBy1[WV] = findBy(_._explicit(target.value))
                  def findBy[O: WTT](target: FindBy1[O]): __FindBy1[O]  = new __FindBy1[O](target)

                        // TODO: add isPresent/... + whatever separation like below
                        class __FindBy1[O: WTT](target: FindBy1[O]) {
                          def hasValue(a: O): Self2 = matches(_ == a)

                          def matches(f: O => Boolean): Self =
                            zz(FilterByV(resolve(target), f, asFind = true)) }

                      // ---------------------------------------------------------------------------
                      def findBy(target: FindBy1[HeadU])(implicit di: DI) = new { // trade-off: pre-process for more more than 1
                        def matches(f: HeadU => HeadV[Boolean]): Self =
                          zz(FilterByU(resolve(target), f, asFind = true)) }

                      // ---------------------------------------------------------------------------
                      def findBy(target: FindBy1[HeadZ])(implicit di: DI, di2: DI) = new { // trade-off: pre-process for more more than 1
                        def matches(f: HeadZ => HeadV[Boolean]): Self =
                          zz(FilterByZ(resolve(target), f, asFind = true)) }

                  // ===========================================================================
                  def findBy(f1: KPathW, f2: KPathW)            : __FindBy2WV = new __FindBy2WV(f1, f2)
                  def findBy(f1: KPathW, f2: KPathW, f3: KPathW): __FindBy3WV = new __FindBy3WV(f1, f2, f3)

                      // ---------------------------------------------------------------------------
                      @Max5
                      def findBy[O1: WTT, O2: WTT         ](f1: FindByT[O1], f2: FindByT[O2])                 : _FindBy2[O1, O2]     = new _FindBy2(f1, f2)
                      def findBy[O1: WTT, O2: WTT, O3: WTT](f1: FindByT[O1], f2: FindByT[O2], f3: FindByT[O3]): _FindBy3[O1, O2, O3] = new _FindBy3(f1, f2, f3)

                    // ===========================================================================                  
                    class __FindBy1WV(target: KPathW) { import gallia.target.utils.TargetQueryUtils.tqkpath
              
                      def matches(f: WV => TWV[Boolean])                 : Self = zz(FilterByWV(tqkpath(target.value), f(_).typed, asFind = true))
                      def matches(f: WV =>     Boolean) (implicit di: DI): Self = zz(FilterByWV(tqkpath(target.value), f,          asFind = true))
              
                      // ---------------------------------------------------------------------------
                      /** similar to SQL's WHERE X IN Y clause */ // TODO: toSet if big enough? use bloom if very big?
                      def    in[T:WTT](coll: Seq[T]): Self = matches({ wv =>  coll.contains(wv.any) })
                      def notIn[T:WTT](coll: Seq[T]): Self = matches({ wv => !coll.contains(wv.any) })
              
                      def hasValue(value: Any): Self2 = matches(_.any == value)
                      def notValue(value: Any): Self2 = matches(_.any != value)
              
                      def isPresent      : Self = hasSize(1)
                      def isMissing      : Self = hasSize(1)
              
                      /** does not work for String.size (will return 1) */
                      def hasSize(n: Int): Self = matches(Whatever.size(_) == n)
              
                      // ---------------------------------------------------------------------------
                      def greaterThan     [N: Numeric](value: N): Self = matches(_ >  value.asInstanceOf[Number])
                      def greaterOrEqualTo[N: Numeric](value: N): Self = matches(_ >= value.asInstanceOf[Number])
              
                      def lessThan        [N: Numeric](value: N): Self = matches(_ <  value.asInstanceOf[Number])
                      def lessOrEqualTo   [N: Numeric](value: N): Self = matches(_ <= value.asInstanceOf[Number])
                    }

                    // ---------------------------------------------------------------------------
                    class __FindBy2WV(target1: KPathW, target2: KPathW) { import gallia.target.utils.TargetQueryUtils.tqkpath2
                        def matches(f: (WV, WV) => TWV[Boolean])                 : Self = zz(FilterByWV2(tqkpath2(target1.value, target2.value), f(_, _).typed, asFind = true))
                        def matches(f: (WV, WV) =>     Boolean) (implicit di: DI): Self = zz(FilterByWV2(tqkpath2(target1.value, target2.value), f,             asFind = true)) }
                  
                      // ---------------------------------------------------------------------------
                      class __FindBy3WV(target1: KPathW, target2: KPathW, target3: KPathW) { import gallia.target.utils.TargetQueryUtils.tqkpath3
                        def matches(f: (WV, WV, WV) => TWV[Boolean])                 : Self = zz(FilterByWV3(tqkpath3(target1.value, target2.value, target3.value), f(_, _, _).typed, asFind = true))
                        def matches(f: (WV, WV, WV) =>     Boolean) (implicit di: DI): Self = zz(FilterByWV3(tqkpath3(target1.value, target2.value, target3.value), f,                asFind = true)) }
                  
                    // ===========================================================================
                    class _FindBy2[O1: WTT, O2: WTT](f1: FindByT[O1], f2: FindByT[O2]) {
                      def matches(f: (O1, O2) => Boolean): Self =
                        zz(FilterByV2(resolve2(f1, f2), f, asFind = true)) }

                    // ---------------------------------------------------------------------------
                    class _FindBy3[O1: WTT, O2: WTT, O3: WTT](f1: FindByT[O1], f2: FindByT[O2], f3: FindByT[O3]) {
                      def matches(f: (O1, O2, O3) => Boolean): Self =
                        zz(FilterByV3(resolve3(f1, f2, f3), f, asFind = true)) }


  // ===========================================================================
  def filterUnsafe(pred: Obj => Boolean): Self = zz(FilterUnsafe(pred))

  // ===========================================================================
  def filterBy        (target: KPathW)      : __FilterBy1WV   = new __FilterBy1WV (target)
  def filterBy[O: WTT](target: FilterBy1[O]): __FilterBy1[O]  = new __FilterBy1[O](target)

      // ---------------------------------------------------------------------------
      //def filterBy2(k: KeyW) = new {  } // eg "init" def filterByInclusion[T: WTT](k: KeyW, coll: Iterable[T]): HeadZ
      //  .filterBy(_.string('SIG)).matches { _.containedIn(csts.SigSet) } }

      class __FilterBy1[O: WTT](target: FilterBy1[O]) {
        def matches(f: O => Boolean): Self = zz(FilterByV(resolve(target), f))
      }

      // ---------------------------------------------------------------------------
      class __FilterBy1WV(target: KPathW) { import gallia.target.utils.TargetQueryUtils.tqkpath

        def matches(f: WV => TWV[Boolean])                 : Self = zz(FilterByWV(tqkpath(target.value), f(_).typed, asFind = false))
        def matches(f: WV =>     Boolean) (implicit di: DI): Self = zz(FilterByWV(tqkpath(target.value), f,          asFind = false))

        // ---------------------------------------------------------------------------
        /** similar to SQL's WHERE X IN Y clause */ // TODO: toSet if big enough? use bloom if very big?
        def    in[T:WTT](coll: Seq[T]): Self = matches({ wv =>  coll.contains(wv.any) })
        def notIn[T:WTT](coll: Seq[T]): Self = matches({ wv => !coll.contains(wv.any) })

        def hasValue(value: Any): Self2 = matches(_.any == value)
        def notValue(value: Any): Self2 = matches(_.any != value)

        def isPresent      : Self = hasSize(1)
        def isMissing      : Self = hasSize(1)

        /** does not work for String.size (will return 1) */
        def hasSize(n: Int): Self = matches(Whatever.size(_) == n)

        // ---------------------------------------------------------------------------
        def greaterThan     [N: Numeric](value: N): Self = matches(_ >  value.asInstanceOf[Number])
        def greaterOrEqualTo[N: Numeric](value: N): Self = matches(_ >= value.asInstanceOf[Number])

        def lessThan        [N: Numeric](value: N): Self = matches(_ <  value.asInstanceOf[Number])
        def lessOrEqualTo   [N: Numeric](value: N): Self = matches(_ <= value.asInstanceOf[Number])
      }

      // ---------------------------------------------------------------------------
      def filterBy(target: FilterBy1[HeadU])(implicit di: DI) = new { // trade-off: pre-process for more more than 1
        //TODO: add isPresent/...
        def matches(f: HeadU => HeadV[Boolean]): Self =
          zz(FilterByU(resolve(target), f)) }

      // ---------------------------------------------------------------------------
      def filterBy(target: FilterBy1[HeadZ])(implicit di: DI, di2: DI) = new { // trade-off: pre-process for more more than 1
        def matches(f: HeadZ => HeadV[Boolean]): Self =
          zz(FilterByZ(resolve(target), f)) }

  // ===========================================================================
  @Max5
  def filterBy(f1: KPathW, f2: KPathW)            : __FilterBy2WV = new __FilterBy2WV(f1, f2)
  def filterBy(f1: KPathW, f2: KPathW, f3: KPathW): __FilterBy3WV = new __FilterBy3WV(f1, f2, f3)

    // ---------------------------------------------------------------------------
    @Max5
    def filterBy[O1: WTT, O2: WTT         ](f1: FilterByT[O1], f2: FilterByT[O2])                   : _FilterBy2[O1, O2]     = new _FilterBy2(f1, f2)
    def filterBy[O1: WTT, O2: WTT, O3: WTT](f1: FilterByT[O1], f2: FilterByT[O2], f3: FilterByT[O3]): _FilterBy3[O1, O2, O3] = new _FilterBy3(f1, f2, f3)

    // ===========================================================================
    class __FilterBy2WV(target1: KPathW, target2: KPathW) { import gallia.target.utils.TargetQueryUtils.tqkpath2
        def matches(f: (WV, WV) => TWV[Boolean])                 : Self = zz(FilterByWV2(tqkpath2(target1.value, target2.value), f(_, _).typed, asFind = false))
        def matches(f: (WV, WV) =>     Boolean) (implicit di: DI): Self = zz(FilterByWV2(tqkpath2(target1.value, target2.value), f,             asFind = false)) }
  
      // ---------------------------------------------------------------------------
      class __FilterBy3WV(target1: KPathW, target2: KPathW, target3: KPathW) { import gallia.target.utils.TargetQueryUtils.tqkpath3
        def matches(f: (WV, WV, WV) => TWV[Boolean])                 : Self = zz(FilterByWV3(tqkpath3(target1.value, target2.value, target3.value), f(_, _, _).typed, asFind = false))
        def matches(f: (WV, WV, WV) =>     Boolean) (implicit di: DI): Self = zz(FilterByWV3(tqkpath3(target1.value, target2.value, target3.value), f,                asFind = false)) }

    // ===========================================================================  
    class _FilterBy2[O1: WTT, O2: WTT](f1: FilterByT[O1], f2: FilterByT[O2]) { 
      //TODO: areAllMissing/areAllPresent, ...
      def matches(f: (O1, O2) => Boolean): Self =
        zz(FilterByV2(resolve2(f1, f2), f)) }

    // ---------------------------------------------------------------------------
    class _FilterBy3[O1: WTT, O2: WTT, O3: WTT](f1: FilterByT[O1], f2: FilterByT[O2], f3: FilterByT[O3]) {
      def matches(f: (O1, O2, O3) => Boolean): Self =
        zz(FilterByV3(resolve3(f1, f2, f3), f)) }

}

// ===========================================================================
