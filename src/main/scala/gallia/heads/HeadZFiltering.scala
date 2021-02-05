package gallia.heads

import gallia._
import gallia.actions.ActionsZZFiltering._

// ===========================================================================
trait HeadZFiltering { _: HeadZ => // pretty messy, need to find a cleaner way to keep find and filter more in sync

  // TODO: t201023083923 - add find/filter to HeadU as well, to make it an "option" (relates to t201021120753 - HeadOptionU)

  private type FilterBy1[T] = TSL.FilterBy1.TSelector[T]
  private type FilterByT[T] = TSL.FilterByT.TSelector[T]

  private type FindBy1  [T] = FilterBy1[T]
  private type FindByT  [T] = FilterByT[T]

  import TSL.FilterBy1._

  // ===========================================================================
  // must match filter below other than the flag:
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
                  def findBy(f1: KPathW, f2: KPathW)            : _FindBy2[WV, WV]     = findBy(_._explicit(f1.value), _._explicit(f2.value))
                  def findBy(f1: KPathW, f2: KPathW, f3: KPathW): _FindBy3[WV, WV, WV] = findBy(_._explicit(f1.value), _._explicit(f2.value), _._explicit(f3.value))

                      // ---------------------------------------------------------------------------
                      @Max5
                      def findBy[O1: WTT, O2: WTT         ](f1: FindByT[O1], f2: FindByT[O2])                 : _FindBy2[O1, O2]     = new _FindBy2(f1, f2)
                      def findBy[O1: WTT, O2: WTT, O3: WTT](f1: FindByT[O1], f2: FindByT[O2], f3: FindByT[O3]): _FindBy3[O1, O2, O3] = new _FindBy3(f1, f2, f3)

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
      class __FilterBy1WV(target: KPathW) {

        /** similar to SQL's WHERE X IN Y clause */
        def in[T:WTT](coll: Iterable[T]): Self = ???

        def hasValue(a: WV): Self2 = matches(_ == a)

        def isPresent      : Self = hasSize(1)
        def isMissing      : Self = hasSize(1)
        def hasSize(n: Int): Self = matches(Whatever /* may or may not be Whatever */.size(_) == n)

        def matches(f: WV => WV2[Boolean]): Self = zz(FilterByWV(gallia.target.utils.TargetQueryUtils.tqkpath(target.value), f)) //TODO: disallow?
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
  def filterBy(f1: KPathW, f2: KPathW)            : _FilterBy2[WV, WV]     = filterBy(_._explicit(f1.value), _._explicit(f2.value))
  def filterBy(f1: KPathW, f2: KPathW, f3: KPathW): _FilterBy3[WV, WV, WV] = filterBy(_._explicit(f1.value), _._explicit(f2.value), _._explicit(f3.value))

      // ---------------------------------------------------------------------------
      @Max5
      def filterBy[O1: WTT, O2: WTT         ](f1: FilterByT[O1], f2: FilterByT[O2])                   : _FilterBy2[O1, O2]     = new _FilterBy2(f1, f2)
      def filterBy[O1: WTT, O2: WTT, O3: WTT](f1: FilterByT[O1], f2: FilterByT[O2], f3: FilterByT[O3]): _FilterBy3[O1, O2, O3] = new _FilterBy3(f1, f2, f3)

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
