package gallia
package heads
package reducing

// ===========================================================================
case class ReducingPairs(values: Seq[ReducingPair]) { def keyz: Keyz = Keyz(values.map(_.target.value)) }

// ===========================================================================
trait ReducingPair {
    val target: KeyW // TODO: ren?

    // ---------------------------------------------------------------------------
    def field(in: Cls): Fld
    def dataTriplet(in: Cls): ReducingDataTriplet

    // ---------------------------------------------------------------------------
    protected final def optional(in: Cls): Boolean               = in.field(target.value).isOptional
    protected final def ntipeOpt(in: Cls): Option[NumericalType] = in.field(target.value).numericalTypeOpt
  }

  // ===========================================================================
  case class ReducingPair1(
          target: KeyW, // TODO: ren?
          tipe  : ReducingType)
        extends ReducingPair {

      def field(in: Cls): Fld = in.field(target.value).pipe(tipe.field(target.value, _))

      def dataTriplet(in: Cls) = ReducingDataTriplet1(target.value, tipe, optional(in), ntipeOpt(in))
    }

    // ===========================================================================
    object ReducingPair1 {

      trait Implicit_ {
        protected val _key: Key

        // ---------------------------------------------------------------------------
        /** note: flattens None (must pre-process if need to account for them */
        def flattened: ReducingPair1 = ReducingType.flattened.pair(_key)

        def count_all                : ReducingPair1 = ReducingType.count_all             .pair(_key)
          def count_present          : ReducingPair1 = ReducingType.count_present         .pair(_key)
          def count_missing          : ReducingPair1 = ReducingType.count_missing         .pair(_key)
          def count_distinct         : ReducingPair1 = ReducingType.count_distinct        .pair(_key)
          def count_distinct_present : ReducingPair1 = ReducingType.count_distinct_present.pair(_key)

        @deprecated("name seems to conflict with something, use sum_ instead") // t240124112404 - investigate
        def sum   : ReducingPair1 = ReducingType.sum   .pair(_key)
        def sum_  : ReducingPair1 = ReducingType.sum   .pair(_key)
        def mean  : ReducingPair1 = ReducingType.mean  .pair(_key)
        def stdev : ReducingPair1 = ReducingType.stdev .pair(_key)
        def median: ReducingPair1 = ReducingType.median.pair(_key)

        def stats: ReducingPair1  = ReducingType.stats .pair(_key)

        // ---------------------------------------------------------------------------
        @deprecated def count  = count_all
        @deprecated def values = flattened } }

  // ===========================================================================
  case class ReducingPairN(
          target: KeyW, // TODO: ren?
          tipes : Seq[ReducingType])
        extends ReducingPair {

      def field(in: Cls): Fld =
        in
          .field(target.value)
          .pipe { field => tipes.map(_.field(field)).pipe(Cls.apply) }
          .pipe(Fld.one(target.value, _))

      def dataTriplet(in: Cls) = ReducingDataTripletN(target.value, tipes, optional(in), ntipeOpt(in))
    }

    // ---------------------------------------------------------------------------
    object ReducingPairN {

      trait Implicit_ {
        protected val _key: Key

        def aggregates(
              agg1: ReducingType.type => ReducingType,
              more: ReducingType.type => ReducingType*)
            : ReducingPairN =
          ReducingPairN(_key, (agg1 +: more).map(_.apply(ReducingType)))
      }
    }

// ===========================================================================
