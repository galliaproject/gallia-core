package gallia
package actions

import atoms.AtomsReducing._
import heads.reducing._
import heads.reducing.ReducingPair

// ===========================================================================
@NumberAbstraction
object ActionsReducing {

  case class Reduce(pairs: ReducingPairs) extends ActionZUc {
      def vldt  (in: Cls): Errs    = Nil//TODO; at least one pair, no duplicates
      def _meta (in: Cls): Cls    = pairs.values.map(_.field(in)).pipe(Cls.apply)
      def atomzu(in: Cls): AtomZU = pairs.values.map(_.dataTriplet(in)).pipe(_Reduce)
    }

    // ---------------------------------------------------------------------------
    case class Reduce1(targets: TqKeyz, tipe: ReducingType) extends ActionZUc {
      def vldt (in: Cls): Errs = Nil

      def _meta (in: Cls): Cls    = pairs(in).map(_.field(in)).pipe(Cls.apply)
      def atomzu(in: Cls): AtomZU = pairs(in).map(_.dataTriplet(in)).pipe(_Reduce)

      private def pairs(in: Cls) = targets.resolve(in).map(ReducingPair1(_, tipe))
    }

    // ---------------------------------------------------------------------------
    case class ReduceN(targets: TqKeyz, tipes: Seq[ReducingType] /* TODO: check at least one*/) extends ActionZUc {
      def vldt  (in: Cls): Errs = Nil
      def _meta (in: Cls): Cls    = pairs(in).map(_.field(in)).pipe(Cls.apply)
      def atomzu(in: Cls): AtomZU = pairs(in).map(_.dataTriplet(in)).pipe(_Reduce)
        //targets.resolve(in).map { key => ReducingDataTripletN(key, tipes, in.field(key).subInfo.numericalTypeOpt) }.pipe(_Reduce)

      private def pairs(in: Cls) = targets.resolve(in).map(ReducingPairN(_, tipes))
    }

}

// ===========================================================================
