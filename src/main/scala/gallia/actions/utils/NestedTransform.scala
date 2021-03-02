package gallia.actions.utils

import aptus.Anything_

import gallia._
import gallia.plans._
import gallia.actions._
import gallia.FunctionWrappers._
import gallia.dag.RootId
import gallia.heads.HeadsNestingHandler
import gallia.atoms.AtomsUUTransforms._Transform1to1

// ===========================================================================
class NestedTransform(adag: MetaPlan, val rootId: RootId) { // TODO: as a peer of MetaPlan rather?
  import ActionsUUUtils._

  // ---------------------------------------------------------------------------
  //FIXME: overwriting fields ok?
  def vldt(c: Cls, qpathz: RPathz): Errs = qpathz.map(_.from).flatMap(vldt(c, _)).toSeq

    // ---------------------------------------------------------------------------
    def vldt(c: Cls, kpath: KPath): Errs =
      kpath
        .thn(c.field_(_))
        .flatMap(_.info.nestingTypeOpt) //FIXME: check nesting field instead
        .toSeq
        .flatMap(_vldt)

      // ---------------------------------------------------------------------------
      /*private - needed for forkey */def _vldt(c: Cls) = adag.runMeta(rootId, c).allErrors

  // ===========================================================================
  // meta

  def transformMeta(c: Cls, paths: RPathz): Cls = paths.foldLeft(c)(_transformMeta)

  def generateMeta(c: Cls, from: KPath): Cls = _meta(c.forceNestedClass(from))

    private def _transformMeta(c: Cls, path: RPath): Cls = c.transformInfo(path)(_.transformNestedClass(_meta))

      /*private - needed for forkey */def _meta(cc: Cls): Cls = adag.runMeta(rootId, cc).forceLeafClass

  // ===========================================================================
  // data

  def atomuusUU(c: Cls)(qpathz: RPathz, optional: Boolean): AtomUUs = atomuus(c)(qpathz, optional)(_.FromNesting.u2u(optional))
  def atomuusZZ(c: Cls)(qpathz: RPathz, optional: Boolean): AtomUUs = atomuus(c)(qpathz, optional)(_.FromNesting.z2z(optional))

  def atomuusUZ(c: Cls)(qpathz: RPathz, optional: Boolean): AtomUUs = atomuus(c)(qpathz, optional)(_.FromNesting.u2z(optional))
  def atomuusZU(c: Cls)(qpathz: RPathz, optional: Boolean): AtomUUs = atomuus(c)(qpathz, optional)(_.FromNesting.z2u(optional))

  def atomuusUV(c: Cls)(qpathz: RPathz, optional: Boolean): AtomUUs = atomuus(c)(qpathz, optional)(_.FromNesting.u2v(optional))
  def atomuusZV(c: Cls)(qpathz: RPathz, optional: Boolean): AtomUUs = atomuus(c)(qpathz, optional)(_.FromNesting.z2v(optional))

    // ---------------------------------------------------------------------------
    private def atomuus(c: Cls)(qpathz: RPathz, optional: Boolean)(f: AtomPlan => Any => Any): AtomUUs =
        qpathz.values.thn(_atoms(c)(pair => {
          assert(pair.optional == optional)
           val nestedClass = c.forceNestedClass(pair.path)

          _Transform1to1(pair, pair.path, f(atomPlan(nestedClass))) }))

  // ---------------------------------------------------------------------------
  // eg as used in generate
  def uu(c: Cls, optional: Boolean): _ff11 = atomPlan(c).FromNesting.u2u(optional)
  def zz(c: Cls, optional: Boolean): _ff11 = atomPlan(c).FromNesting.z2z(optional)

  // ---------------------------------------------------------------------------
  // used in zen/for-key
  def dataU2U(c: Cls): Obj  => Obj  = atomPlan(c).naiveRunUU _
  def dataZ2Z(c: Cls): Objs => Objs = atomPlan(c).naiveRunZZ _

  def dataU2Z(c: Cls): Obj  => Objs = atomPlan(c).naiveRunUZ _
  def dataZ2U(c: Cls): Objs => Obj  = atomPlan(c).naiveRunZU _

  def dataU2V(c: Cls): Obj  => Vle  = atomPlan(c).naiveRunUV _
  def dataZ2V(c: Cls): Objs => Vle  = atomPlan(c).naiveRunZV _

  def dataU2B(c: Cls): Obj  => Boolean = x => atomPlan(c).naiveRunUV(x).asInstanceOf[Boolean]
  def dataZ2B(c: Cls): Objs => Boolean = x => atomPlan(c).naiveRunZV(x).asInstanceOf[Boolean]

  // ===========================================================================
  private def atomPlan(c: Cls): AtomPlan =
    adag
      .runMeta(rootId, c)
      .forceActionPlan
      .atomPlan

}

// ===========================================================================
object NestedTransform {
  def parseUU        (f: HeadU => HeadU   ): NestedTransform =  { val (rootId, dag, leafId) = HeadsNestingHandler.uToU(f); new NestedTransform(MetaPlan(dag), rootId) }
  def parseZZ        (f: HeadZ => HeadZ   ): NestedTransform =  { val (rootId, dag, leafId) = HeadsNestingHandler.zToZ(f); new NestedTransform(MetaPlan(dag), rootId) }

  def parseUZ        (f: HeadU => HeadZ   ): NestedTransform =  { val (rootId, dag, leafId) = HeadsNestingHandler.uToZ(f); new NestedTransform(MetaPlan(dag), rootId) }
  def parseZU        (f: HeadZ => HeadU   ): NestedTransform =  { val (rootId, dag, leafId) = HeadsNestingHandler.zToU(f); new NestedTransform(MetaPlan(dag), rootId) }

  def parseUV[T: WTT](f: HeadU => HeadV[T]): NestedTransform =  { val (rootId, dag, leafId) = HeadsNestingHandler.uToV(f); new NestedTransform(MetaPlan(dag), rootId) }
  def parseZV[T: WTT](f: HeadZ => HeadV[T]): NestedTransform =  { val (rootId, dag, leafId) = HeadsNestingHandler.zToV(f); new NestedTransform(MetaPlan(dag), rootId) }
}

// ===========================================================================
