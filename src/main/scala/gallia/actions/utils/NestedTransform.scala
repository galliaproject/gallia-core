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

  def atomuusUU(c: Cls)(qpathz: RPathz, optional: Boolean): AtomUUs = atomuus(c)(qpathz, optional)(_.atomPlan.FromNesting.u2u(optional))
  def atomuusZZ(c: Cls)(qpathz: RPathz, optional: Boolean): AtomUUs = atomuus(c)(qpathz, optional)(_.atomPlan.FromNesting.z2z(optional))

  def atomuusUZ(c: Cls)(qpathz: RPathz, optional: Boolean): AtomUUs = atomuus(c)(qpathz, optional)(_.atomPlan.FromNesting.u2z(optional))
  def atomuusZU(c: Cls)(qpathz: RPathz, optional: Boolean): AtomUUs = atomuus(c)(qpathz, optional)(_.atomPlan.FromNesting.z2u(optional))

  def atomuusUV(c: Cls)(qpathz: RPathz, optional: Boolean): AtomUUs = atomuus(c)(qpathz, optional)(_.atomPlan.FromNesting.u2v(optional))
  def atomuusZV(c: Cls)(qpathz: RPathz, optional: Boolean): AtomUUs = atomuus(c)(qpathz, optional)(_.atomPlan.FromNesting.z2v(optional))

    // ---------------------------------------------------------------------------
    private def atomuus(c: Cls)(qpathz: RPathz, optional: Boolean)(f: ActionPlan => Any => Any): AtomUUs =
        qpathz.values.thn(_atoms(c)(pair => {
          assert(pair.optional == optional)
          val actionPlan = this.actionPlan(c.forceNestedClass(pair.path))

          _Transform1to1(pair, pair.path, f(actionPlan)) }))

  // ---------------------------------------------------------------------------
  // eg as used in generate
  def uu(c: Cls, optional: Boolean): _ff11 = actionPlan(c).atomPlan.FromNesting.u2u(optional)
  def zz(c: Cls, optional: Boolean): _ff11 = actionPlan(c).atomPlan.FromNesting.z2z(optional)

  // ---------------------------------------------------------------------------
  // used in zen/for-key
  def dataU2U(c: Cls): Obj  => Obj  = actionPlan(c).atomPlan.naiveRunUU _
  def dataZ2Z(c: Cls): Objs => Objs = actionPlan(c).atomPlan.naiveRunZZ _

  def dataU2Z(c: Cls): Obj  => Objs = actionPlan(c).atomPlan.naiveRunUZ _
  def dataZ2U(c: Cls): Objs => Obj  = actionPlan(c).atomPlan.naiveRunZU _

  def dataU2V(c: Cls): Obj  => Vle  = actionPlan(c).atomPlan.naiveRunUV _
  def dataZ2V(c: Cls): Objs => Vle  = actionPlan(c).atomPlan.naiveRunZV _

  def dataU2B(c: Cls): Obj  => Boolean = x => actionPlan(c).atomPlan.naiveRunUV(x).asInstanceOf[Boolean]
  def dataZ2B(c: Cls): Objs => Boolean = x => actionPlan(c).atomPlan.naiveRunZV(x).asInstanceOf[Boolean]

  // ===========================================================================
  private def actionPlan(c: Cls): ActionPlan =
    adag
      .runMeta(rootId, c)
      .forceActionPlan
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
