package gallia.actions.utils

import aptus.Anything_

import gallia._
import gallia.domain.PathPair
import gallia.plans._
import gallia.actions._
import gallia.FunctionWrappers._
import gallia.dag.RootId
import gallia.heads.HeadsNestingHandler
import gallia.atoms.AtomsUUTransforms._Transform1to1

// ===========================================================================
class NestedTransform(adag: MetaPlan, val rootId: RootId) { // TODO: as a peer of MetaPlan rather?
  import ActionsUUUtils._

  // ===========================================================================
  // validation

  def vldt(c: Cls, qpathz: RPathz): Errs = qpathz.map(_.from).flatMap(vldt(c, _)).toSeq //FIXME: overwriting fields ok?

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
  // meta to data
  /*private - needed in MapU2U*/ def metaToAtomPlan(c: Cls): AtomPlan = // c may or may not be a nested class
    adag
      .runMeta(rootId, c)
      .forceActionPlan
      .atomPlan
      
  // ===========================================================================
  // data

  def transformData(c: Cls, multiple: Boolean)(path: RPath): Seq[AtomUU] = {
    val optional    = c.isOptional      (path.from)        
    val nestedClass = c.forceNestedClass(path.from)            
    val plan        = metaToAtomPlan(nestedClass)
    
    val f: _ff11 =
      if (!multiple)
        if (optional) plan.V2.naiveRunUU_
        else          plan.V2.naiveRunUU
      else
        if (optional) plan.V2.naiveRunZZ_
        else          plan.V2.naiveRunZZ

    val pair = PathPair(path.to, optional)        
    potentialRenaming(path) :+ _Transform1to1(pair, pair.path, f)
  }  
  
  // ---------------------------------------------------------------------------
  // older way (to be phased out?):
    
    def atomuusUZ(c: Cls)(qpathz: RPathz, optional: Boolean): AtomUUs = atomuus(c)(qpathz, optional)(new NestedPlan(_).nestedRunneru2z(optional))
    def atomuusZU(c: Cls)(qpathz: RPathz, optional: Boolean): AtomUUs = atomuus(c)(qpathz, optional)(new NestedPlan(_).nestedRunnerz2u(optional))
  
    def atomuusUV(c: Cls)(qpathz: RPathz, optional: Boolean): AtomUUs = atomuus(c)(qpathz, optional)(new NestedPlan(_).nestedRunneru2v(optional))
    def atomuusZV(c: Cls)(qpathz: RPathz, optional: Boolean): AtomUUs = atomuus(c)(qpathz, optional)(new NestedPlan(_).nestedRunnerz2v(optional))
  
      // ---------------------------------------------------------------------------
      private def atomuus(c: Cls)(qpathz: RPathz, optional: Boolean)(f: AtomPlan => Any => Any): AtomUUs =
          _atoms(c)(pair => {
              assert(pair.optional == optional)
              val nestedClass = c.forceNestedClass(pair.path)
  
              _Transform1to1(pair, pair.path, f(metaToAtomPlan(nestedClass))) })(
            qpathz.values)
  
    // ---------------------------------------------------------------------------
    // eg as used in generate
    def uu(c: Cls, optional: Boolean): _ff11 = metaToAtomPlan(c).thn(new NestedPlan(_)).nestedRunneru2u(optional)
    def zz(c: Cls, optional: Boolean): _ff11 = metaToAtomPlan(c).thn(new NestedPlan(_)).nestedRunnerz2z(optional)
  
    // ---------------------------------------------------------------------------
    // used in zen/for-key
    def dataU2U(c: Cls): Obj  => Obj  = metaToAtomPlan(c).V1.naiveRunUU _
    def dataZ2Z(c: Cls): Objs => Objs = metaToAtomPlan(c).V1.naiveRunZZ _
  
    def dataU2Z(c: Cls): Obj  => Objs = metaToAtomPlan(c).V1.naiveRunUZ _
    def dataZ2U(c: Cls): Objs => Obj  = metaToAtomPlan(c).V1.naiveRunZU _
  
    def dataU2V(c: Cls): Obj  => Vle  = metaToAtomPlan(c).V1.naiveRunUV _
    def dataZ2V(c: Cls): Objs => Vle  = metaToAtomPlan(c).V1.naiveRunZV _
  
    def dataU2B(c: Cls): Obj  => Boolean = x => metaToAtomPlan(c).V1.naiveRunUV(x).asInstanceOf[Boolean]
    def dataZ2B(c: Cls): Objs => Boolean = x => metaToAtomPlan(c).V1.naiveRunZV(x).asInstanceOf[Boolean]
}

// ===========================================================================
object NestedTransform { // TODO: not necessarily "nested" per se (eg forX context)...
  def parseUU        (f: HeadU => HeadU   ): NestedTransform =  { val (rootId, dag, leafId) = HeadsNestingHandler.uToU(f); new NestedTransform(MetaPlan(dag), rootId) }
  def parseZZ        (f: HeadZ => HeadZ   ): NestedTransform =  { val (rootId, dag, leafId) = HeadsNestingHandler.zToZ(f); new NestedTransform(MetaPlan(dag), rootId) }

  def parseUZ        (f: HeadU => HeadZ   ): NestedTransform =  { val (rootId, dag, leafId) = HeadsNestingHandler.uToZ(f); new NestedTransform(MetaPlan(dag), rootId) }
  def parseZU        (f: HeadZ => HeadU   ): NestedTransform =  { val (rootId, dag, leafId) = HeadsNestingHandler.zToU(f); new NestedTransform(MetaPlan(dag), rootId) }

  def parseUV[T: WTT](f: HeadU => HeadV[T]): NestedTransform =  { val (rootId, dag, leafId) = HeadsNestingHandler.uToV(f); new NestedTransform(MetaPlan(dag), rootId) }
  def parseZV[T: WTT](f: HeadZ => HeadV[T]): NestedTransform =  { val (rootId, dag, leafId) = HeadsNestingHandler.zToV(f); new NestedTransform(MetaPlan(dag), rootId) }
}

// ===========================================================================
