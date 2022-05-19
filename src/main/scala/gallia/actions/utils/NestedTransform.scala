package gallia
package actions
package utils

import plans._
import actions._
import FunctionWrappers._
import dag.RootId
import domain.PathPair
import atoms.common.AtomsUUTransforms.{_Transform1to1, _Transform1to1b, _Transform1to1U, _Transform1to1Z}
import heads.HeadsNestingHandler

// ===========================================================================
// TODO: rename: not necessarily "nested" per se (eg forX context)...
class NestedTransform(disambiguatorOpt: UnionObjectDisambiguatorOpt, adag: MetaPlan, val rootId: RootId) { // TODO: as a peer of MetaPlan rather?
  import ActionsUtils._

  // ===========================================================================
  // validation

  def vldt(c: Cls, qpathz: RPathz): Errs = qpathz.map(_.from).flatMap(vldt(c, _)) //FIXME: overwriting fields ok?

    // ---------------------------------------------------------------------------
    def vldt(c: Cls, kpath: KPath): Errs = //TODO: t220422121108 - check nesting field
      kpath
        .pipe(c.field_).toSeq
        .flatMap { f =>
          disambiguatorOpt
            .map    (_._vldt(f))
            .getOrElse(_vldt(f.forceNestedClass)) }

      // ---------------------------------------------------------------------------
      /*private - needed for forkey */
      def _vldt(c: Cls): Errs = adag.runMeta(rootId, c).allErrors

  // ===========================================================================
  // meta

  def  generateMeta(c: Cls, from: KPath)  : Cls = c.forceNestedClass(from).pipe(_meta)

  // ---------------------------------------------------------------------------
  def transformMeta(c: Cls, path : KPath) : Cls =                     c.transformNestedClass(disambiguatorOpt)(path)(_meta)
  def transformMeta(c: Cls, paths: RPathz): Cls = paths.foldLeft(c) { _.transformNestedClass(disambiguatorOpt)(_)   (_meta) }

      /*private - needed for forkey */
      def _meta(c: Cls): Cls = adag.runMeta(rootId, c).forceLeafClass

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
    val optional = c.isOptional(path.from)
    val plan     = c.field     (path.from).forceNestedClass(disambiguatorOpt).pipe(metaToAtomPlan)

    val f: _ff11 =
      if (!multiple)
        if (!optional) plan.V2.naiveRunUU
        else           plan.V2.naiveRunUU_
      else
        if (!optional) plan.V2.naiveRunZZ
        else           plan.V2.naiveRunZZ_

    val pair = PathPair(path.to, optional)

    potentialRenaming(path).toSeq :+
      disambiguatorOpt
        .map {
          case DisambiguateByClassIndex     (_)       => _Transform1to1b.replace(      f)(pair) // costly
          case DisambiguateByClassPredicateU(_, data) => _Transform1to1U.replace(data, f)(pair)
          case DisambiguateByClassPredicateZ(_, data) => _Transform1to1Z.replace(data, f)(pair) }
        .getOrElse {                                     _Transform1to1 .replace(      f)(pair) }
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
    def uu(c: Cls, optional: Boolean): _ff11 = metaToAtomPlan(c).pipe(new NestedPlan(_)).nestedRunneru2u(optional)
    def zz(c: Cls, optional: Boolean): _ff11 = metaToAtomPlan(c).pipe(new NestedPlan(_)).nestedRunnerz2z(optional)
  
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
  def parseUU(disambiguatorOpt: UnionObjectDisambiguatorOpt)(f: HeadU => HeadU): NestedTransform = { val (rootId, dag, leafId) = HeadsNestingHandler.uToU(f); new NestedTransform(disambiguatorOpt, MetaPlan(dag), rootId) }
  def parseZZ(disambiguatorOpt: UnionObjectDisambiguatorOpt)(f: HeadZ => HeadZ): NestedTransform = { val (rootId, dag, leafId) = HeadsNestingHandler.zToZ(f); new NestedTransform(disambiguatorOpt, MetaPlan(dag), rootId) }

  // ---------------------------------------------------------------------------
  def parseUU        (f: HeadU => HeadU   ): NestedTransform =  { val (rootId, dag, leafId) = HeadsNestingHandler.uToU(f); new NestedTransform(None, MetaPlan(dag), rootId) }
  def parseZZ        (f: HeadZ => HeadZ   ): NestedTransform =  { val (rootId, dag, leafId) = HeadsNestingHandler.zToZ(f); new NestedTransform(None, MetaPlan(dag), rootId) }

  def parseUZ        (f: HeadU => HeadZ   ): NestedTransform =  { val (rootId, dag, leafId) = HeadsNestingHandler.uToZ(f); new NestedTransform(None, MetaPlan(dag), rootId) }
  def parseZU        (f: HeadZ => HeadU   ): NestedTransform =  { val (rootId, dag, leafId) = HeadsNestingHandler.zToU(f); new NestedTransform(None, MetaPlan(dag), rootId) }

  def parseUV[T: WTT](f: HeadU => HeadV[T]): NestedTransform =  { val (rootId, dag, leafId) = HeadsNestingHandler.uToV(f); new NestedTransform(None, MetaPlan(dag), rootId) }
  def parseZV[T: WTT](f: HeadZ => HeadV[T]): NestedTransform =  { val (rootId, dag, leafId) = HeadsNestingHandler.zToV(f); new NestedTransform(None, MetaPlan(dag), rootId) }
}

// ===========================================================================
