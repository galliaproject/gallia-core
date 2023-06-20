package gallia
package testing

// ===========================================================================
trait Suite {
  private val name = getClass.getSimpleName.stripSuffix("$")

  // ===========================================================================
  implicit class BObjs__(u: BObjs) { def noop(f: HeadS => HeadS) = { f(u).check(u) } }
  implicit class AObjs__(u: AObjs) { def noop(f: HeadS => HeadS) = { f(u).check(u) } }
  implicit class AObj__ (u: AObj ) { def noop(f: HeadO => HeadO) = { f(u).check(u) } }
  implicit class BObj__ (u: BObj)  {
    def noop(f: HeadO => HeadO) = { f(u).check(u) }
    def check(c: Cls, value: Obj) { u.forceAObj.identity.check(aobj(c)(value)) }  
    def toOptional(key1: KeyW, more: KeyW*): AObj = testing._toOptional(u.forceAObj, (key1 +: more)) }

  // ===========================================================================
  protected trait Head__[$Data] {
    protected def end: HeadEnd

    // ===========================================================================
    def metaError(pair: (String, String)) { _metaError(pair._1, pair._2) }
    def dataError(pair: (String, String)) { _dataError(pair._1, pair._2) }

    // ---------------------------------------------------------------------------
    def metaError[$Error <: vldt._Error: WTT] { _metaError(typeNode[$Error].leaf.inScopeName) }
    def dataError[$Error <: vldt._Error: WTT] { _dataError(typeNode[$Error].leaf.inScopeName) }       
    
    // ===========================================================================
    private[Suite] def _metaError(markers: String*) { addResult(TestValue.__metaError(end, markers)) }
    private[Suite] def _dataError(markers: String*) { addResult(TestValue.__dataError(end, markers)) } }

  // ===========================================================================
  implicit class HeadO__(u: HeadO) extends Head__[Obj] {
    protected def end = u.end()

    // ===========================================================================
    def checkMetaOnly(expected: Objs) { addResult(TestValue.__check(end, expected)) }
    def checkDataOnly(expected: Obj)  { addResult(TestValue.__check(end, expected)) }
    def checkDataOnly(expected: BObj) { checkDataOnly(expected.forceAObj.o) }

    // ---------------------------------------------------------------------------
    def check(expected: BObj) { addResult(TestValue.__check(end, expected.forceAObj)) }
    def check(expected: AObj) { addResult(TestValue.__check(end, expected)) } }

  // ===========================================================================
  implicit class HeadS__(u: HeadS) extends Head__[Objs] {
    protected def end = u.end()

    // ===========================================================================
    def equivalents(expected: BObjs)(fs: HeadZ => HeadZ*) { fs.foreach { f => f(u).check(expected) } }

    // ===========================================================================
    def checkMetaOnly(expected: Objs) { addResult(TestValue.__check(end, expected)) }
    def checkDataOnly(expected: Objs) { addResult(TestValue.__check(end, expected)) }
    
    // ===========================================================================
    def check(implicit expected: BObjs)        { check(expected.forceAObjs) }
    def check(value1: BObj, more: BObj*)       { check(BObjs(value1 +: more)) }
    def check(c: Cls)(value1: Obj, more: Obj*) { check(aobjs(c)((value1 +: more):_*)) }
    def check(expected: AObjs)                 { addResult(TestValue.__check(end, expected)) }
    
    // ===========================================================================
    def checkEmpty() { addResult(TestValue.__checkPredicate(end, "expected empty result")(_.isEmpty)) } }

  // ===========================================================================
  implicit class HeadV__[T](u: HeadV[T]) extends Head__[T] {
    protected def end = u.end()

    // ---------------------------------------------------------------------------
    def check(expected: T) { addResult(TestValue.__check(end, expected)) } }

  // ===========================================================================
  def throws(v: => Any) { assert(scala.util.Try(v).isFailure, v) } // TODO: port as actual test
  
  // ---------------------------------------------------------------------------
  implicit class Vle__[V](u: V) { // TODO: as part of suite    
    def check(f: V => Boolean) { assert(f(u), u) }
    def check(expected: V    ) { assert(u == expected, (u, expected)) } }
  
  // ===========================================================================
  def addResult(value: TestValue) { TestResults.add(TestResult(name, CallSite.generate(), value)) } // TODO: t220318111014 - look into issue with testing code (see in CallSite)
}

// ===========================================================================
