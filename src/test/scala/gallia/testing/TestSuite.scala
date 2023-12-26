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
    def noop(f: HeadO => HeadO)  : Unit = { f(u).check(u) }
    def check(c: Cls, value: Obj): Unit = { u.forceAObj.identity.check(aobj(c)(value)) }
    def toOptional(key1: KeyW, more: KeyW*): AObj = testing._toOptional(u.forceAObj, (key1 +: more)) }

  // ===========================================================================
  protected trait Head__[$Data] {
    protected def end: HeadEnd

    // ===========================================================================
    def metaError(pair: (String, String)): Unit = { _metaError(pair._1, pair._2) }
    def dataError(pair: (String, String)): Unit = { _dataError(pair._1, pair._2) }

    // ---------------------------------------------------------------------------
    def metaError[$Error <: vldt._Error: WTT]: Unit = { _metaError(implicitly[WTT[$Error]].typeNode.leaf.inScopeName) }
    def dataError[$Error <: vldt._Error: WTT]: Unit = { _dataError(implicitly[WTT[$Error]].typeNode.leaf.inScopeName) }
    
    // ===========================================================================
    private[Suite] def _metaError(markers: String*): Unit = { addResult(TestValue.__metaError(end, markers)) }
    private[Suite] def _dataError(markers: String*): Unit = { addResult(TestValue.__dataError(end, markers)) } }

  // ===========================================================================
  implicit class HeadO__(u: HeadO) extends Head__[Obj] {
    protected def end = u.end()

    // ===========================================================================
    def checkMetaOnly(expected: Objs): Unit = { addResult(TestValue.__check(end, expected)) }
    def checkDataOnly(expected: Obj) : Unit = { addResult(TestValue.__check(end, expected)) }
    def checkDataOnly(expected: BObj): Unit = { checkDataOnly(expected.forceAObj.o) }

    // ---------------------------------------------------------------------------
    def check(expected: BObj): Unit = { addResult(TestValue.__check(end, expected.forceAObj)) }
    def check(expected: AObj): Unit = { addResult(TestValue.__check(end, expected)) } }

  // ===========================================================================
  implicit class HeadS__(u: HeadS) extends Head__[Objs] {
    protected def end = u.end()

    // ===========================================================================
    def equivalents(expected: BObjs)(fs: HeadZ => HeadZ*): Unit = { fs.foreach { f => f(u).check(expected) } }

    // ===========================================================================
    def checkMetaOnly(expected: Objs): Unit = { addResult(TestValue.__check(end, expected)) }
    def checkDataOnly(expected: Objs): Unit = { addResult(TestValue.__check(end, expected)) }
    
    // ===========================================================================
    def check(implicit expected: BObjs)       : Unit = { check(expected.forceAObjs) }
    def check(value1: BObj, more: BObj*)      : Unit = { check(BObjs(value1 +: more)) }
    def check(c: Cls)(value1: Obj, more: Obj*): Unit = { check(aobjs(c)((value1 +: more):_*)) }
    def check(expected: AObjs)                : Unit = { addResult(TestValue.__check(end, expected)) }
    
    // ===========================================================================
    def checkEmpty(): Unit = { addResult(TestValue.__checkPredicate(end, "expected empty result")(_.isEmpty)) } }

  // ===========================================================================
  implicit class HeadV__[T](u: HeadV[T]) extends Head__[T] {
    protected def end = u.end()

    // ---------------------------------------------------------------------------
    def check(expected: T): Unit = { addResult(TestValue.__check(end, expected)) } }

  // ===========================================================================
  def throws(v: => Any): Unit = { assert(scala.util.Try(v).isFailure, v) } // TODO: port as actual test
  
  // ---------------------------------------------------------------------------
  implicit class Vle__[V](u: V) { // TODO: as part of suite    
    def check(f: V => Boolean): Unit = { assert(f(u), u) }
    def check(expected: V    ): Unit = { assert(u == expected, (u, expected)) } }
  
  // ===========================================================================
  def addResult(value: TestValue): Unit = { TestResults.add(TestResult(name, CallSite.generate(), value)) } // TODO: t220318111014 - look into issue with testing code (see in CallSite)
}

// ===========================================================================
