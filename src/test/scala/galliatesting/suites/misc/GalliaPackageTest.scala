package galliatesting
package suites
package misc

// ===========================================================================
object GalliaPackageTest extends utest.TestSuite {
 import utest._

  // ===========================================================================
  val tests = Tests {
    import gallia._

    test("gallia.cls") { assert(cls(f.string, g.int) .formatCompactJson == """{"fields":[{"key":"f","info":{"optional":false,"union":[{"multiple":false,"valueType":"_String"}]}},{"key":"g","info":{"optional":false,"union":[{"multiple":false,"valueType":"_Int"}]}}]}""") }
    test("gallia.obj") { assert(obj(f -> foo, g -> 1).formatCompactJson == """{"f":"foo","g":1}""") }
    test("gallia.objs varargs") { assert(objs(    obj(f -> foo1, g -> 1), obj(f -> foo2, g -> 2) ).toListAndTrash.map(_.formatCompactJson) == List("""{"f":"foo1","g":1}""", """{"f":"foo2","g":2}""")) }
    test("gallia.objs list")    { assert(objs(Seq(obj(f -> foo1, g -> 1), obj(f -> foo2, g -> 2))).toListAndTrash.map(_.formatCompactJson) == List("""{"f":"foo1","g":1}""", """{"f":"foo2","g":2}""")) }

    test { Predef.assert(gallia.aobjFromDataClass(TestMeta.Foo("x", "y"))._forceResult.c.formatCompactJson == """{"fields":[{"key":"a","info":{"optional":false,"union":[{"multiple":false,"valueType":"_String"}]}},{"key":"A","info":{"optional":false,"union":[{"multiple":false,"valueType":"_String"}]}}]}""") }
    test { Predef.assert(gallia.aobjFromDataClass(TestMeta.Foo("x", "y"))._forceResult.o.formatCompactJson == """{"a":"x","A":"y"}""") } } }

// ===========================================================================
