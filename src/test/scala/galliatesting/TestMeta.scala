package galliatesting

// ===========================================================================
object TestMeta {
  case class _Default01(f: String = "foo", g: Int = 1) { def op: Double = (f.size + g) * 1.1 }
  case class __Default01(p: Seq[_Default01])
  case class __Default01b(p: Seq[Int])

  // ---------------------------------------------------------------------------
  case class Foo (a: String, A: String)
  case class Foo2(a: String, A: Option[String])
  case class Foo3(a: String, A: Seq   [String])
  case class Foo4(a: String, A: Option[Seq[String]])

  case class Bar(a: String, A: String, i: Int)
  
  case class Baz1(a: String, A: String, q:               Qux  )
  case class Baz2(a: String, A: String, q:        Option[Qux] )
  case class Baz3(a: String, A: String, q:        Seq   [Qux] ) { require(q         .nonEmpty , this) }
  case class Baz4(a: String, A: String, q: Option[Seq   [Qux]]) { require(q.forall(_.nonEmpty), this) }
  
    case class Qux(i: Int)

  case class Quux1 (f: String, g: Int)
  case class Quux2a(f: String)
  case class Quux2b(f2: Int, h: Boolean)

  case class Quux2c         (f : Int, h: Boolean)
  case class f_Int$h_Boolean(f : Int, h: Boolean)

  // ---------------------------------------------------------------------------
case class $f_String       (p: Seq[f_String])
case class $f_Int$h_Boolean(p:     f_Int$h_Boolean)
  case class f_String  (f:            String )
  case class f_String_ (f: Option[    String])
  case class f_Strings (f:        Seq[String])
  case class f_Strings_(f: Option[Seq[String]])

  // ---------------------------------------------------------------------------
  case class f2_String  (f2:            String )
  case class f2_String_ (f2: Option[    String])
  case class f2_Strings (f2:        Seq[String])
  case class f2_Strings_(f2: Option[Seq[String]])

  // ---------------------------------------------------------------------------
  case class f2_Int$h_Boolean(f2: Int, h: Boolean)

  // ---------------------------------------------------------------------------
  case class Default01DataClass(f:     String , g: Int)
  case class Default02DataClass(f: Seq[String], g: Int)
  case class InvalidDataClass1 (f:     String , g: java.io.File)
       class InvalidDataClass2 (f:     String , g: Int)

  // ===========================================================================
  case class MyComplexData(
        b            : Boolean,

        myString     :            String   ,
        myOptInt     : Option[    Int     ],
        myDoubles    :        Seq[Double ] ,
        myOptBooleans: Option[Seq[Boolean]],

        myObj        :            MyComplexSubData ,
        myOptObj     : Option[    MyComplexSubData],
        myObjs       :        Seq[MyComplexSubData],
        myOptObjs    : Option[Seq[MyComplexSubData]],

        myClosing    : Boolean)

      // ---------------------------------------------------------------------------
      case class MyComplexSubData(
          mySubString: String,
          mySubInt   : Int)

  // ===========================================================================
   sealed trait MyEnum1 extends enumeratum.EnumEntry

    object MyEnum1 extends enumeratum.Enum[MyEnum1] {
      val values = findValues

      // ---------------------------------------------------------------------------
      case object a extends MyEnum1
      case object b extends MyEnum1 }

    // ---------------------------------------------------------------------------
    sealed trait MyEnum2 extends enumeratum.EnumEntry

      object MyEnum2 extends enumeratum.Enum[MyEnum2] {
        val values = findValues

        // ---------------------------------------------------------------------------
        case object foo  extends MyEnum2
        case object foo2 extends MyEnum2 } }

// ===========================================================================
