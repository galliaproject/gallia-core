package gallia
package reflect

import aptus.Seq_

// ===========================================================================
class Instantiator(
      private val f      : List[Any] => Any,
              val nesting: Map[String, Instantiator]) {

    override def toString = if (isPlaceholder) "Instantiator.Placeholder" else "Instantiator([...])"

    // ---------------------------------------------------------------------------
    def isPlaceholder: Boolean = f == null // easier for now...

    // ---------------------------------------------------------------------------
    @inline def construct         (args: Seq[Any] /* Obj uses Seq */): Any = f(args.toList)
    @inline def withEnumeratumName(name: String)                     : Any = { assert(f!= null); f(List(name)) } }

  // ===========================================================================
  object Instantiator {
    val Placeholder: Instantiator = new Instantiator(f = null, nesting = Map()) // easier for now...

    // ---------------------------------------------------------------------------
    /* don't rename, see 231215104241 */ def from2[T1, T2](f: (T1, T2) => Any, nesting: Map[String, Instantiator]): Instantiator =
      new Instantiator(
        (anys: List[Any]) => {
          val (a1, a2) = anys.force.tuple2
          f(  a1.asInstanceOf[T1],
              a2.asInstanceOf[T2]) },
        nesting)

    // ---------------------------------------------------------------------------
    // TODO: create macro (simplifies other macro)...

    /* don't rename, see 231215104241 */ def from1[T1]        (f:  T1          => Any, nesting: Map[String, Instantiator]): Instantiator = new Instantiator(x => { val  a1          = x.force.one   ; f(a1.asInstanceOf[T1])                                           }, nesting)
    /* don't rename, see 231215104241 */ def from3[T1, T2, T3](f: (T1, T2, T3) => Any, nesting: Map[String, Instantiator]): Instantiator = new Instantiator(x => { val (a1, a2, a3) = x.force.tuple3; f(a1.asInstanceOf[T1], a2.asInstanceOf[T2], a3.asInstanceOf[T3]) }, nesting)

    /* don't rename, see 231215104241 */ def from4[T1, T2, T3, T4]    (f: (T1, T2, T3, T4)     => Any, nesting: Map[String, Instantiator]): Instantiator = new Instantiator(x => { val (a1, a2, a3, a4)     = x.force.tuple4; f(a1.asInstanceOf[T1], a2.asInstanceOf[T2], a3.asInstanceOf[T3], a4.asInstanceOf[T4])                      }, nesting)
    /* don't rename, see 231215104241 */ def from5[T1, T2, T3, T4, T5](f: (T1, T2, T3, T4, T5) => Any, nesting: Map[String, Instantiator]): Instantiator = new Instantiator(x => { val (a1, a2, a3, a4, a5) = x.force.tuple5; f(a1.asInstanceOf[T1], a2.asInstanceOf[T2], a3.asInstanceOf[T3], a4.asInstanceOf[T4], a5.asInstanceOf[T5]) }, nesting)

    /* don't rename, see 231215104241 */ def from10[T1, T2, T3, T4, T5, T6, T7, T8, T9, TA](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, TA) => Any, nesting: Map[String, Instantiator]): Instantiator = new Instantiator(x => { val (a1, a2, a3, a4, a5, a6, a7, a8, a9, aA) = x.force.tuple10; f(
      a1.asInstanceOf[T1], a2.asInstanceOf[T2], a3.asInstanceOf[T3], a4.asInstanceOf[T4], a5.asInstanceOf[T5],
      a6.asInstanceOf[T6], a7.asInstanceOf[T7], a8.asInstanceOf[T8], a9.asInstanceOf[T9], aA.asInstanceOf[TA]) }, nesting)

    // ---------------------------------------------------------------------------
    /* don't rename, see 231215104241 */ def enumeratumWithName[T1](f: T1 => Any): Instantiator =
      new Instantiator(
        f       = x => f(x.force.one.asInstanceOf[T1]),
        nesting = Map())
  }

// ===========================================================================