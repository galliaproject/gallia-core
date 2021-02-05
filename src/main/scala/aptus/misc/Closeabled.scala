package aptus

// ===========================================================================
final class Closeabled[A](val u: A, val cls: Closeable) extends Closeable { // TODO: look into geny for Iterator version?
    override def close() { cls.close() }

    // ---------------------------------------------------------------------------
    def consume[B](f: A =>            B ):            B  = { val result = f(u); close(); result }
    def     map[B](f: A =>            B ): Closeabled[B] = Closeabled.fromPair(f(u), cls)
    def flatMap[B](f: A => Closeabled[B]): Closeabled[B] = f(u).thn { x => Closeabled.from(x.u, Seq(x.cls, this.cls)) }
  }

  // ===========================================================================
  object Closeabled {

    def from    [A <: Closeable](a: A)                        : Closeabled[A] = new Closeabled(a, a)
    def from    [A]             (a: A, values: Seq[Closeable]): Closeabled[A] = new Closeabled(a, closeable(values))
    def fromPair[A]             (pair: (A, Closeable))        : Closeabled[A] = new Closeabled(pair._1, closeable(pair._2))

    // ---------------------------------------------------------------------------
    @ordermatters private def closeable[A](values: Seq[Closeable]           ): Closeable = new java.io.Closeable { def close() { values.foreach(_.close()) } }
    @ordermatters private def closeable[A](cls1: Closeable, more: Closeable*): Closeable = closeable(cls1 +: more)
  }

// ===========================================================================