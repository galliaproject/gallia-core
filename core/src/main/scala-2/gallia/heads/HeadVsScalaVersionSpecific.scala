package gallia
package heads

// ===========================================================================
trait HeadVsScalaVersionSpecific[T] { self: HeadV[T] =>
  // TODO:
  // - t240124104448 - figure out why has to differ for scala version
  // - t220916113454 - separate HeadV[T] from HeadV[Seq[U]]

  // ---------------------------------------------------------------------------
  def flattened[U : WTT](implicit ev1: T <:< Iterable[Option[U]]): HeadV[Seq[U]] = mapV(_.toSeq.flatten)

  // ---------------------------------------------------------------------------
  def min  [N : WTT](implicit ev1: T <:< Iterable[N], ev2: Numeric[N]): HeadV[N] = mapV(_.min)
  def max  [N : WTT](implicit ev1: T <:< Iterable[N], ev2: Numeric[N]): HeadV[N] = mapV(_.max)
  def range[N : WTT](implicit ev1: T <:< Iterable[N], ev2: Numeric[N]): HeadV[N] = mapV(x => ev2.minus(x.max, x.min))

  // ---------------------------------------------------------------------------
  def sum  [N : WTT](implicit ev1: T <:< Iterable[N], ev2: Numeric[N]): HeadV[N] = mapV(_.sum) }

// ===========================================================================