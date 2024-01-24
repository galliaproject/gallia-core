package gallia
package heads

// ===========================================================================
trait HeadVsScalaVersionSpecific[T: WTT] { self: HeadV[T] =>
  // TODO:
  // - t240124104448 - figure out why has to differ for scala version
  // - t220916113454 - separate HeadV[T] from HeadV[Seq[U]]

  def flattened[U](implicit ev1: T <:< Iterable[Option[U]]): HeadV[Seq[U]] = mapV(_.toSeq.flatten)(typeArgTypeArg(implicitly[WTT[T]]))

  // ---------------------------------------------------------------------------
  def min  [N](implicit ev1: T <:< Iterable[N], ev2: Numeric[N]): HeadV[N] = mapV(_.min)                       (typeArg(implicitly[WTT[T]]))
  def max  [N](implicit ev1: T <:< Iterable[N], ev2: Numeric[N]): HeadV[N] = mapV(_.max)                       (typeArg(implicitly[WTT[T]]))
  def range[N](implicit ev1: T <:< Iterable[N], ev2: Numeric[N]): HeadV[N] = mapV(x => ev2.minus(x.max, x.min))(typeArg(implicitly[WTT[T]]))

  // ---------------------------------------------------------------------------
  def sum  [N](implicit ev1: T <:< Iterable[N], ev2: Numeric[N]): HeadV[N] = mapV(_.sum)                       (typeArg(implicitly[WTT[T]])) }

// ===========================================================================