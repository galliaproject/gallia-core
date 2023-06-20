package gallia

// ===========================================================================
package object testing {
  /*private[gallia] */var ActualTestOpt: Option[Boolean] = Some(true)//None // TODO: until finish migrating tests

  // ---------------------------------------------------------------------------
  type HeadEnd    = gallia.heads.HeadEnd
  type ActionPlan = gallia.plans.ActionPlan

  // ===========================================================================
  def resourceContent(target: String):      String  = target.pipe(scala.io.Source.fromResource(_)).mkString
  def resourceLines  (target: String): List[String] = target.pipe(scala.io.Source.fromResource(_)).getLines().toList

  // ===========================================================================
  private[testing] def _toOptional(value: AObj, keys: Seq[KeyW]): AObj =
    value
      .copy(c = keys
      .map(_.value)
      .foldLeft(value.c)(_ toOptional _))

  // ===========================================================================
  /** typically those are private so users don't shoot themselves in the foot */
  object accessPrivate {
    def useWeakTypeTagDecorator[T: WTT, A](wtt: WTT[T])(f: WeakTypeTagDecorator[T] => A) = f(new WeakTypeTagDecorator(wtt)) }
}

// ===========================================================================
