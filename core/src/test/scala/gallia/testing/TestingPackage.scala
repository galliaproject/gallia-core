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

  def resourceFilePath(target: String): String = com.google.common.io.Resources.getResource(target).getPath // eg /home/tony/[...]/core/bin/core/scala-2.13/test-classes/test.json

  // ===========================================================================
  private[testing] def _toOptional(value: AObj, keys: Seq[KeyW]): AObj =
    value
      .copy(c = keys
      .map(_.value)
      .foldLeft(value.c)(_ toOptional _)) }

// ===========================================================================
