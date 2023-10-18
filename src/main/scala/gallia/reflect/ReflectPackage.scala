package gallia

// ===========================================================================
package object reflect {
  lazy val low: lowlevel.ReflectionUtilsAbstraction = lowlevel.ConcreteReflectionUtils

  // ---------------------------------------------------------------------------
  /** eg           "String" */ type InScopeName    = String
  /** eg "java.lang.String" */ type FullNameString = String

  /** eg "String" instead of "java.lang.String", but None for "foo.bar.Baz" */
  type Alias       = String

  /** eg "north" for Cardinal.north */
  type EntryNameString = String }

// ===========================================================================
