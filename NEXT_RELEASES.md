# Current

Upcoming releases of Gallia will focus on the following aspects:

- Migration to Scala 3
- Optimization: via (optional) code generation + more reliance on macros (thanks to Scala 3 migration)
- Support for flagship project: https://github.com/anthony-cros/scalabix (Bioinformatics library in Scala), especially "trio" genetic variants portal.


# Formerly:

- Misc:
	- Address known bugs (mostly corner cases now)
	- Splitting `gallia.heads.HeadV` in four separate cases: single vs multiple values, and numerical vs non-numerical values; this will allow appropriate operations to be defined on each combination more seamlessly (rather than relying on implicit evidences)
	- Rework of the target selection mechanism, current one has too much indirection (for historical reasons); see [TargetSelectionTest.scala](https://github.com/galliaproject/gallia-testing/blob/v0.4.0/src/main/scala/galliatest/suites/TargetSelectionTest.scala)
	- Generalize the "call site" mechanism, as seen in [_ConvertToInt(origin: CallSite)](https://github.com/galliaproject/gallia-core/blob/v0.4.0/src/main/scala/gallia/atoms/common/AtomsCommonConverts.scala#L37)
	- Separate the `Cls` hierarchy and related entities into a lighter repo (gallia-meta), although the newly created metaschema (see [CHANGELOG.md](https://github.com/galliaproject/gallia-core/blob/master/CHANGELOG.md#221013105445)) will makes this more difficult
- Optimizations:
	- More work on the `Obg9` mechanism for dense data (see [CHANGELOG.md](https://github.com/galliaproject/gallia-core/blob/master/CHANGELOG.md#221013151606))
	- Offering more code generation, especially for field names (as enums), classes (as case classes) and atom plan (as hackable code)
	- More atom plan optimizations (need formalizing/homogenization of targets)
- I/O:
	- Offer streaming to/from case class collections
	- Add/improve `.checkpoint`/`.uncheckpoint` mechanism (only partially working at the moment)
	- Ability to delay execution on DAG until data from all leaves is requested (if forks)
- Basic Types:
	- Offer more type parameterization, eg decimal+scale for `BigDec`; [BasicType._Enm](https://github.com/galliaproject/gallia-core/blob/v0.4.0/src/main/scala/gallia/reflect/BasicType.scala#L293) offers a precedent now
	- Add a `_Fixed` counterpart to [BasicType._Binary](https://github.com/galliaproject/gallia-core/blob/v0.4.0/src/main/scala/gallia/reflect/BasicType.scala#L281), or as a parameter thereof
	- Add an `_OpaqueObj` type, for arbitrary key/values untracked by the schema
- Schema:
	- Offer references (to schema, classes, fields, ...) to minimize repetitions
	- Offer schema relations ("foreign keys") to simplify joins/coGroups
	- Consider adding a notion of "default" value
	- Consider adding semantics to fields - tricky: see [semantics.md#obstacles](https://github.com/galliaproject/gallia-docs/blob/v0.4.0/semantics.md#obstacles)
- Transformations:
	- Offer additional operations for union types, currently limited (see [CHANGELOG.md](https://github.com/galliaproject/gallia-core/blob/master/CHANGELOG.md#221013103753) and [SubInfosLike](https://github.com/galliaproject/gallia-core/blob/v0.4.0/src/main/scala/gallia/meta/SubInfosLike.scala#L15))
	- New mechanism for `.filterBy(X).matches(Y).thenRemove` (very common)
	- Macro-based counterparts to `transformDataClass` and `coTransformViaDataClass` (see [DataClassesTest.scala](https://github.com/galliaproject/gallia-testing/blob/v0.4.0/src/main/scala/galliatest/suites/single/DataClassesTest.scala))
- Validation:
	- Add missing validations in actions
	- Ability to enable/disable validations at runtime (see [RuntimeValidationTest.scala](https://github.com/galliaproject/gallia-testing/blob/v0.4.0/src/main/scala/galliatest/suites/RuntimeValidationTest.scala))
- Integration:
	- Other IDLs:
		- Add support for (relevant subset of) JSON schema
		- Spark's StructType/StructField
		- Support Thrift/Protobuf?
	- Integrate with R in a similar way to `gallia-python-integration` (see [CHANGELOG.md](https://github.com/galliaproject/gallia-core/blob/master/CHANGELOG.md#221013104950))
	- Offer more visualizations via Python via ScalaPy (see [CHANGELOG.md](https://github.com/galliaproject/gallia-core/blob/master/CHANGELOG.md#221013104950))
	- Improve usability from Java and Kotlin

<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>

