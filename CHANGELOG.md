# v0.4.0

Note: this is not an exhaustive list of the changes, but the most important ones

- <a name="221013105445"></a> Metaschema: see [MetaSchema.scala](https://github.com/galliaproject/gallia-core/blob/v0.4.0/src/main/scala/gallia/MetaSchema.scala) and example usage in [MetaSchemaTest.scala](https://github.com/galliaproject/gallia-testing/blob/v0.4.0/src/main/scala/galliatest/suites/MetaSchemaTest.scala)
- <a name="221013103753"></a> Union Types: limited support
	- See [union_types.md](https://github.com/galliaproject/gallia-docs/blob/v0.4.0/union_types.md)
	- See code: [Info.scala](https://github.com/galliaproject/gallia-core/blob/v0.4.0/src/main/scala/gallia/meta/Info.scala#L8-L10)
	- See usage: [UnionTypeTest.scala](https://github.com/galliaproject/gallia-testing/blob/v0.4.0/src/main/scala/galliatest/suites/UnionTypeTest.scala) (shows typical usage, hints, and `fuseToUnion`/`fissionFromUnion` functionalities)
- Additional Basic Types:
	- Enums: see [EnumTest.scala](https://github.com/galliaproject/gallia-testing/blob/v0.4.0/src/main/scala/galliatest/suites/EnumTest.scala)
	- Binary data: see [UncommonTypesTest.scala](https://github.com/galliaproject/gallia-testing/blob/v0.4.0/src/main/scala/galliatest/suites/single/UncommonTypesTest.scala)
	- Temporal data (LocalDateTime, ...): see [TimeTest.scala](https://github.com/galliaproject/gallia-testing/blob/v0.4.0/src/main/scala/galliatest/suites/TimeTest.scala)
- I/O:
	- Input:
		- New data-class based construct, eg:
			```scala
			case class Foo(s: String, i: Int)
			aobj(Foo("hello", 3)).[...]
			```
		- Homogenization of the "tax mechanism" for JSON/Table streaming (eg automatic Int conversion), see [GsonToGalliaData.scala](https://github.com/galliaproject/gallia-core/blob/v0.4.0/src/main/scala/gallia/data/json/GsonToGalliaData.scala)
	- Output:
		- Improved support for "naked" value(s) (`gallia.heads.HeadV`) output, see [HeadVTest.scala](https://github.com/galliaproject/gallia-testing/blob/v0.4.0/src/main/scala/galliatest/suites/single/HeadVTest.scala)
	- Both:
		- <a name="221014125247"></a>Apache Avro: Added support to read/write Avro files; usage:
			```scala
			// libraryDependencies += "io.github.galliaproject" %% "gallia-avro" % "0.4.0"
			import gallia.avro._
			"./episodes.avro".streamAvro().[...]
			[...].writeAvro("/tmp/foo.avro")
			```
		- <a name="221014125248"></a> Apache Parquet: Added support to read/write Parquet files (via Avro); usage
			```scala
			// libraryDependencies += "io.github.galliaproject" %% "gallia-parquet" % "0.4.0"
			import gallia.parquet._
			"./episodes.avro".streamParquet().[...]
			[...].writeParquet("/tmp/foo.parq")
			```
- Integration:
	- <a name="221013104950"></a> Python: Experimentation with Python integration (and soon R), via the excellent [ScalaPy](https://scalapy.dev/) by Shadaj Laddad
		- Pandas: see [ScalaPyPandasTest.scala](https://github.com/galliaproject/gallia-python-integration/blob/v0.4.0/gallia-pandas/src/main/scala/gallia/pandas/ScalaPyPandasTest.scala)
		- Seaborn (for visualization): see [GalliaVizTest.scala#L37](https://github.com/galliaproject/gallia-python-integration/blob/master/gallia-python-viz/src/main/scala/gallia/pyviz/GalliaVizTest.scala#L37)
	- Java: See [GalliaJava.scala gist](https://gist.github.com/anthony-cros/9570ea90bb8430232ece3249eccac11a) (early attempt), mostly hindered by this Scala [bug](https://users.scala-lang.org/t/issue-with-calling-scala-code-from-java/8321/8), which would make it prohibitively verbose to use Gallia in Java, at least as it is
- Transformations::
	- `transformDataClass`: see [DataClassesTest.scala](https://github.com/galliaproject/gallia-testing/blob/v0.4.0/src/main/scala/galliatest/suites/single/DataClassesTest.scala#L13)
	- `cotransformViaDataClass`: see [DataClassesTest.scala](https://github.com/galliaproject/gallia-testing/blob/v0.4.0/src/main/scala/galliatest/suites/single/DataClassesTest.scala#L42)
	- custom `.aggregateBy: see [AggregatingTest.scala](https://github.com/galliaproject/gallia-testing/blob/v0.4.0/src/main/scala/galliatest/suites/multiple/AggregatingTest.scala#L17-L62)
	- custom `.reduce`: see [ReducingTest.scala](https://github.com/galliaproject/gallia-testing/blob/v0.4.0/src/main/scala/galliatest/suites/multiple/ReducingTest.scala#L69-L104)
	- `.removeIf/.setDefault` "if value for": see [RemoveIfTest.scala](https://github.com/galliaproject/gallia-testing/blob/v0.4.0/src/main/scala/galliatest/suites/single/RemoveIfTest.scala#L181)
	- `.dropWhile/.takeWhile`: see [FilterByTest.scala](https://github.com/galliaproject/gallia-testing/blob/v0.4.0/src/main/scala/galliatest/suites/multiple/FilterByTest.scala#L19)
	- `.zipSameSize`: see [MergingTest.scala](https://github.com/galliaproject/gallia-testing/blob/v0.4.0/src/main/scala/galliatest/suites/multiple/MergingTest.scala#L92)
	- `.unpivotOne`: see [UnpivotTest.scala](https://github.com/galliaproject/gallia-testing/blob/v0.4.0/src/main/scala/galliatest/suites/single/UnpivotTest.scala#L25)
	- `.asNewKeys[SomeEnum]`: see [DeserializeTest.scala](https://github.com/galliaproject/gallia-testing/blob/v0.4.0/src/main/scala/galliatest/suites/single/DeserializeTest.scala#L25)
	- `filterBy(X).isPresent/isMissing`: see [FilterByTest.scala](https://github.com/galliaproject/gallia-testing/blob/v0.4.0/src/main/scala/galliatest/suites/multiple/FilterByTest.scala#L55)
	- `filterOutEmptyLines`: see [FilterByTest.scala](https://github.com/galliaproject/gallia-testing/blob/v0.4.0/src/main/scala/galliatest/suites/multiple/FilterByTest.scala#L84)
	- `.custom` mechanism improvements: see [CustomTest.scala](https://github.com/galliaproject/gallia-testing/blob/v0.4.0/src/main/scala/galliatest/suites/CustomTest.scala#L19)
	- Renamed operations: (unchanged behaviors)
		- `zen` -> `thn`: see [ForXTest.scala](https://github.com/galliaproject/gallia-testing/blob/v0.4.0/src/main/scala/galliatest/suites/single/ForXTest.scala#L17)
		- `untuplify`-> `deserialize` see [DeserializeTest.scala](https://github.com/galliaproject/gallia-testing/blob/v0.4.0/src/main/scala/galliatest/suites/single/DeserializeTest.scala)
- Optimizations:
	- <a name="221013151606"></a>Experimental code for memory optimization of dense entities (basically `class Obg9(size: Int, data: Array[Any])`), along with some example operations: see code at [Obg9.scala](https://github.com/galliaproject/gallia-core/blob/v0.4.0/src/main/scala/gallia/obg9/Obg9.scala) and usage at [Obg9Test.scala](https://github.com/galliaproject/gallia-testing/blob/v0.4.0/src/main/scala/galliatest/suites/Obg9Test.scala)
	- ["Spilling"](https://github.com/galliaproject/gallia-core/blob/v0.4.0/README.md#spilling) mechanism optimizations: see [GalliaSpilling.scala](https://github.com/galliaproject/gallia-core/blob/v0.4.0/src/main/scala/gallia/atoms/utils/GalliaSpilling.scala)
- Execution DAG improvements:
	- For Iterator mode (`gallia.streamer.IteratorStreamer`):
		- Forking: see [IteratorStreamer.scala#fork()](https://github.com/galliaproject/gallia-core/blob/v0.4.0/src/main/scala/gallia/streamer/IteratorStreamer.scala#L26)
		- Data regeneration (via closure): see [IteratorStreamer.scala#from()](https://github.com/galliaproject/gallia-core/blob/v0.4.0/src/main/scala/gallia/streamer/IteratorStreamer.scala#L14)
	- Added support for more combinations: `HeadV -> HeadO`, `HeadV -> HeadZ`, `(HeadV, HeadV) -> HeadV`, and `HeadV -> HeadO` for instance for the likes of [ReducingTest.scala#dressUp()](https://github.com/galliaproject/gallia-testing/blob/v0.4.0/src/main/scala/galliatest/suites/multiple/ReducingTest.scala#L93)
	- More tests for edge cases: see e.g. [GraphTest.scala#diamond()](https://github.com/galliaproject/gallia-testing/blob/v0.4.0/src/main/scala/galliatest/suites/GraphTest.scala#L58)


# v0.3.X

Not tracked here, see commits


# v0.2.X

Not tracked here, see commits
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
