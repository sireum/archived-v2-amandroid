#Amandroid challenges

Before you take one of the challenges, please contact [`@fgwei`](https://github.com/fgwei) to let him aware. And put a mark (e.g., Resolving by [`@fgwei`](https://github.com/fgwei)) in the end of the challenge to avoid any situation of conflict. You can create a PR following guidance in [CONTRIBUTING.md](CONTRIBUTING.md) after you resolved it. 

##Continuous tasks

All the APIs should be documented. (Post by [`@fgwei`](https://github.com/fgwei))

Error handling in the code need to be cleaned. (Post by [`@fgwei`](https://github.com/fgwei))

Amandroid documentations need to be revised. (Post by [`@fgwei`](https://github.com/fgwei))(Resolving by [`@fgwei`](https://github.com/fgwei))

##sireum-amandroid
In package `org.sireum.amandroid.parser`, the LayoutFileParser.scala and ManifestParser.scala only can handle plain text xml files. Better design is to read from raw xml files from apk directly, and parse the equivalent information as current parsers. (Important!) (Post by [`@fgwei`](https://github.com/fgwei))

In package `org.sireum.amandroid.appInfo`, the ReachableInfoCollector.scala need to be updated for adding more callbacks. (Post by [`@fgwei`](https://github.com/fgwei))

##sireum-amandroid-alir
In package `org.sireum.amandroid.alir.pta.reachingFactsAnalysis.model`:

- We need to add more models for android apis. (Post by [`@fgwei`](https://github.com/fgwei))
- Models actually sharing similar desings, the best way of doing it is designing a DSL to write the model in a simpler way and generate the model codes automatically. (Important!) (Post by [`@fgwei`](https://github.com/fgwei))
- API model need to be redesigned to input/output general datas, which allows multiple points-to analysis can share the same model, e.g., [`SuperSpark`](https://github.com/sireum/jawa/blob/master/sireum-jawa-alir/src/main/scala/org/sireum/jawa/alir/pta/suspark/InterproceduralSuperSpark.scala) and [`RFA`](https://github.com/sireum/jawa/tree/master/sireum-jawa-alir/src/main/scala/org/sireum/jawa/alir/pta/reachingFactsAnalysis).  (Post by [`@fgwei`](https://github.com/fgwei))

##sireum-amandroid-concurrent
In package `org.sireum.amandroid.concurrent`, the Actors setting and configuration need to be completed, currently only make sure how the logic works, but we need to make it more beatuiful. (Post by [`@fgwei`](https://github.com/fgwei))

##sireum-amandroid-dedex
In package `org.sireum.amandroid.dedex`,

- Register type resolving in DedexTypeResolver.scala and DexInstructionToPilarParser need to be tested and cleaned (or even redesigned). The main beast is the const4 resoling, as it can be `int`/`short`/`boolean` 0 or `object` null. (Important!) (Post by [`@fgwei`](https://github.com/fgwei))
- Make the decompiling process faster. (Post by [`@fgwei`](https://github.com/fgwei))

Package `org.sireum.amandroid.java` need to be added, and developing pilar to java translator. (Major task.) (Post by [`@fgwei`](https://github.com/fgwei))

