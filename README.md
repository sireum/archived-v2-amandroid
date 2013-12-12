sireum-amandroid-private
========================
To run test case, you need to do following four steps:

1. Set environment variable ANDROID_LIB_DIR points to the library-pilar-file's container directory.
2. Set environment variable ANDROID_TEST_DIR points to the source directory. Inside source directory we support following dirs: maliciousArbor, droidBench, maliciousapk, normalapk, testapk. (If you want you can specify the dir you want to include by modify org.sireum.amandroid.example.interprocedural.InterproceduralExamples.scala file.)
3. Modify CompleteRFAtest.scala under org.sireum.amandroid.test.interprocedural package to use corresponding dirs.
4. Set environment variable ANDROID_OUTPUT_DIR points to the directory you want to output results.
