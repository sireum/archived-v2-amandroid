sireum-amandroid-private
========================
To run test case, you need to do following two steps:

1. Add symbolic link from sireum-amandroid-test/src/test/resources/org/sireum/amandroid/example/interprocedural to where you store your test files. Currently, we support following dirs: maliciousArbor, droidBench, maliciousapk, normalapk, testapk. 
2. Modify CompleteRFAtest.scala under org.sireum.amandroid.test.interprocedural package to use corresponding dirs.
3. Set environment variable ANDROID_OUTPUT_DIR points to the directory you want to output results.
