IF NOT DEFINED JAVA_HEAP_SIZE (
  SET JAVA_HEAP_SIZE=1024m
)
IF NOT DEFINED MAX_PERM_SIZE (
  SET MAX_PERM_SIZE=128m
)
IF NOT DEFINED RESERVED_CODE_CACHE_SIZE (
  SET RESERVED_CODE_CACHE_SIZE=300m
)
set PROJECT_DIR=%~dp0\..\..
set SBT_DIR=%PROJECT_DIR%\sbt
java -Divy.home=%SBT_DIR%/ivy -Dsbt.ivy.home=%SBT_DIR%/sbtivy -Dsbt.boot.directory=%SBT_DIR%/boot -Dfile.encoding=UTF8 %JAVA_OPTS% -XX:ReservedCodeCacheSize=%RESERVED_CODE_CACHE_SIZE% -Xss1M -XX:+CMSClassUnloadingEnabled -Xmx%JAVA_HEAP_SIZE% -jar %PROJECT_DIR%\tools\bin\sbt-launch.jar %SBT_OPTS% %*