# A Rust rewrite of zephyr's [`gen_kobject_list.sh`]

This script is in the critical path of the zephyr incremental build process and is invoked up to twice whenever *any* file in the project has changed.
With large projects that use `CONFIG_USERSPACE=y`, this can create incremental build times of 60 seconds.

In the author's benchmarks, this drop-in rewrite reduces the bare script runtime by 100x (28s to 260ms) and the total incremental rebuild time by
12x (58s to 4.9s).

## Using

Build and install the tool by cloning the repository and running `cargo install --path . --locked`. Make sure the cargo bin directory is in your `$PATH`.

Apply the following patches to zephyr (based on zephyr [7cef0e36](https://github.com/zephyrproject-rtos/zephyr/tree/7cef0e361467136064a066de718af12e00a9e7d9)):

<details>
<summary>zephyr patches</summary>

```patch
From 87912a903c7d5b38b8702020d82093ebc24db4ba Mon Sep 17 00:00:00 2001
From: Armin Brauns <armin.brauns@embedded-solutions.at>
Date: Wed, 12 Feb 2025 15:39:23 +0000
Subject: [PATCH 1/2] cmake: use variables for gen_kobject_list.py

This makes it a lot easier to use a different implementation instead.

Signed-off-by: Armin Brauns <armin.brauns@embedded-solutions.at>
---
 CMakeLists.txt   | 10 ++++------
 cmake/kobj.cmake |  5 ++---
 2 files changed, 6 insertions(+), 9 deletions(-)

diff --git a/CMakeLists.txt b/CMakeLists.txt
index 16a471ae6ee..a0f294e7323 100644
--- a/CMakeLists.txt
+++ b/CMakeLists.txt
@@ -874,19 +874,20 @@ add_custom_command(
   )

 # This is passed into all calls to the gen_kobject_list.py script.
+set(GEN_KOBJ_LIST_SCRIPT ${ZEPHYR_BASE}/scripts/build/gen_kobject_list.py)
+set(GEN_KOBJ_LIST ${PYTHON_EXECUTABLE} ${GEN_KOBJ_LIST_SCRIPT})
 set(gen_kobject_list_include_args --include-subsystem-list ${struct_tags_json})

 set(DRV_VALIDATION ${PROJECT_BINARY_DIR}/include/generated/zephyr/driver-validation.h)
 add_custom_command(
   OUTPUT ${DRV_VALIDATION}
   COMMAND
-  ${PYTHON_EXECUTABLE}
-  ${ZEPHYR_BASE}/scripts/build/gen_kobject_list.py
+  ${GEN_KOBJ_LIST}
   --validation-output ${DRV_VALIDATION}
   ${gen_kobject_list_include_args}
   $<$<BOOL:${CMAKE_VERBOSE_MAKEFILE}>:--verbose>
   DEPENDS
-  ${ZEPHYR_BASE}/scripts/build/gen_kobject_list.py
+  ${GEN_KOBJ_LIST_SCRIPT}
   ${PARSE_SYSCALLS_TARGET}
   WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
   )
@@ -1090,7 +1091,6 @@ if(CONFIG_USERSPACE)
          NO_COVERAGE_FLAGS "${compiler_flags_priv}"
   )

-  set(GEN_KOBJ_LIST ${ZEPHYR_BASE}/scripts/build/gen_kobject_list.py)
   set(PROCESS_GPERF ${ZEPHYR_BASE}/scripts/build/process_gperf.py)
 endif()

@@ -1238,7 +1238,6 @@ if(CONFIG_USERSPACE)
   add_custom_command(
     OUTPUT ${KOBJECT_PREBUILT_HASH_LIST}
     COMMAND
-    ${PYTHON_EXECUTABLE}
     ${GEN_KOBJ_LIST}
     --kernel $<TARGET_FILE:${ZEPHYR_LINK_STAGE_EXECUTABLE}>
     --gperf-output ${KOBJECT_PREBUILT_HASH_LIST}
@@ -1438,7 +1437,6 @@ if(CONFIG_USERSPACE)
   add_custom_command(
     OUTPUT ${KOBJECT_HASH_LIST}
     COMMAND
-    ${PYTHON_EXECUTABLE}
     ${GEN_KOBJ_LIST}
     --kernel $<TARGET_FILE:${ZEPHYR_LINK_STAGE_EXECUTABLE}>
     --gperf-output ${KOBJECT_HASH_LIST}
diff --git a/cmake/kobj.cmake b/cmake/kobj.cmake
index 22fa36ae44a..b0766da4f7b 100644
--- a/cmake/kobj.cmake
+++ b/cmake/kobj.cmake
@@ -16,15 +16,14 @@ function(gen_kobj gen_dir_out)
   add_custom_command(
     OUTPUT ${KOBJ_TYPES} ${KOBJ_OTYPE} ${KOBJ_SIZE}
     COMMAND
-    ${PYTHON_EXECUTABLE}
-    ${ZEPHYR_BASE}/scripts/build/gen_kobject_list.py
+    ${GEN_KOBJ_LIST}
     --kobj-types-output ${KOBJ_TYPES}
     --kobj-otype-output ${KOBJ_OTYPE}
     --kobj-size-output ${KOBJ_SIZE}
     ${gen_kobject_list_include_args}
     $<$<BOOL:${CMAKE_VERBOSE_MAKEFILE}>:--verbose>
     DEPENDS
-    ${ZEPHYR_BASE}/scripts/build/gen_kobject_list.py
+    ${GEN_KOBJ_LIST_SCRIPT}
     ${PARSE_SYSCALLS_TARGET}
     WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
     )
--
2.43.0
```

```patch
From df176ae2cabf7d7bb58e0500a330c9303d769ef1 Mon Sep 17 00:00:00 2001
From: Armin Brauns <armin.brauns@embedded-solutions.at>
Date: Wed, 12 Feb 2025 15:39:57 +0000
Subject: [PATCH 2/2] Use Rust rewrite of gen_kobject_list

Needs to be installed in $PATH.

Signed-off-by: Armin Brauns <armin.brauns@embedded-solutions.at>
---
 CMakeLists.txt   | 5 ++---
 cmake/kobj.cmake | 1 -
 2 files changed, 2 insertions(+), 4 deletions(-)

diff --git a/CMakeLists.txt b/CMakeLists.txt
index a0f294e7323..c3c8c4a3fb3 100644
--- a/CMakeLists.txt
+++ b/CMakeLists.txt
@@ -874,8 +874,8 @@ add_custom_command(
   )

 # This is passed into all calls to the gen_kobject_list.py script.
-set(GEN_KOBJ_LIST_SCRIPT ${ZEPHYR_BASE}/scripts/build/gen_kobject_list.py)
-set(GEN_KOBJ_LIST ${PYTHON_EXECUTABLE} ${GEN_KOBJ_LIST_SCRIPT})
+set(GEN_KOBJ_LIST_SCRIPT "")
+set(GEN_KOBJ_LIST gen_kobject_list)
 set(gen_kobject_list_include_args --include-subsystem-list ${struct_tags_json})

 set(DRV_VALIDATION ${PROJECT_BINARY_DIR}/include/generated/zephyr/driver-validation.h)
@@ -887,7 +887,6 @@ add_custom_command(
   ${gen_kobject_list_include_args}
   $<$<BOOL:${CMAKE_VERBOSE_MAKEFILE}>:--verbose>
   DEPENDS
-  ${GEN_KOBJ_LIST_SCRIPT}
   ${PARSE_SYSCALLS_TARGET}
   WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
   )
diff --git a/cmake/kobj.cmake b/cmake/kobj.cmake
index b0766da4f7b..2ab21b152b4 100644
--- a/cmake/kobj.cmake
+++ b/cmake/kobj.cmake
@@ -23,7 +23,6 @@ function(gen_kobj gen_dir_out)
     ${gen_kobject_list_include_args}
     $<$<BOOL:${CMAKE_VERBOSE_MAKEFILE}>:--verbose>
     DEPENDS
-    ${GEN_KOBJ_LIST_SCRIPT}
     ${PARSE_SYSCALLS_TARGET}
     WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
     )
--
2.43.0
```
</details>

## Testing

The `test.sh` script can be used to compare the outputs of the rewritten tool to the original `gen_kobject_list.py`.

1. Build a zephyr project
2. Pick an output binary, e.g. `build/zephyr/zephyr.elf` or `build/zephyr/zephyr_pre0.elf` (`$BIN` below)
3. Run `./test.sh --accept "$BIN" golddir/ "$ZEPHYR_BASE/scripts/build/gen_kobject_list.py"` to create
   a set of reference output files in `golddir/`.
4. Run `./test.sh "$BIN" golddir/ gen_kobject_list` to check that the generated output files match.

[`gen_kobject_list.sh`]: (https://github.com/zephyrproject-rtos/zephyr/blob/7cef0e361467136064a066de718af12e00a9e7d9/scripts/build/gen_kobject_list.py)
