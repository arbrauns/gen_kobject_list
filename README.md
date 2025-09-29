# A Rust rewrite of zephyr's [`gen_kobject_list.py`]

This script is in the critical path of the zephyr incremental build process and is invoked up to twice whenever *any* file in the project has changed.
With large projects that use `CONFIG_USERSPACE=y`, this can create incremental build times of 60 seconds.

In the author's benchmarks, this drop-in rewrite reduces the bare script runtime by 100x (28s to 260ms) and the total incremental rebuild time by
12x (58s to 4.9s).

## Using

Build and install the tool by cloning the repository and running `cargo install --path . --locked`. Make sure the cargo bin directory is in your `$PATH`.

The binary will be detected and used automatically by the Zephyr CMake build system, as indicated by output like the one below:

```
$ west build
[...]
-- Found gen_kobject_list: [$HOME]/.cargo/bin/gen_kobject_list
[...]
```

## Testing

The `test.sh` script can be used to compare the outputs of the rewritten tool to the original `gen_kobject_list.py`.

1. Build a zephyr project
2. Pick an output binary, e.g. `build/zephyr/zephyr.elf` or `build/zephyr/zephyr_pre0.elf` (`$BIN` below)
3. Run `./test.sh --accept "$BIN" golddir/ "$ZEPHYR_BASE/scripts/build/gen_kobject_list.py"` to create
   a set of reference output files in `golddir/`.
4. Run `./test.sh "$BIN" golddir/ gen_kobject_list` to check that the generated output files match.

[`gen_kobject_list.py`]: (https://github.com/zephyrproject-rtos/zephyr/blob/7cef0e361467136064a066de718af12e00a9e7d9/scripts/build/gen_kobject_list.py)
