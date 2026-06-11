#import "@preview/diatypst:0.9.3": *
#import "@preview/cetz:0.5.1"
#import "@preview/cetz-plot:0.1.4": chart

#let bg-color = white
#let title-color = purple.desaturate(20%).darken(30%)
#let title = "Surgically oxidising the Zephyr build system"
#let subtitle = "for 100x performance improvements"
#let authors = ("Armin Brauns",)
#show: slides.with(
  title: title,
  subtitle: subtitle,
  date: "2026-06-17",
  authors: authors,
  toc: false,
  title-color: title-color,
  bg-color: bg-color,
  count: "number",
  first-slide: false,
)
#set heading(numbering: none)
#let repo-url = link("https://github.com/arbrauns/gen_kobject_list")

// custom title page with SILA logo
#{
  set page(footer: none, header: none, margin: 0cm)
  block(
    inset: (x:1cm, y:1em),
    fill: title-color,
    width: 100%,
    height: 60%,
    align(bottom)[#text(2.0em, weight: "bold", fill: bg-color, title)]
  )
  block(
    height: 30%,
    width: 100%,
    inset: (x:1cm,top:0cm, bottom: 1em),
    {
      text(1.4em, fill: title-color, weight: "bold", subtitle)
      align(left+bottom, authors.join(", ", last: " & "))
      place(right+bottom, image(width: 35%, "sila.svg", format: "svg", alt: "SILA company logo"))
    }
  )
}

#set page(
  foreground: place(
    bottom+right,
    dx: -2%,
    dy: -5%,
    image(width: 18%, "sila.svg", format: "svg", alt: "SILA company logo")
  )
)

== About me

At work:
  - Embedded software engineer at _SILA Embedded Solutions GmbH_
  - Usually somewhere between C and voltage levels
  - No embedded Rust - so far!

In my own time:
  - Rust is a great language to have fun with!
  - Most hobby projects are at a higher level - web services etc.

== The zephyr build system

- Unified development experience for a ton of different environments
  (architecures, hardware, application scales,~...)
- Comes at a cost:
  - #{sym.approx}30~000 lines of CMake
  - #{sym.approx}20~000 lines of python
  - sometimes, ridiculously long build times:
    #let old-results = [
      ```
      $ west build --pristine [...]
      $ touch src/main.c
      $ time west build
      [...]
      Executed in   36.53 secs
         usr time   34.68 secs
         sys time    2.46 secs
      ```
    ]
    #old-results

== Digression: userspace (`CONFIG_USERSPACE`)

#v(2em)

- Allows running threads with reduced permissions
- Enforced by Memory Protection Unit (MPU) and software checks in system calls
- Permissions are granted per kernel object (mutexes, pipes, threads, devices,~...)

== Kobject metadata

Each kernel object has a bit of metadata:
```c
struct k_object {
  void *name;
  uint8_t perms[CONFIG_MAX_THREAD_BYTES];
  uint8_t type;
  uint8_t flags;
  union k_object_data data;
};
```

#sym.arrow.r.double Used to validate kernel objects passed as syscall
parameters by userspace

#pagebreak()

#v(2em)

- Metadata needs to live in trusted kernel memory
- Kobjects may be anywhere in memory
- Fast mapping from kobject address to metadata is needed

#pagebreak()

#v(2em)

Solution:
1. Kobject instances are extracted from the ELF binary's debug information
2. Metadata is created and indexed using a Perfect Hash Function
3. Indexed metadata is compiled back into ELF

More information: `doc/kernel/usermode/kernelobjects.rst`

== `gen_kobject_list.py`

#v(1em)

#figure(
  {
    // undo raw text formatting by diatypst
    show box: it => {
      if it.body.func() == raw {
        it.body
      } else {
        it
      }
    }

    let content-rect(pos, it, ..args) = {
      cetz.draw.content(
        pos,
        it,
        stroke: 1pt + black,
        padding: 0.8em,
        frame: "rect",
        ..args
      )
    }

    cetz.canvas({
      import cetz.draw: line

      let input-elf = [`build/zephyr/
    zephyr_pre*.elf`]
      let input-struct-tags = [`build/zephyr/
    misc/generated/struct_tags.json`]
      let script = [`scripts/build/gen_kobject_list.py`]
      let output = [`build/zephyr/
    kobject_hash.gperf`]

      let bg-input = blue.lighten(80%)
      let bg-script = yellow.lighten(80%)
      let bg-output = green.lighten(80%)
      content-rect(
        (0, 6),
        input-elf,
        name: "input-elf",
        fill: bg-input,
      )
      content-rect(
        (6, 6),
        input-struct-tags,
        name: "input-struct-tags",
        fill: bg-input,
      )
      content-rect(
        (1, 3.5),
        script,
        name: "script",
        fill: bg-script,
      )
      content-rect(
        (1, 1),
        output,
        name: "output",
        fill: bg-output,
      )

      let arrow-style = (mark: (end: "stealth", fill: black, scale: 1.2, offset: 2pt), stroke: 0.5pt)
      line("input-elf", "script", ..arrow-style)
      line("input-struct-tags", "script", ..arrow-style)
      line("script", "output", ..arrow-style)
    })
  },
  alt: "The gen_kobject_list.py script takes two inputs, the zephyr_pre*.elf binary and the struct_tags.json file, and produces one output, the kobject_hash.gperf file"
)

#pagebreak()

`struct_tags.json` input file contains names of network and API structs:

```json
{
    "__net_socket": [
        "modem_socket",
        "net_context",
        "quic_stream",
        "quic_context",
        "tls_context",
        "websocket_context",
        [...]
    ],
    "__subsystem": [
        "gpio_driver_api",
        "reset_driver_api",
        [...]
```

#pagebreak()

`kobject_hash.gperf` output file contains metadata of detected kobjects:

```gperf
[...]
"\x7c\x11\x18\x10", {0}, K_OBJ_STACK,
    0 | K_OBJ_FLAG_INITIALIZED, { .unused = 0 }

"\x94\x11\x18\x10", {0}, K_OBJ_MSGQ,
    0 | K_OBJ_FLAG_INITIALIZED, { .unused = 0 }

"\x78\x14\x18\x10", {0}, K_OBJ_THREAD, 0, { .thread_id = 1 }

"\x28\xcc\x00\x18", {0}, K_OBJ_DRIVER_GPIO,
    0 | K_OBJ_FLAG_DRIVER, { .unused = 0 }

"\x44\xcc\x00\x18", {0}, K_OBJ_DRIVER_GPIO,
    0 | K_OBJ_FLAG_DRIVER, { .unused = 0 }
[...]
```

== Benchmarking

#v(2em)

Ninja outputs precise timestamps for every build task, out of the box!

- Converted into Chrome tracing format using https://github.com/nico/ninjatracing
- Loaded into https://ui.perfetto.dev/

== Building a decently-sized application

```
$ west build --pristine [...]
```

#image(
  "python_pristine.png",
  height: 80%,
  alt: "Profiler timeline of zephyr build tasks. Only the first quarter or so of build time is spent actually compiling source code with massive parallelism. The rest of the time is spent almost entirely single-threaded, with two invocations of gen_kobject_list.py taking up the vast majority of the time.",
)

== Even worse: incremental build

```
$ touch src/main.c
$ west build
```

#image(
  "python_incremental.png",
  alt: "Similar profiler timeline, but this time there is no significant compilation, the entire time is spent on the same single-threaded tasks as before",
)

36s build time, 31s spent waiting on `gen_kobject_list.py`!

= This is extremely slow!\ Let's rewrite it in Rust.

== The rewrite

#v(2em)

#repo-url

- approx. 1500 lines of code
- mostly ported 1:1, line-by-line
  - some small idiomatic changes
  - more library use for DWARF debug info
- 100% drop-in replacement for python script
  - automatically detected by upstream build system

== Results

```
$ cargo install gen_kobject_list
$ west build --pristine [...]
$ touch src/main.c
$ time west build
[...]
Executed in    5.21 secs
   usr time    4.54 secs
   sys time    1.25 secs
```

Finally, back to useful iteration times!

== ...before

#old-results

== Benchmarks

```
$ west build --pristine [...]
```

#image(
  "rust_pristine.png",
  height: 80%,
  alt: "Profiler timeline, 80% to 90% of time is spent on massively parallel compilation, with only a little bit of single-threaded work at the beginning and end",
)

#pagebreak()

```
$ touch src/main.c
$ west build
```

#image(
  "rust_incremental.png",
  alt: "Profiler timeline of an incremental build, resulting in only the single-threaded work. Several different tasks are discernible, with no huge outliers.",
)

#pagebreak()

```
$ touch src/main.c
$ west build
```

#image(
  "rust_incremental_annotated.png",
  alt: "The same profiler timeline, but with the two gen_kobject_list invocations hightlighted. They are only a tiny part of the timeline.",
)

Only 200ms spent blocked on `gen_kobject_list`!

== Tests

#v(2em)

- `test.sh` script in repository allows comparing Rust and python
  implementations
- Except for some small formatting differences, outputs should be bit-for-bit
  identical!
- Also useful for more precise performance comparison:

#figure(
  {
    cetz.canvas({
      chart.barchart(
        (
          ([python], 17.881),
          ([Rust], 0.234),
        ),
        size: (10, auto),
        x-label: "seconds",
      )
    })
  },
  alt: "Bar chart comparing python and Rust run times. Python spans the whole length at almost 18 seconds, while Rust is a barely visible sliver at 0.2 seconds.",
)

== The end

#v(2em)

If you are annoyed by long `CONFIG_USERSPACE=y` builds, give it a try!

#align(center, repo-url)

#v(3em)

#align(center, text(size: 1.4em)[*Questions?*])

#place(bottom + left)[
  Armin Brauns \
  #link("mailto:armin.brauns@embedded-solutions.at")
]

#place(bottom + right, dy: -0.8cm, text(size: 0.8em)[
  Presented using typst + diatypst
])
