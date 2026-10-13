## Compiling Java files

To build the Java class files (for the sqlite bindings) run:

```
clojure -T:build jcompile
```

## Building SQLite binaries

Install `zig`. Ensure the SQLite amalgamation files you want to build are in `sqlite-amalgamation` then run:

```
bash cross-compile.sh
```

New binaries will be under `resources`.

The latest SQLite amalgamations can be found [here](https://sqlite.org/download.html).
