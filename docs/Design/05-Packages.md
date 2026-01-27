# Octizys Package Manager and Build System Architecture

## 1. Local Project Structure

A typical Octizys project layout:

```
ProjectRoot/
  A/
    src/
    APackage.toml
  B/
    src/
    BPackage.toml
  D/
    src/
    DPackage.toml
  project.toml
  project.lock
```

- Each package folder contains:
  - `src/`: Octizys source code (\*.oct)
  - `APackage.toml`: package metadata (name, version, dependencies)
- The project root contains:
  - `project.toml`: defines which packages are part of the project and their
    version/path constraints
  - `project.lock`: lockfile fixing resolved versions/dependencies

## 2. Compilation Concept

- The compiler produces **intermediate `.ioct` files**, which are
  target-agnostic.
- Each package can have multiple build targets:
  - Python (`python-v`): generates `.py` modules
  - C (`c-v`): generates `.c`, `.o` or `.dll`
  - Future targets: WASM, LLVM IR, native binaries
- Each build depends on:
  - Compilation options
  - Compiler version
  - Package dependencies

## 3. Store and Database Architecture

The **store** is a centralized system for compiled artifacts, backed by a
**database** that tracks packages, builds, targets, and dependencies.

### 3.1 Database Schema (Conceptual)

- **Package (UUID)**

  - Name, version, source URL or local path
  - Dependencies (other Package UUIDs)
  - Builds (one-to-many)

- **Build**

  - Target name (Python, C, WASM, etc.)
  - Compiler name and version
  - Compilation options hash
  - List of `.ioct` files
  - List of native target outputs (e.g., `.py`, `.c`, `.o`, `.dll`)

- **Advantages of DB approach**

  - No ambiguity between multiple sources for same version
  - Can track compilation options and compiler version per build
  - Easier to manage concurrent access via DB locking
  - Provides unified API for queries like "where is package X compiled for
    target Y?"

### 3.2 Human-Readable Store View

- The package manager can generate a human-friendly directory view with symbolic
  links pointing to DB-stored artifacts:

```
store/
  A/
    version/
      built/
        python-v/ -> ../../UUID/build/python-v/hash/
        c-v/ -> ../../UUID/build/c-v/hash/
```

- This mirrors a filesystem hierarchy without storing duplicates of the compiled
  files.

### 3.3 Build Output Layout

```
build/
  compilerName_version/
    ioct/*.ioct          # Intermediate target-agnostic files
    python-v/*.py        # Python target outputs
    c-v/*.c, *.o, *.dll  # C target outputs
```

- `.ioct` files allow reuse across targets.
- Each target folder contains files native to that target.
- Compiler name/version is included to distinguish builds made with different
  toolchains.

## 4. Dependency Handling

- Two levels of dependencies:

  1. **Package-level**: packages depend on other packages.
  1. **Module-level**: individual modules may depend on modules from the same or
     other packages.

- The package manager provides `.ioct` inputs to the compiler so it only
  resolves paths within the project or store.

- Local project packages can be linked into the store with symbolic links for
  uniform access.

- Metadata per package tracks source location (local path or remote URL) to
  prevent conflicts.

## 5. Potential Problems and Considerations

- Multiple package sources for the same version (e.g., GitHub vs GitLab)
- Handling multiple versions of the same compiler
- Concurrency in accessing/updating the database
- Mapping `.ioct` back to source for debugging and LSP purposes
- Project-local builds versus global store builds
- Ensuring that symbolic links remain valid and cleaning up broken links

## 6. Summary of Design Decisions

1. **Intermediate `.ioct` files** are target-agnostic to allow multiple target
   outputs.
1. **DB-centric store** solves source/version ambiguity, concurrency, and
   complex dependency tracking.
1. **Human-readable symbolic structure** exposes DB contents in a way that is
   user-friendly without duplicating data.
1. **Build outputs include compiler and version info** to support multiple
   compilers and options.
1. **Two-level dependency graph** ensures accurate compilation and type
   checking.
1. **Local project packages** can be linked into the store to unify treatment
   with remote packages.
