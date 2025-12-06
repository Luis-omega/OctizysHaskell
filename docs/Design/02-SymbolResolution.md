# Symbol resolution

We need to talk about the module system before we talk about the symbol
resolution approach.

## The module system

Every file is a module and one cannot create new modules inside them. The name
of the module is the path to the file from the root of the folder declared on
the project file that contains the file in it's children, with the name of the
package included as the first component.

Example:

```
File system path: /home/user/projects/PackageName/src/A/B/C.oct
Path provided to the compiler that allowed the compiler to discover it: /home/user/projects/someProjectWithoutRepo/src/
Logic path assigned: PackageName.A/B/C.oct
```

If you are in a folder not inside a repository and not declared as a project,
the package name is replaced for a default name (to be defined) and the logic
path is still the root path that the compiler used to discover the file.

```
File system path: /home/user/projects/someProjectWithoutRepo/src/D/E
Path provided to the compiler that allowed the compiler to discover it: /home/user/projects/someProjectWithoutRepo/src/
Logic path assigned: DefaultName.D.E
```

All the paths provided to the compiler are sorted lexicographically and by
length (the shortest came first), the first path that allow us to discover a
module is the one used to determine the module path.

### Imports

We only allow two kind of imports:

- Explicit import of unqualified items by the use of a keyword.
- Import of a full module but it must be qualified.

Example:

```haskell
import unqualified A.B.C(d,e,f)  -- explicit import of unqualified items.
import G.H.J as J -- explicit import of a module qualified
```

The module qualifier can be another logic path `(IDENTIFIER ".")+ IDENTIFIER`
and it can be totally unrelated to the original name that it is aliasing.

```haskell
import G.H.J as A.B
import G.H.K as H
import G.H.W as H
```

We choose this rules since we wanted to encourage a particular style of imports:

- We want to discourage the use of unqualified items, the reason is that octizys
  is not a yet know language and sometimes even for some know languages the tool
  support can be very low for some systems and places, we aim to be able to
  easily find the dependencies of a module and inspect it's code without the
  help of a lsp or a tags system if needed.

- Explicit unqualified items also make it easier to avoid the clash (shadow) of
  names. Admittedly the aliasing of modules reintroduce them, but in a less
  sever degree.

- We allow logic path aliases to be any logic path instead of restricting them
  to a prefix or a component of the original logic path, since the developers
  can have good reasons to group several modules under a single descriptive
  name.

### Exports

Instead of a list of exports like in haskell, or allowing all to be exported as
in python, we follow for exports the same model as in rust, every definable
variable, type or class can be public or private. The default value is private.

Additionally we don't support the re-export of items inside modules, since it
usually makes it difficult to determine the original definition of a identifier
even with the help of tools (and for tool implementers is a pain to maintain
them).

Maybe in the future we would introduce files (not modules) with explicit
declaration of export of items in the same folder and it's children. But for now
this is not allowed.

# The Input

For every file (module) we expect to have available:

- A list of CST.Definition (CST.Definition is the tree resulting of parsing a
  definition in the code).
- A list of CST.TypeDefinition (CST.TypeDefinition is the tree resulting of
  parsing a type definition in the code).
- A list of type classes and instances(TODO).
- A list of Import declarations (qualified or unqualified).
- A list of exported items.

Additionally we expect to have the modules dependency tree available.

# The output

Every module has the variable names replaced with unique and fully qualified
identifiers inside it's scope. There are only 2 module level scopes, one for
types and one for expressions (types live at compilation stage, expressions does
at run time). This means that an assigned identifier can be in both scopes but
meaning a different thing.

A symbol table peer module and scope, it maps the unique identifier assigned to
a symbol to it's information, like if it is a variable or a parameter, the
source position of it, the original name (if any) and the CST of the symbol (if
not a parameter).

Another table peer module and scope that maps the original module level
variables to it's new identifier.

# Checks done by this stage

- Ensure symbols are defined before they are used(raise one error per symbol).
- Find unused symbols (a warn is emitted for every symbol).
- Find variable shadowing (a warn is emitted for every occasion).
- All variables in the module are unique after it ends the resolution and all
  have the full qualifier (logic path) of their original definition.
