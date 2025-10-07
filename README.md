# Perk

Perk is a modern low level programming language, compiled to C. It builds on C's simplicity, adding features such as option types, lambdas, typeclasses and algebraic data types. Its current goal is to replace C in the implementation of the [MellOS](https://github.com/mell-o-tron/MellOs) operating system.

---

## Join us!
If you want to join Perk development, discussions, or just want to see what's going on, join our [Discord server](https://discord.com/invite/DgDDV6xPQe)!
We love new people joining the community, and are always happy to help out new contributors, so don't hesitate!

---

## Table of Contents
- [Documentation](#documentation)
- [Integration with C](#integration-with-c)
- [Features](#features)
- [Running Perk](#running-perk)
    - [Nix](#nix)
    - [Arch Linux](#arch-linux)
- [VSCode Extensions](#vscode-extensions)
- **[Contributing](CONTRIBUTING.md)**

---

## Documentation

We are using `ocamldoc` to generate the documentation. Use the command `make docs` to generate the documentation.

You can view the latest documentation automatically built from this repository on [the Perk website](https://perklang.org/).

---

## Integration with C

Import C libraries using `import "libname.h"`. The compiler will automatically add most of the prototypes from the C source to the typechecker. This is not yet perfect, and does not yet work for custom include paths [(check out this issue)](https://github.com/Alex23087/Perk/issues/12), nor does it currently include macros.

---

## Features

- Simple **type inference**

```
fun main () : int {
    // No need to write the type here!
    let x := 3;
    return 0;
} 
```

- **Tuples**

```
let position: (float * float) = (200.,200.)
```

- **Option types**, with implicit boxing at assignment

```
let z : (int*)? = nothing;
z = just &x;
```

- **Typeclass system** (*Archetypes and Models*)

```
archetype Drawable {
    position : (float * float),
    rotation : float,
    texture : SDL_Texture*,
    size : (float * float),
    draw : SDL -> void
}
```

```
model Character: Drawable, PhysicsObject {
    let position: (float * float) = (200.,200.),
    let velocity: (float * float) = (0., 0.),
    ...

    fun constructor (sdl : SDL) : void {
        ...
    },

    fun draw (sdl : SDL) : void {
        ...
    }
}
```

- Parametric generic functions through archetype sums

```
let f := (x: <Drawable+PhysicsObject>) : void {...}

f(drawable_physics_thing as Drawable+PhysicsObject)
```

- Capturing anonymous functions

```
fun streq (s1 : char*) : char* => int {
    return (s2 : char*) : int {return (strcmp (s1, s2) == 0)}
}
```

- Static subset (only uses the stack, only allows non-capturing lambdas)

- Algebraic data types (ADTs) :warning: currently in `devel` :warning:

```
type IntList = | Empty | Cons (int, IntList)

fun sum (l : IntList) : int {
    let res := 0;
    match (l) {
        Empty {res = 0},
        Cons(var x : int, var l1 : IntList) {res = x + sum(l1)}
    };
    return res
}
```

---

## Running Perk
### Nix

```bash
nix run github:Alex23087/Perk -- [files]
```

This will build the `perkc` compiler and run it directly (you can pass any arguments after `--`).

### Arch Linux
[![AUR](https://img.shields.io/aur/version/perk-opam-git?style=flat)](https://aur.archlinux.org/packages/perk-opam-git)

Perk is on the AUR!\
You can install `perk-opam-git` from your favourite AUR helper.

This package pulls the latest Perk version from the main branch of this repository and builds `perkc` using `opam` and `dune`. All required dependencies are automatically installed. It then installs the compiler in `/usr/local/bin/perkc`

---

## VSCode Extensions
We have two extensions for VSCode, located in `tools/vscode-extensions/`:
- `perk-syntax`: provides syntax highlighting for Perk files;
- `perk-lsp`: provides error checking using `perkc --check`.

To install them, you should build them using `make extensions` and then install them in VSCode using the "Install from VSIX..." option.