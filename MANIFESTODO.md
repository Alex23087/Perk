# THE PERK MANIFESTO

This file contains the features we want to add to perk.


# Perk v1.0

## Structs and Models

Structs and models will be *almost the same thing*, but:

- Structs cannot be recursive
- Structs can be both on the heap and on the stack, whereas models can only be on the heap
- Structs, models, ADTs **and archetypes** will have a **type parameter**

## Open shall unsuck 👍

Open should not import the whole file
- Compiling a perk file generates a .h file
  - (need to add access modifiers like `export`, `fileprivate`, ...)
- `open`ing a perk file only imports exported names for the typechecker
- Separate compilation of the output files using the target C compiler

## C compilation step should be performed by perkc

- C compiler path can be passed as option (gcc as default)
- Add option to generate C file without compiling it

## ADT && Matching && switch (if not match)

```perk
type Point = Point(x: int = 0, y: int = 0)

type List<T> = Empty | Cons(T, List<T>)

fun main(): void {
    let l := Cons(10, Cons(20, Empty));

    match (l) {
        case Empty {

        };
        case Cons(x, _) {

        }
        other/_/default/else {

        }
    }
}
```

```C
typedef struct ListT {
    Tag tag;
    union Data {
        struct Empty {} empty;
        struct Cons {
            T contents;
            struct ListT* rest;
        } cons;
    } data;
}
```

### Implementation of pattern matching???



### Interplay between ADTs and Models !??!?!!?



# Perk v>1


## Functions can have optional parameters with default values and named arguments
Functions can have optional parameters with default values and named arguments 👍

## Computed Properties

```perk
model Test {
    let x : int =
        get {
            return 10
        }
        set (new_value: int) {
            x = new_value + 10
        }
}
```


## Extension methods
Extension methods 👍

## Package manager


# Perk ∞

- Invariant checking with tapes

- Monadic session types with capabilities and effect handlers for secure parallelism and safety hyperproperty checking 👍

- Expect spanish inquisition in `parser.mly`

- Resolve parser conflicts

- Remove Herobrine


```
⠀⠀⠀⢀⣄⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⡀⠀⠀⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⢹⠉⠛⠛⠲⣦⣄⣀⠀⠀⡀⠀⠄⢀⠠⢀⠀⠄⡀⠠⢀⠀⠄⡀⠠⢀⠀⠄⡀⠠⢀⠀⡀⣯⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⢸⠷⢶⣤⣀⡀⠊⠙⠛⢶⣦⣌⣀⠂⠐⠠⢈⠐⠠⢁⠂⠌⡐⠠⢁⠂⠌⡐⠠⢁⠂⡐⠀⣿⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⢸⠀⠀⣿⠉⠙⠻⢦⣤⣼⡇⠉⠉⠻⠶⣤⣄⡈⠄⠂⢈⠐⠠⢁⠂⠌⡐⠠⢁⠂⡐⠠⠁⣿⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⠀⠀⠀⠀
⠀⠀⠀⢸⠀⠄⣿⠀⠂⠄⢸⡇⠉⠛⠛⣶⣤⣄⣀⠈⠛⠛⠶⣦⣐⣀⣄⣀⣂⡀⠁⢂⠐⠠⢁⠂⣹⠀⠀⠀⠀⠀⠀⢀⠀⠀⣀⣠⣠⣶⠀⠀⠈⠀⠀⠀⠀
⠀⠀⠀⢸⠀⠀⣿⠀⠡⢈⢰⡇⠐⠠⠈⣿⠀⠈⠹⣿⠶⣦⣴⣾⣿⣿⣿⣿⣿⣿⣶⣤⡈⠐⢀⠂⢸⠀⣠⣤⣤⣶⣿⣿⣿⣿⣿⣿⣿⣿⣦⠀⠀⠈⠀⠀⠀
⠀⠀⠀⢸⠀⠃⣿⠀⠃⠄⢸⡇⠀⢃⠀⣿⠀⢃⠀⣟⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣧⣤⣼⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿O⣿⣇⠀⠀⠠⠀⠀
⠀⠀⠀⢸⠀⠂⣿⠀⡁⢂⠘⣧⠈⠄⠂⣿⠀⢂⠀⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣆⠀⠀⠀⡀
⠀⠀⠀⢸⠀⠄⣿⡃⠐⠠⠘⣿⠀⢂⠀⣿⠂⠄⠂⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿OKAPI MODE⣿⣿⣿⣿⣿⣿⣿⠿⠋⠉⠙⢿⣿⣿⣿⡆⠀⠀⠁
⠀⠀⠀⢸⠀⡀⢻⡅⠈⠄⡁⣿⠀⠂⠄⢿⡐⠈⠄⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠓⠦⣤⣀⠀⠀⠈⠻⠿⠁⠀⠀⠄
⠀⠀⠀⢸⠀⠠⢹⡇⢈⠐⠀⣿⠀⡁⠂⢸⠆⡁⠀⣿⣿⣿⣿⣿⣿⣿⣿⣿⢿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠟⡗⠢⢤⣀⡹⡇⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⢸⠀⠂⢸⡇⠠⠈⠄⣿⠀⡐⠀⢸⡃⠠⠁⣿⣹⣿⣿⣿⣿⣿⣿⣿⠀⢻⡿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡿⡏⠀⡧⠀⢸⡏⠉⡇⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⢸⠶⢾⣼⣇⣀⣁⣀⣿⢀⠠⠁⢸⡅⠐⡀⢿⣽⣿⣿⣿⢿⣿⣿⡏⠀⠠⡗⠀⢸⡇⠀⢻⡇⡟⣿⣿⣿⠃⡇⠀⣷⠀⠀⡇⠀⢹⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⢸⣤⣤⣄⣈⣉⠉⠙⠛⠳⠶⠶⢾⣤⣔⣀⣹⡟⢿⣿⣿⠿⣿⣿⡇⠀⠒⣏⠐⢸⡇⠐⢸⡇⣇⣿⣿⡿⠀⣇⠀⢹⠀⠀⡇⠀⢸⡄⠀⠀⠀⠀⠀⠀
⠀⠀⠀⢸⠀⠀⠉⠉⠉⠛⠛⠛⠷⠶⣦⣤⣤⣌⣉⣙⠙⠛⠻⠿⢦⣿⣿⣧⣐⡀⣏⠠⢸⡇⠐⢸⡇⡏⢿⣿⠁⠀⢹⠀⢸⠀⠀⣧⠀⠈⡇⠀⠀⠀⠀⠀⠀
⠀⠀⠀⢸⣦⣬⣀⣁⣂⣠⠀⠄⠠⠀⢀⠀⠀⠈⠉⠙⠛⠛⠲⠶⠶⢦⣤⣬⣉⣛⣛⠓⠾⠷⣮⣼⣇⣇⣸⣿⠀⠀⢸⠀⢸⡀⠀⣽⠀⠀⢿⠀⠀⠀⠀⠀⠀
⠀⠀⠀⢸⢎⡽⣩⢛⡭⣛⠿⣻⢷⡶⣶⢦⣥⣬⣴⣀⣠⡐⠀⠠⠀⡀⢀⠈⠉⠉⠉⠛⠛⠷⠶⠦⢭⣭⣭⣛⣛⠿⠾⠶⣬⣧⣄⣾⣀⡀⢸⠀⠀⠀⡀⠀⠀
⠀⠀⠀⢸⢎⠶⣡⢏⠶⣩⠞⡱⢎⠶⣡⢏⡼⣡⢏⡻⣝⢻⠻⡟⡟⣷⢶⣶⣦⣤⣤⣆⣐⣀⣠⠀⠀⠀⡀⠉⠉⠋⠛⠛⠳⠶⠿⣭⣿⣹⣻⣇⠀⠀⠐⠀⠀
⠀⠀⠀⢸⢎⡳⡱⢎⡳⢥⢫⡕⣫⠞⡱⢎⠶⡱⢎⡵⣊⢧⡛⡼⡱⢎⡞⡴⢣⡝⡲⣍⠻⣝⢻⠻⡿⢶⢶⡶⢷⣦⣤⣤⣀⣀⣀⣀⠀⠀⠉⢹⡇⠀⠀⠀⠀
⠀⠀⠀⢸⣷⣷⣿⣾⣵⣯⣶⣽⣦⣯⣵⣏⣞⣱⣫⣴⡩⢖⡹⢲⡙⢮⡜⣜⢣⢞⡱⢎⡳⣌⢧⡛⣜⢣⠞⣜⢣⢞⡱⢫⡝⣫⣛⣽⣿⠿⠷⠾⠇⠀⠀⠀⠀
⠀⠀⠀⢸⣟⡾⣵⣿⣾⣷⣯⣿⣹⢯⡟⣿⣻⢟⣿⣻⢿⡿⣿⢿⡿⣿⣻⢿⣿⣾⣷⣿⣶⣭⣶⣽⣬⣧⣟⣬⣷⠾⠷⠟⠛⠉⠉⠁⠀⠀⠀⠀⠀⠀⠐⠁⠀
⠀⠀⠀⢸⣟⡾⣿⠀⠀⠀⢸⣷⣯⣿⣾⣳⣯⣿⣮⣷⣻⣾⣷⣯⣷⣯⣽⣯⣾⣧⣿⣾⣽⣯⣿⣹⢯⣟⡾⣿⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⢸⣿⣼⣿⠀⠀⠀⢸⡇⠀⠀⢸⡇⠀⠀⢸⣿⠋⠁⠀⠀⠀⠙⣿⣿⠀⠀⠘⠋⠀⠀⢹⣿⣯⣾⣽⣿⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⢸⣷⣫⣿⠀⠀⠀⢸⡇⠀⠀⢸⡇⠀⠀⢸⣯⠀⠀⠉⠉⠻⠶⣤⣄⡈⠄⠂⢈⠐⠠⢁⠂⠌⡐⠠⢁⠂⡐⠠⠁⣿⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⠀⠀⠀⠀
⠀⠀⠀⢸⠀⠄⣿⠀⠂⠄⢸⡇⠉⠛⠛⣶⣤⣄⣀⠈⠛⠛⠶⣦⣐⣀⣄⣀⣂⡀⠁⢂⠐⠠⢁⠂⣹⠀⠀⠀⠀⠀⠀⢀⠀⠀⣀⣠⣠⣶⠀⠀⠈⠀⠀⠀⠀
⠀⠀⠀⢸⠀⠀⣿⠀⠡⢈⢰⡇⠐⠠⠈⣿⠀⠈⠹⣿⠶⣦⣴⣾⣿⣿⣿⣿⣿⣿⣶⣤⡈⠐⢀⠂⢸⠀⣠⣤⣤⣶⣿⣿⣿⣿⣿⣿⣿⣿⣦⠀⠀⠈⠀⠀⠀
⠀⠀⠀⢸⠀⠃⣿⠀⠃⠄⢸⡇⠀⢃⠀⣿⠀⢃⠀⣟⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣧⣤⣼⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿X⣿⣇⠀⠀⠠⠀⠀
⠀⠀⠀⢸⠀⠂⣿⠀⡁⢂⠘⣧⠈⠄⠂⣿⠀⢂⠀⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣆⠀⠀⠀⡀
⠀⠀⠀⢸⠀⠄⣿⡃⠐⠠⠘⣿⠀⢂⠀⣿⠂⠄⠂⣿⣿BREW BETTER CODE⣿⣿⣿⣿⣿A⣿⣿⣿⣿⣿⠿⠋⠉⠙⢿⣿⣿⣿⡆⠀⠀⠁
⠀⠀⠀⢸⠀⡀⢻⡅⠈⠄⡁⣿⠀⠂⠄⢿⡐⠈⠄⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠓⠦⣤⣀⠀⠀⠈⠻⠿⠁⠀⠀⠄
⠀⠀⠀⢸⠀⠠⢹⡇⢈⠐⠀⣿⠀⡁⠂⢸⠆⡁⠀⣿⣿⣿⣿⣿⣿⣿⣿⣿⢿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠟⡗⠢⢤⣀⡹⡇⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⢸⠀⠂⢸⡇⠠⠈⠄⣿⠀⡐⠀⢸⡃⠠⠁⣿⣹⣿⣿⣿⣿⣿⣿⣿⠀⢻⡿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡿⡏⠀⡧⠀⢸⡏⠉⡇⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⢸⠶⢾⣼⣇⣀⣁⣀⣿⢀⠠⠁⢸⡅⠐⡀⢿⣽⣿⣿⣿⢿⣿⣿⡏⠀⠠⡗⠀⢸⡇⠀⢻⡇⡟⣿⣿⣿⠃⡇⠀⣷⠀⠀⡇⠀⢹⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⢸⣤⣤⣄⣈⣉⠉⠙⠛⠳⠶⠶⢾⣤⣔⣀⣹⡟⢿⣿⣿⠿⣿⣿⡇⠀⠒⣏⠐⢸⡇⠐⢸⡇⣇⣿⣿⡿⠀⣇⠀⢹⠀⠀⡇⠀⢸⡄⠀⠀⠀⠀⠀⠀
⠀⠀⠀⢸⠀⠀⠉⠉⠉⠛⠛⠛⠷⠶⣦⣤⣤⣌⣉⣙⠙⠛⠻⠿⢦⣿⣿⣧⣐⡀⣏⠠⢸⡇⠐⢸⡇⡏⢿⣿⠁⠀⢹⠀⢸⠀⠀⣧⠀⠈⡇⠀⠀⠀⠀⠀⠀
⠀⠀⠀⢸⣦⣬⣀⣁⣂⣠⠀⠄⠠⠀⢀⠀⠀⠈⠉⠙⠛⠛⠲⠶⠶⢦⣤⣬⣉⣛⣛⠓⠾⠷⣮⣼⣇⣇⣸⣿⠀⠀⢸⠀⢸⡀⠀⣽⠀⠀⢿⠀⠀⠀⠀⠀⠀
⠀⠀⠀⢸⢎⡽⣩⢛⡭⣛⠿⣻⢷⡶⣶⢦⣥⣬⣴⣀⣠⡐⠀⠠⠀⡀⢀⠈⠉⠉⠉⠛⠛⠷⠶⠦⢭⣭⣭⣛⣛⠿⠾⠶⣬⣧⣄⣾⣀⡀⢸⠀⠀⠀⡀⠀⠀
⠀⠀⠀⢸⢎⠶⣡⢏⠶⣩⠞⡱⢎⠶⣡⢏⡼⣡⢏⡻⣝⢻⠻⡟⡟⣷⢶⣶⣦⣤⣤⣆⣐⣀⣠⠀⠀⠀⡀⠉⠉⠋⠛⠛⠳⠶⠿⣭⣿⣹⣻⣇⠀⠀⠐⠀⠀
⠀⠀⠀⢸⢎⡳⡱⢎⡳⢥⢫⡕⣫⠞⡱⢎⠶⡱⢎⡵⣊⢧⡛⡼⡱⢎⡞⡴⢣⡝⡲⣍⠻⣝⢻⠻⡿⢶⢶⡶⢷⣦⣤⣤⣀⣀⣀⣀⠀⠀⠉⢹⡇⠀⠀⠀⠀
⠀⠀⠀⢸⣷⣷⣿⣾⣵⣯⣶⣽⣦⣯⣵⣏⣞⣱⣫⣴⡩⢖⡹⢲⡙⢮⡜⣜⢣⢞⡱⢎⡳⣌⢧⢰⣦⠀⠀⢸⣯⠀⠀⠀⣆⠀⠀⠀⣿⣾⣧⣟⣿⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⢸⡷⣽⣿⠀⠀⠀⢸⡇⠀⠀⢸⡇⠀⠀⢸⣿⣿⡾⠿⠃⠀⠀⢨⣟⠀⠀⠀⣷⠀⠀⠀⣿⣿⡾⣼⣻⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⢸⡿⣵⣿⠀P⠀⢸⡇E⠀⢸⡇⠀R⢸⡏⠀K⢠⣶⠀⠀⠰⣟⠀⠀⠀⣿⡆⠀⠀⣿⣟⣿⣷⢿⣇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⢸⣿⣿⠛⠀⠀⠀⣸⡇⠀⠀⠸⠇⠀⠀⢸⡇⠀⠀⠘⠿⠀⠀⢸⣯⡀⠀⠀⣿⡇⠀⠀⣿⣿⡯⣿⣿⣿⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⢸⣿⣿⣤⣤⣤⣴⣿⣿⣶⣶⣦⣦⣶⣿⣿⣦⣤⣄⣤⣦⣤⣿⣿⣿⣿⣯⣿⣿⣿⣯⣿⣿⣿⣿⣿⡿⣿⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠠⠀⠀⠈⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠀⠀⠠⠀⠀


```