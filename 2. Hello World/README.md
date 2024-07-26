# Hello World

Let's start with a simple "Hello World" program.

However before we write our first program, let's understand how to execute Haskell code.

There are 3 ways in which one can execute Haskell code:

1. Running Haskell code in the interactive mode using `ghci`.
2. Running Haskell code using `runhaskell`.
3. Compiling Haskell code using `ghc`.

## Running Haskell code in the interactive mode using `ghci`

GHC provides an interactive mode called `ghci` which stands for GHC Interactive.

In the most basic form, you can run `ghci` in the terminal and it will open an interactive shell where you can write Haskell code and see the output.

```bash
ghci
```

Examples of code that can be run in `ghci`:

```haskell
odd 3
```

```haskell
5 + 3
```

```haskell
take 5 [1..]
```

However another usage of `ghci` is to load a Haskell file and run the code in the file.

```bash
ghci test.hs
```

Example of code that can be run in `ghci` after loading a file:

```haskell
main
```

```haskell
func 3
```

```haskell
func2 3
```

Both `func` and `func2` are functions defined in the file `test.hs`. The `main` function is a special function that is the entry point of the program when compiled.

## Running Haskell code using `runhaskell`

`runhaskell` is a command line tool that can be used to run Haskell scripts.

```bash
runhaskell test.hs
```

This will execute the `main` function in the file `test.hs`.

## Compiling Haskell code using `ghc`

`ghc` is the Haskell compiler that can be used to compile Haskell code.

```bash
ghc test.hs
```

This will create 3 files:

1. `test.hi` - Intermediate file
2. `test.o` - Object file
3. `test` - Executable file

The only file we need to concern ourselves with is the executable file `test`. The other files are created by the compiler to help in subsequent compilations steps. The `.hi` file contains the type information of the functions in the file and the `.o` file contains the object code. Both are binary files and are used by the complier in case you ask it to compile another file that depends on the functions in the `test.hs` file.

To run the executable file:

```bash
./test
```

## Usage while programming

While programming, it is common to use `ghci` to test small snippets of code and then use `ghc` to compile the final program.

`runhaskell` is used to run scripts that are not meant to be compiled and is a quick way to run Haskell code.

## Detecting errors

`ghci` is a great tool to detect errors in your code. It will give you a detailed error message that will help you understand what went wrong.

Because of that it very common to use `ghci` regularly while writing Haskell code.
