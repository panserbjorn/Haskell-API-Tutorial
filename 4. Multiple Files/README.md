# Multiple Files

When programming in any language it is common to use multiples files, this can come in the form of scripts, packages, libraries and other forms. In Haskell, we can create multiple files to organize our code and make it more readable and maintainable.

We've already seen a little bit of this in the section in which we added dependencies to a proyect (both in cabal and in stack). Now we will see how to structure our code in modules and how importing works in local files and in external libraries.

## Modules

In Haskell, a module is a collection of related functions, types, and typeclasses. Modules are used to organize code and avoid naming conflicts. A module can be defined in a single file or spread across multiple files.

A module is defined using the `module` keyword followed by the module name. The module name should start with a capital letter and match the file name. For example, if we have a module named `MyModule`, the file should be named `MyModule.hs`.

Here is an example of a module definition:

```haskell
module MyModule where

-- example function in the module
myFunction :: Int -> Int
myFunction x = x + 1

-- example type in the module
data MyType = MyInt Int | MyString String
```

This defines a module named `MyModule`. The module name should match the file name, so the file should be named `MyModule.hs`.

## `import`

Once we have defined a module, we can import it into another module using the `import` keyword. This allows us to use the functions, types, and typeclasses defined in the imported module.

Let's work in the example directory in this section that has a `Main.hs` file and a `MyModule.hs` file. The `Main.hs` file contains the following code:

```haskell
module Main where

main :: IO ()
main = do
  print 5
```

Let's add the function that is in the `MyModule.hs` file to the `Main.hs` file. First, we need to import the `MyModule` module into the `Main` module. In the end we will have the following code in the `Main.hs` file:

```haskell
module Main where

import MyModule

main :: IO ()
main = do
  print $ myFunction 5
```

Now we can use a command like `runhaskell Main.hs` to run the code in the `Main.hs` file. This will print `6` to the console.
However this only works for top level files in the same directory. 

If we want to import a module in a different directory we can construct the path in the import, but this afffects the name of the module. For example, if we have a module in a directory called `Inner` we can import it in the `Main.hs` file like this:

```haskell
import Inner.MyModule2
```

But in other to accomplish this, the name of the module inside the `MyModule2.hs` file should be `Inner.MyModule2`.

Another way is to tell ghc where to find the module using the `-i` flag. For example, if we have a module in a directory called `Inner` we can import it in the `Main.hs` file like this:

```haskell
import MyModule2
```

And run the following command:

```bash
runhaskell --ghc-arg="-i ./inner" Main.hs
```

This will tell ghc to look for the module in the `inner` directory.

Finally, we can choose what to import from a module when we import it. We can import all the functions, types, and typeclasses, or we can import only a subset of them. 

For example we can only import the `myFunction` function from the `MyModule` module like this:

```haskell
import MyModule (myFunction)
```

In that way we can avoid naming conflicts and only import the functions that we need.

## Exposing

When defining a module, we can choose which functions, types, and typeclasses to expose to the outside world. This is done using the `module` keyword followed by the module name and a list of functions, types, and typeclasses to expose.

For example, if we had a module that contained 3 functionas and 2 types, but we only wanted to expose 2 functions and 1 type, we could do the following:

```haskell
module Exposing
    ( myFunction1
    , myFunction2
    , MyType1
    ) where

myFunction1 :: Int -> Int
myFunction1 x = x + 1

myFunction2 :: Int -> Int
myFunction2 x = myFunction3 x - 1

myFunction3 :: Int -> Int
myFunction3 x = x + 3

data MyType1 = MyInt1 Int | MyString1 String

data MyType2 = MyInt2 Int | MyString2 String
```

This is useful when we want to hide the complexity of the module and only provide a subset with which the user can interact.

You can note how we use myFunction3 in myFunction2 but we don't expose it in the module.

## Hiding and Qualified

In some cases we might have conflicting names in the modules that we are importing. In this case we can qualify the import to avoid naming conflicts or we can explicitly hide the conflicting names.

For example, if we have two modules that contain a function with the same name, we can import them like this:

```haskell
import MyModule1 (myFunction) as M1
import MyModule2 (myFunction) as M2
```

Then we can use the functions like this:

```haskell
M1.myFunction 5
M2.myFunction 5
```

This way we avoid naming conflicts.

Another way to avoid naming conflicts is to hide the conflicting names. We can do this using the `hiding` keyword. For example, if we have two modules that contain a function with the same name, we can import them like this:

```haskell
import MyModule1 hiding (myFunction)
import MyModule2 
```

In this way we can use the function from the `MyModule2` module without conflicts.

## Modules in Cabal

There are no clear lines on how modules should be structured in a Haskell project and there are a lot of opinions on how to do it. 

I'll explore here 2 ways in which we can structure a project with multiple files using Cabal.

### Executable with extra modules (simpler?)

In this case we have an executable and extra modules that are part of the package. This is a simpler way to structure a project and it is useful when we have a small project that doesn't need a library.

We will have the following structure:

```
my-project/
  app/
    Main.hs
    ExtraModule.hs
  src/
    MyModule.hs
  my-project.cabal
```

In this case, the `Main.hs` file is the entry point of the executable and both the `ExtraModule.hs` and `MyModule.hs` files are extra modules that are part of the package.

They are located in different directories, but that is only to show how flexible Cabal is. We could have all the files in the same directory if we wanted.

There are 2 sections inside the `.cabal` file that are needed to add the modules to the executable. The first one is the `other-modules` sectiona that defines which other files need to be included with the executable. The second one is the `source-dirs` section that defines the directories where the files are located (we only need this because one of the modules in a different directory).

Here is an example of a `.cabal` file that includes the `ExtraModule.hs` and `MyModule.hs` files with the executable:

```cabal
...
executable my-executable
  main-is: Main.hs
  other-modules:
      ExtraModule
      MyModule
  source-dirs:
      app
      src
...
```

We can add as many modules as we want to the `other-modules` section and as many directories as we want to the `source-dirs` section.

However for larger projects that have multiple modules, it is better to use a library and an executable that uses the library since the library can define the exposed modules the executable can use.


### Library and executable (most common)

When starting a new project with cabal we can choose to create a library and an executable that uses the library. This is the most common way to structure a project. 

The structure of the project will be like this:

```
my-project/
  app/
    Main.hs
  src/
    MyModule.hs
    Module2.hs
  my-project.cabal
```

The structure is basically the same, but what changes is the `.cabal` file. In this case we will have a `library` section that defines the library and an `executable` section that defines the executable that uses the library.

Here is an example of a `.cabal` file that includes the `MyModule.hs` and `Module2.hs` files with the library and the `Main.hs` file with the executable:

```cabal
name: my-project
...
library
  exposed-modules:
      MyModule
      Module2
  source-dirs:
      src

executable my-executable
    main-is: Main.hs
    source-dirs:
        app
    build-depends:
        base,
        my-project
```

Note how the `exposed-modules` section in the `library` section defines which modules are exposed by the library. This is important because the executable can only use the modules that are exposed by the library.
Also note how the `build-depends` section in the `executable` section defines the dependencies of the executable. In this case, the executable depends on the library that we defined.

Inside the library we can arrange the modeuls in whichever way we want, with or without directories. The important thing is that the modules that we want to expose are in the `exposed-modules` section of the library.

