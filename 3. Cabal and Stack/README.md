# Cabal and Stack

Both Cabal and Stack are build tools for Haskell. They are used to build, test, and run Haskell projects. They are similar in many ways, but they have some differences. Cabal is the older of the two, and Stack was created to address some of the shortcomings of Cabal.

They both help manage dependencies, build projects, and run tests. They both have a command line interface that you can use to interact with your project. They both have a configuration file that you can use to specify the dependencies and build options for your project.

## Cabal

It is a package ecosystem for Haskell. It is used to build, test, and run Haskell projects. It is similar to `npm` in the JavaScript world. It has a command line interface that you can use to interact with your project. It has a configuration file that you can use to specify the dependencies and build options for your project.

### Start a new project

To start a new project with Cabal, you can use the `cabal init` command. This will create a new project directory with a `cabal` file that you can use to specify the dependencies and build options for your project.

Let's run the following command to generate a new sample project:

```bash
cabal init cabal-sample-project --interactive --license=NONE
```

This will create a new project directory called `cabal-sample-project` with a `cabal` file that you can use to specify the dependencies and build options for your project.

The interactive flag will prompt you for some information about your project, such as the name, version, and author. The license flag will specify the license for your project. You can safely use the default values for each of these prompts.

The structure of the project will look like this:

```
cabal-sample-project/
  cabal-sample-project.cabal
  CHANGELOG.md
  app/
    Main.hs
```

The `cabal-sample-project.cabal` file is the configuration file for your project. It specifies the dependencies and build options for your project. 

The `CHANGELOG.md` file is a file that you can use to keep track of the changes that you make to your project. 

The `app` directory is where you can put the source code for your project.
This directory contains a `Main.hs` file that is the entry point for your project.

The `Main.hs` file contains the following code:

```haskell
module Main where

main :: IO ()
main = putStrLn "Hello, Haskell!"
```

This is a simple Haskell program that prints "Hello, Haskell!" to the console.


### Build the project

Once we have our project set up, we can build it using the `cabal build` command. This will compile the source code in the `app` directory and produce an executable that we can run.

Let's run the following command to build the project:

```bash
cabal build
```
This command will compile the source code in the `app` directory and produce a distributable directory with the executable and many other files.

### Run the project

Once we have built our project, we can run it using the `cabal run` command. This will run the executable that was produced by the build command.

Let's run the following command to run the project:

```bash
cabal run
```
The output will be:

```
Hello, Haskell!
```
If we make changes to our source code, cabal will automatically recompile the project when we run the `cabal run` command.

Let's make a change to the `Main.hs` file:

```haskell
module Main where

main :: IO ()
main = putStrLn "Hello, World!"
```

Now, if we run the `cabal run` command again, we will see the following output:

```
Hello, World!
```

### Add dependencies

Most of the projects use code from other libraries. To add a dependency to your project, you can edit the `cabal` file and add the dependency to the `build-depends` section.

Let's add the `HaskellSay` package as a dependency to our project. To do this, we need to add the following line to the `build-depends` section of the `cabal` file:

```haskell
build-depends:       base >=4.7 && <5
                   , haskell-say
```

Now we will change the `Main.hs` file to use the `HaskellSay` package:

```haskell
module Main where

import HaskellSay (haskellSay)

main :: IO ()
main = putStrLn $ haskellSay "Hello, World!"
```

Now, if we run the `cabal run` command, we should get a nice output from the `haskellsay` package. 

**Note:** If you see and error marked in VS Code that says `Could not find module ‘HaskellSay’`, it is because the HLS (Haskell Language Server) only checks the packages in the cabal file that is in the root of the directory that you have opened in VS Code. To fix this, you can open the `cabal-sample-project` directory in VS Code and reload the window.

## Stack

Stack is a building tool made on top of Cabal. It used to cover for most of t Cabal deficiencies. It focuses on the idea of snapshots, which are a set of packages that are known to work well together. It also has a feature called `stack.yaml` file, which is used to specify the resolver and other build options for your project. Another aspect is that it uses a specific version of GHC for each project, which is specified in the `stack.yaml` file.

We will show how to use Stack with an example in this section, but will use only cabal for the rest of the examples.

### Start a new project

To start a new project with Stack, you can use the `stack new` command. This will create a new project directory with a `stack.yaml` file that you can use to specify the resolver and other build options for your project.

Let's run the following command to generate a new sample project:

```bash
stack new stack-sample-project --snapshot lts-21.25
```

We use the snapshot `lts-21.25` in this example. The snapshot defines the versions of the tools and the templates for the projects that are going to be used. This snapshot in paticular hast a stable version of ghc that works well with lhs. 

The structure of the project will look like this:

```
stack-sample-project/
  app/
    Main.hs
  src/
    Lib.hs
  test/
    Spec.hs
  stack.yaml
  stack.yaml.lock
  CHANGELOG.md
  LICENSE
  README.md
  stack-sample-project.cabal
  .gitignore
```

This is a more elaborate strcuture for a project. 

In this case the project is divided in three main directories: `app`, `src`, and `test`. Thet contain the code for the project, the source code for the library, and the tests for the project, respectively.

If you review the cabal file, you'll find the 3 sections that correspond to the 3 directories mentioned above. The library section is the one that corresponds to the `src` directory, the executable section is the one that corresponds to the `app` directory, and the test-suite section is the one that corresponds to the `test` directory.

You can see how the app and the library interact in the `Main.hs` file:

```haskell
module Main (main) where

import Lib

main :: IO ()
main = someFunc
```

And the `Lib.hs` file:

```haskell
module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"
```
The relationshipt is also present in the cabal file in the `build-depends` section of the `executable`.



### Build the project

To build the stack project, you can use the `stack build` command. This will compile the source code in the `app` directory and produce an executable that you can run.

Let's run the following command to build the project:

```bash
stack build
```

If the version of GHC that you are using is not the same as the one specified in the `stack.yaml` file, Stack will download the correct version of GHC and use it to build the project.

### Run the project

Once we have built our project, we can run it using the `stack run` command. This will run the executable that was produced by the build command.

Let's run the following command to run the project:

```bash
stack run
```

We can also run the tests for the project using the `stack test` command. This will run the tests that are defined in the `test` directory.

Let's run the following command to run the tests for the project:

```bash
stack test
```

### Add dependencies

To add a dependency to the project we need to add it to the `package.yaml` file. This file is used to specify the dependencies and build options for your project.

Let's add the `HaskellSay` package as a dependency to our project. To do this, we need to add the following line to the `dependencies` section of the `package.yaml` file:

```yaml
dependencies:
- base >= 4.7 && < 5
- haskell-say
```

Now we will change the `Lib.hs` file to use the `HaskellSay` package:

```haskell
module Lib
    ( someFunc
    ) where

import HaskellSay (haskellSay)

someFunc :: IO ()
someFunc = haskellSay "someFunc"
```

Now if we build we will get the following error: `Could not find module ‘HaskellSay’`. This is because we need to add the package to the `extra-deps` section of the `stack.yaml` file.

Since Stack works with snapshots, the package might be in the list or it might not. In the case that it is not, we need to add it to the `extra-deps` section of the `stack.yaml` file.

Let's add the `HaskellSay` package to the `extra-deps` section of the `stack.yaml` file:

```yaml
extra-deps:
- haskell-say-1.0.0.0
```

Now if we build the project again, it should work.
