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

### Start a new project

### Build the project

### Run the project

### Add dependencies