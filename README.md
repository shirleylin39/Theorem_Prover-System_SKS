
# System-SKS

This is a Haskell project that uses [Cabal](https://www.haskell.org/cabal/) as the build system and package manager. Below are the steps to set up and run the project on your local machine.

## Prerequisites

Make sure you have the following installed:

- [Haskell Platform](https://www.haskell.org/platform/)
- [Cabal](https://www.haskell.org/cabal/)
- [GHC](https://www.haskell.org/ghc/) (Glasgow Haskell Compiler)

You can verify the installation of Cabal and GHC by running:

```bash
cabal --version
ghc --version
```

## Getting Started


### Install dependencies:

Run the following command to install the necessary dependencies specified in the `.cabal` file:

```bash
cabal update
cabal install
```

### Build the project:

To compile the project, use:

```bash
cabal build
```

### Run the project:

To run the project, use:

```bash
cabal run
```

### Testing:

If you have a test suite set up, you can run it with:

```bash
cabal test
```

## Project Structure

- `src/`: Contains the Haskell source code.
- `test/`: Contains the unit test file.
- `benchmark/`: Contains the benchmark file and previous raw data.
- `dist-newstyle/`: Output directory for compiled files.
- `System-SKS.cabal`: The Cabal configuration file, which defines the package, dependencies, and other settings.

## Cleaning Up

To clean up the build artifacts:

```bash
cabal clean
```

## Further Information

For more detailed documentation on using Cabal, visit the [official Cabal documentation](https://www.haskell.org/cabal/users-guide/).
