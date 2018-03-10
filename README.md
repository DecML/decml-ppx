# DecML PPX

This repository is an experimental implementation of the DecML language, the theoretical bases of which are laid out in https://arxiv.org/abs/1710.03984.

The key idea behind this work is to achieve a decoupling between a supervised learning model and its parameters, through the programming language itself.

This explores the potential of removing the burden of managing parameters manually from the programmer, reducing the chances of incorrectly implementing models (and getting wrong results).

## DecML PPX Extension

This DecML implementation works by translating OCaml programs that contain [extension nodes](https://caml.inria.fr/pub/docs/manual-ocaml/extn.html) to vanilla OCaml programs.

At compile time, the extension will examine the syntactic contents of a model, and separate it from its parameters - yielding a pair `(f, p)`. 

Applying `f` to `p` yields the model defined originally, with the current values of the parameters slotted in.

The extension can translate a variety of syntactic constructs, including function definitions and applications, lists (and other algebraic data types), tuples, let bindings etc.
Have a look at some of the [examples](#Examples).

## Dependencies

This version only works for OCaml version *4.05* - as syntax trees are slightly different between versions.

Library has no external dependencies.
There are a few dependencies used for the tests and example code:

- [ppx_tools](https://opam.ocaml.org/packages/ppx_tools/) (tests only)
- [oUnit](http://ounit.forge.ocamlcore.org/) (tests only)
- [csv](https://github.com/Chris00/ocaml-csv) (examples only)

## Building and installing

The package will shortly be available on the [opam](http://opam.ocaml.org/) repository.
To pin the package locally and install it from this source:

```
opam pin add decml .
```

Build process is managed using [oasis](https://github.com/ocaml/oasis). To build and install:

```
ocaml setup.ml -configure --enable-tests
ocaml setup.ml -build
ocaml setup.ml -install
```

Running tests:

```
ocaml setup.ml -test
```

Building including examples:

```
ocaml setup.ml -build -tag examples
```

## Usage

Once library is installed, you can use it in your own programs. Example:

```
ocamlfind ocamlc -package decml.ppx -package decml -linkpkg foo.ml
``` 

## Examples

The examples/ directory contains several examples of using the language in practice:

- Simple and multivariate linear regression
- Fitting a confidence interval
- Mixture of regression models, fitted with expectation-maximization (EM)
- Feedforward neural network


## Checking transformed model

You may want to check the transformation of a model. For this, you can use the `rewriter` from the [ppx_tools](https://github.com/ocaml-ppx/ppx_tools) package, e.g.:

```
ocamlfind ppx_tools/rewriter ./decml_ppx_extension.native foo.ml -o foo_transformed.ml
```
