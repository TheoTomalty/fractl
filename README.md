# CAUTION

This project is a work in progress.  The Lisp IDE is functional enough for me to
use full-time, but there are still lots of minor bugs.  DO NOT USE IN
PRODUCTION.  Some of the other components are incomplete and are not yet
functional.  So far fractl has been entirely hand-written by me, without the
use of AI.

# What is fractl?

Fractl is a programming language meta-framework.  It is intended to be a
complete programming environment, which supports many different integrated
languages.  A polyglot's dream.  Fractl is:
- A scripting language which is a Python-like syntax extension to Common Lisp
- A scannerless and incremental parser generator like tree-sitter or yacc/bison
- A text editor based on a combination of Emacs and Vim
- A POSIX-like shell
- An interpreted implementation of Common Lisp (incomplete)
- A compiler intermediate representation, in a similar role to LLVM (incomplete)
- An AArch64 assembler and linker (incomplete)
- A C-like compiled systems programming language with a Hindley-Milner type
  system (incomplete)
- A Prolog-like logic programming language (incomplete)
- A compiled implementation of Common Lisp (planned)

## Dependencies

For now there are only two dependencies for fractl: `sbcl` and a C toolchain
(`cc`, `as`, `ld`).  However, once fractl is complete, each of these
dependencies will be replaced by built-in alternatives.  Fractl is designed to
ultimately have zero dependencies.  That means the entire system should compile
to a functional IDE from a brand new clone of the repo.  In other words,
everything must be bootstrapped from a single binary executable (called the
seed), which will be checked into the repository.

The bootstrapping procedure goes as follows:
- The binary seed is executed, and this implements a simple Forth interpreter.
- The Forth interpreter is expanded in Forth itself.
- The basic interpreter for Common Lisp is implemented in Forth.
- The full Common Lisp system is expanded in Common Lisp itself.
- Common Lisp is used to implement the parser-generator and the rest of the
  fractl languages.
- The compiled version of Common Lisp can be implemented in fractl languages.
- This new version of Common Lisp can then be used to loop back and implement
  the full system using native compilation.

## Usage

To run the IDE on Linux (the only supported platform) make sure that SBCL and a
C toolchain are installed and then, in the project root:

```
make bin/fed
./bin/fed
```

# Fractl Text (ftx)

The core part of fractl.  This subproject contains the grammar system for
parsing fractl languages, and the textual representation that enables parsing to
be done incrementally (which is an essential feature of the editor).  It also
contains the implementation of three fractl languages: gamma is the PEG-like
grammar, lambda is the grammar for Common Lisp, and phi is a syntax for Common
Lisp that is indentation-based (like Python).

The parser generator is based on the Earley Virtual Machine, outlined in the
doctoral dissertation "Extensible parsing with Earley virtual machines" (Audrius
Šaikūnas, 2019).  It works by compiling a PEG-like grammar into the bytecode for
a virtual machine that performs the actual parsing.  It is designed to be
extensible, so that languages may be extended mid-parse.  In effect, you can
define a grammar and then start to use it in the same file.

The Earley Virtual Machine has been extended to act incrementally on a rope,
which is the data structure that FED uses to represent text in buffers.  This
means that, as you type in the text editor, only the grammar non-terminals that
can be affected by that change are re-executed by the VM.  That way, a full
parse-tree is maintained in the buffer (to be used by the facilities for
syntax-highlighting, delimiter-matching, completion, etc.) with minimal latency
experienced by the user.

# Fractl Editor (fed)

The text editor and main interface for programmers of fractl.  Usable only in
xterm-compatible terminals.  It is based on a combination of Vim and Emacs.
From Vim it borrows the concept of modal editing, and most of the
normal/visual/operator-mode bindings.  Vim users will be very comfortable using
FED.  Insert-mode, however, is mostly populated by keybindings borrowed from
Emacs, so Emacs users may also feel at home if they don't use the other modes.
This gives the user the best of both worlds, since Vim's power comes primarily
from outside of insert-mode, and Emacs keybindings are intended to coexist with
normal insertion.

FED is designed to be extended in either Common Lisp or any other fractl
language (since they are all integrated).  You could even write plugins for FED
in shell script if you really wanted (with some modifications), and it would run
inside the same IDE process.

# Fractl Shell (fsh)

A simple POSIX-like shell that is built into FED (use `C-z` to switch between
the editor and the CLI).  Vim has a dedicated command-mode but in FED this is
unified with FSH.  If you want to open a particular buffer, for example, you
would type `b <name>`.  Since the shell is running in the same process as the
editor, they can be fully integrated.  If you `cd` in the shell, the editor will
also be in the new directory.

# Fractl Zero (f0, incomplete)

The Common Lisp interpreter used in the early stages of the bootstrapping
procedure.  It implements only the most basic Common Lisp features: the special
forms, and some built-in functions that cannot be implemented in Common Lisp
itself (such as primitive data accessors and type predicates).

The seed is a basic, threaded implementation of Forth.  It is based on the
excellent `jonesforth`, which was originally written in x86 assembly, and which
has been adapted for the AArch64 architecture.  As it is, the generated file is
tens of KB, and it would ideally be much smaller: small enough for a person to
be able to fully audit the binary file by hand.

The Forth language is extended in Forth itself, and a buddy memory allocator is
implemented for heap objects.

The full Forth system is then used to build a simple Common Lisp system.  This
includes the core built-in Common Lisp data types and their primitives, a parser
for Common Lisp s-expressions, and a compiler for Common Lisp forms down to
Forth words.  The threaded Forth runtime is then used to actually interpret the
Common Lisp functions.  (This construction is more like a byte-code
compiler/interpreter than either a native compiler or a full interpreter.)

# Fractl Common Lisp (fcl, incomplete)

The libraries built on top of a primitive Common Lisp execution engine that
implement a spec-conformant Common Lisp environment (as well as a Common Lisp in
Common Lisp interpreter that is useful for testing).  The files are executed in
a particular order so that earlier files do not depend in any way (not even at
compile-time) on code that is defined in later files.  The first file
(`base.lisp`) assumes the existence of only very basic built-in functions.

One of the most complex parts of this implementation is the Common Lisp Object
System (CLOS) defined in `type.lisp`.  What makes this especially difficult is
that CLOS is meta-circular: classes are objects themselves, with classes of
their own.  For example, the class `standard-class` is an object whose class is
itself.  Setting up the entire system requires a careful bootstrapping
procedure.

# Fractl Assembly (fasm, incomplete)

The middle-end and back-end compiler infrastructure.  It defines an Intermediate
Representation (IR) that can be targeted by any compiled fractl language or
system.  The IR is then compiled to AArch64 assembly, after register allocation,
and that is then assembled and encoded to form the final object file, or linked
and executed directly in memory.

Most of the essential parts of the AArch64 instruction-set architecture are
supported by the assembler, in particular those instructions which might be used
by a simple native compiler.  More work would be needed to implement floating
point, SIMD, many system instructions, atomics, and other miscellaneous
instructions.

# Fractl Compiler (fc, incomplete)

A fractl sublanguage and compiler for a C-like systems programming language.  It
targets the FASM IR.  However, unlike C, FC uses a Hindley-Milner type system
with traits for polymorphism, similar to Rust.  It might appeal to those who
like the freedom and versatility of C, but who also appreciate a sane, modern
type system (and eventually Lisp macros for metaprogramming).

# Fractl Logic (flog, incomplete)

Another fractl sublanguage for a logic programming language similar to Prolog.
It is primarily based on "Warren's Abstract Machine: A Tutorial Reconstruction"
(Hassan Aït-Kaci, 1991), and used to implement the type system for FC.

# Fractl One (f1, planned)

A compiler for Common Lisp, equivalent to F0 except using the infrastructure for
native compilation, and implemented primarily in fractl itself.  It would then
recurse as the implementation for the previous fractl components.
