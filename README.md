MAlice
======
MAlice compiler for Milestone III in Haskell.
All of our code is in src/MAlice.
The compiler can be run with ./compile /path/to/file.alice, or alternatively
dist/build/compile/compile -c /path/to/file.alice -o /path/to/output.j
Note that invoking the compiler directly will NOT assemble the generated JVM
bytecode, the actual "runnable" script will only be created by running ./compile.

The extension is located in the same binary and can be accessed by running
./interactive /path/to/file.alice, or dist/build/compile/compile --interactive=/path/to/file.alice.
Note that path names used when directly invoking the compiler have to be complete,
i.e. can't be ~/path/to/file.alice (instead /home/username/path/...). This is also true
for loading files from the interactive shell with the :l command. Since haskell-platform
doesn't contain Unix readline bindings (and we didn't want to import more dependencies),
it is highly recommended to use rlwrap with the interactive shell to get command history.
The ./interactive script already does this for you (it also allows you to specify normal
unix filenames). 
