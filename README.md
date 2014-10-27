# falsec #

A False compiler for x86_64.

## f2 ##

`f2` is the actual top-level compiler from false to the target intermediate
language. By default, `nasm` is used as the intermediate language.

The options currently are:

- `nasm`
- `c`

Hopefully more will be added for fun.


## falsec frontend ##

`falsec` is a python script that turns `f2` into an end to end compiler for
false. This manages the creation of intermediate files or pipes to produce a
final binary through the target route desired. It allows for the same targets as
`f2`.

`falsec` also allows for intermediate output, or just passing through the output
of `f2`. This makes falsec the desired user endpoint.
