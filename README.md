# jq

An Erlang NIF for [jq](https://github.com/stedolan/jq).

## Usage

- Build
  ```
  $ rebar3 compile
  ```

- Have a try
  ```
  $ rebar3 shell
  ...
  1> jq:parse(<<".a">>, <<"{\"b\": 1}">>).
  {ok,[<<"null">>]}
  
  2> jq:parse(<<".a">>, <<"{\"a\": {\"b\": {\"c\": 1}}}">>).
  {ok,[<<"{\"b\":{\"c\":1}}">>]}
  
  3> jq:parse(<<".a|.[]">>, <<"{\"a\": [1,2,3]}">>).
  {ok,[<<"1">>,<<"2">>,<<"3">>]}
  ```

- Test with address sanitizer
  
  There are scripts that can help when one wants to run the
  eunit tests with address sanitizer. Address sanitizer is
  included in recent versions of gcc and clang.
  
  Use the following commands to run the eunit tests with
  address sanitizer:
  ```
  ./test/address_sanitizer_setup.sh 
  ./test/address_sanitizer_run_eunit.sh
  ```
  The "test/address_sanitizer_setup.sh" scripts compiles an
  erlang VM with address sanitizer support and don't need
  to be executed every time a change is made to the NIF.

- About hot upgrading

This library supports hot code reloading/upgrading. For the hot upgrade from an
old version to a new version to go smoothly, the new version needs to have a
different number in the version macro (`-define(VERSION, NUM)`) in
`src/jq.erl`. This version number is read by the `Makefile` for the shared
library. The `Makefile` produces a shared library with the version number in
its file name. [Most operating systems require that the new shared library has
a different name than the previous
one](https://www.erlang.org/doc/man/erlang.html#load_nif-2) (otherwise, the
operating system will not load the new library). One should thus always
increase the version number in the version macro before releasing a new version
of this library.

