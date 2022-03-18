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
