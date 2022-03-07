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
  1> jqerl:parse(<<".a">>, <<"{\"b\": 1}">>).
  {ok,[<<"null">>]}
  
  2> jqerl:parse(<<".a">>, <<"{\"a\": {\"b\": {\"c\": 1}}}">>).
  {ok,[<<"{\"b\":{\"c\":1}}">>]}
  
  3> jqerl:parse(<<".a|.[]">>, <<"{\"a\": [1,2,3]}">>).
  {ok,[<<"1">>,<<"2">>,<<"3">>]}
  ```
