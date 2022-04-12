# Erlang jq Library

Erlang library for processing JSON with [jq][1]. The library has both a
[NIF][2]-based implementation and a [port][3]-based implementation.

## Usage

  ```
  $ rebar3 shell
  ...
  1> jq:process_json(<<".a">>, <<"{\"b\": 1}">>).
  {ok,[<<"null">>]}
  
  2> jq:process_json(<<".a">>, <<"{\"a\": {\"b\": {\"c\": 1}}}">>).
  {ok,[<<"{\"b\":{\"c\":1}}">>]}
  
  3> jq:process_json(<<".a|.[]">>, <<"{\"a\": [1,2,3]}">>).
  {ok,[<<"1">>,<<"2">>,<<"3">>]}
  ```

The public API for this library is specified by the exported functions in the
`jq` module. Most users should not need to care about the other modules. See
the [jq documentation][5] for more information about how to write jq filter
programs.

## Compiling
  ```
  $ rebar3 compile
  ```

## Testing

The [valgrind][4] memory analyzer is needed for one test
case (this test case will be skipped if valgrind is not installed).

  ```
  $ rebar3 eunit
  ```
  
## Configuration

The configurations can be changed by passing `-jq OptionName Value` as an `erl`
command line option or programmatically by using one of the
`application:set_env` functions before the jq library has been loaded. Most of
settings below will only take effect if they are set before the jq Erlang
application is loaded (the jq Erlang application is loaded automatically when
`jq:process_json/2` is called).


* `jq_filter_program_lru_cache_max_size` (default value = 500) - Sets the size of
  the LRU caches that are holding compiled JQ programs to prevent frequent
  expensive recompilation of the same program.
* `jq_implementation_module` (default value = `jq_port`) - Sets the implementation
  that will be used. The options are:
  * `jq_port` - This implementation uses a port program to interact with jq.
    This is the most safe option as a bug in jq cannot
    cause the Erlang VM to crash or leak memory.
  * `jq_nif` - This implementation uses a NIF library to interact with jq. This
    option is faster than the `jq_port` option but it is also less safe even
    though we are currently not not aware of any problems with this option.
* `jq_port_nr_of_jq_port_servers` (default value =
  `erlang:system_info(schedulers)`) (only relevant for the `jq_port` option) -
  Use this option to set how many port programs that will handle jq requests.
  Higher values can lead to better performance (due to parallelism) at the
  expense of increased memory usage and cache locality. 
* `jq_port_restart_period` (default value = 1000000) (only relevant for the
  `jq_port` option) - Use this option to set how many `jq:process_json/2` calls
  a port program can process before it is restarted. This is a safety option
  that can be handy if it turn out that the jq port program has memory leaks.
  As far as we know the port program does not have any memory leaks but it is
  possible that it is leaky for inputs that we have not tested (please report a
  bug if you find any leaks).

## Test with address sanitizer
  
  There are scripts that can help when one wants to run the eunit tests with
  address sanitizer. This is only relevant for the NIF-based implementation.
  The port-based implementation is tested with `valgrind` in the eunit suit
  automatically. Address sanitizer is included in recent versions of gcc and
  clang.
  
  Use the following commands to run the eunit tests with
  address sanitizer:
  ```
  ./test/address_sanitizer_setup.sh 
  ./test/address_sanitizer_run_eunit.sh
  ```
  The "test/address_sanitizer_setup.sh" scripts compiles an
  erlang VM with address sanitizer support and don't need
  to be executed every time a change is made to the NIF.

## About hot upgrading

This library supports hot code reloading/upgrading. For the hot upgrade from an
old version to a new version of the NIF-based implementation to go smoothly,
the new version needs to have a different number in the version macro
(`-define(VERSION, NUM)`) in `src/jq_nif.erl`. This version number is read by
the `Makefile` for the shared library. The `Makefile` produces a shared library
with the version number in its file name. [Most operating systems require that
the new shared library has a different name than the previous one][6]
(otherwise, the operating system will not load the new library). One should
thus always increase the version number in the version macro before releasing a
new version of this library.



[1]: https://github.com/stedolan/jq
[2]: https://www.erlang.org/doc/man/erl_nif.html
[3]: https://www.erlang.org/doc/tutorial/c_port.html
[4]: https://valgrind.org/
[5]: https://stedolan.github.io/jq/manual/v1.6
[6]: https://www.erlang.org/doc/man/erlang.html#load_nif-2
