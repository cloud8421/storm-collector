-module(parser_test).

-c(parser).

-include_lib("eunit/include/eunit.hrl").

parses_payload_test() ->
  Expected = #{temp => 23.4, bright => 4000},
  ?assert(parser:parse("temp:f:23.4|bright:i:4000") == Expected).
