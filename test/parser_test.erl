-module(parser_test).

-c(parser).

-include_lib("eunit/include/eunit.hrl").

parses_payload_test() ->
  Expected = [{temperature,23.4}, {brightness,4000}],
  ?assert(parser:parse("temperature:f:23.4|brightness:i:4000\r\n") == Expected),
  ?assert(parser:parse("temperature:f:23.4|brightness:i:4000\n\r") == Expected),
  ?assert(parser:parse("temperature:f:23.4|brightness:i:4000") == Expected).
