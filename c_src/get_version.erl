-module(get_version).

-export([main/1]).

main([FilePath]) ->
    {ok, Content} = file:read_file(FilePath),
    ContentStr = binary_to_list(Content),
    Match = re:run(ContentStr, ".*VERSION.*,.*(\\d+).*", [{capture, all_but_first, binary}]),
    case Match of
        {match, [MatchBin | _]} ->
            %%erlang:display({ContentStr, Start, Len}),
            %%VersionStr = string:substr(ContentStr, Start, 2),
            io:format("~s", [MatchBin])
    end.
