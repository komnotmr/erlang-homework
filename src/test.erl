-module(test).

-export([start/2]).

start(NumberTest, Args) ->
    case NumberTest of
        1 ->
            [CountUsers] = Args,
            io:format("Generate: ~f sec.~nStatistics: ~f sec.", [
                generate_data:auto(CountUsers),
                calc_statistics:auto()
            ]),
            ok;
        _ -> error
    end.