-module(generate_data).

-export([gen/3, auto/1]).

-define(MIN_AGE, 14).
-define(MAX_AGE, 60).

readNames(Fin) ->
    case file:read_file(Fin) of
        { ok, Bin } ->
            { ok, string:tokens(erlang:binary_to_list(Bin), "\n")};
        _ ->
            { error, []}
    end.

getRandom(Min, Max) ->
    Rval = rand:uniform() * (Max - Min) + Min,
    round(Rval).

getName(NeedInd, CurInd, [H | _]) when NeedInd =:= CurInd -> H;
getName(NeedInd, CurInd, [_ | T]) -> getName(NeedInd, CurInd + 1, T).

writeRec(Fptr, Name) ->
    Age = getRandom(?MIN_AGE, ?MAX_AGE),
    io:format("\~B,\~s~n", [Age, Name]),
    io:fwrite(Fptr, "\~B,\~s~n", [Age, Name]).

writeRandomName(_, _, _, _, 0) -> ok;
writeRandomName(Fptr, Min, Max, Names, Limit) ->
    Ind = getRandom(Min, Max),
    Name = getName(Ind, 0, Names),
    writeRec(Fptr, Name),
    writeRandomName(Fptr, Min, Max, Names, Limit - 1).

generate(Fout, Names, Limit) ->
    {ok, Fptr} = file:open(Fout, write),
    Min = 0,
    Max = length(Names) - 1,
    writeRandomName(Fptr, Min, Max, Names, Limit),
    file:close(Fptr).

gen(Fin, Fout, Limit) ->
    case readNames(Fin) of
        { ok, Names } -> generate(Fout, Names, Limit);
        _ -> error
    end.

auto(Limit) ->
    { Time, _ } = timer:tc(generate_data, gen, ["input.file", "output.file", Limit]),
    Time / 1000000.