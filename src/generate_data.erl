-module(generate_data).

-export([
    gen/3,
    auto/1
]).

-define(MIN_AGE, 14).
-define(MAX_AGE, 60).
-define(AUTO_PATH_INPUT, "input.file").
-define(AUTO_PATH_OUTPUT, "output.file").

%%% readNames
readNames(Fin) ->
    case file:read_file(Fin) of
        { ok, Bin } -> { ok, string:tokens(erlang:binary_to_list(Bin), "\n") };
        _ -> { error, []}
    end.

%%% getRandom
getRandom(Min, Max) -> round(rand:uniform() * (Max - Min) + Min).

%%% getName
getName(NeedInd, CurInd, [H | _]) when NeedInd =:= CurInd -> H;
getName(NeedInd, CurInd, [_ | T]) -> getName(NeedInd, CurInd + 1, T).

%%% writeRec
writeRec(Fptr, Name) ->
    Age = getRandom(?MIN_AGE, ?MAX_AGE),
    io:fwrite(Fptr, "\~B,\~s~n", [Age, Name]).

%%% writeRandomName
writeRandomName(_, _, _, _, 0) -> ok;
writeRandomName(Fptr, Min, Max, Names, Limit) ->
    Ind = getRandom(Min, Max),
    Name = getName(Ind, 0, Names),
    writeRec(Fptr, Name),
    writeRandomName(Fptr, Min, Max, Names, Limit - 1).

%%% generate
generate(Fout, Names, Limit) ->
    { ok, Fptr } = file:open(Fout, write),
    writeRandomName(Fptr, 0, length(Names) - 1, Names, Limit),
    file:close(Fptr).

%%% gen
gen(Fin, Fout, Limit) ->
    case readNames(Fin) of
        { ok, Names } -> generate(Fout, Names, Limit);
        _ -> error
    end.

%%% auto
auto(Limit) ->
    { Time, _ } = timer:tc(
        generate_data
        , gen
        , [
            ?AUTO_PATH_INPUT
            , ?AUTO_PATH_OUTPUT
            , Limit
        ]
    ),
    Time / 1000000.