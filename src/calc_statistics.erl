-module(calc_statistics).

-export([
    calc/1,
    auto/0
]).

-record(user, {
    name,
    age
}).

%%% split
split(Str, Del) -> string:tokens(Str, Del).

%%% readLines
readLines(Fin) ->
    case file:read_file(Fin) of
        { ok, Bin } -> {
                ok,
                split(erlang:binary_to_list(Bin), "\n")
            };
        _ ->
            { error, []}
    end.

%%% parseToRecords
parseToRecords([]) -> [];
parseToRecords(Lines) ->
    ParseAge = fun(Age) ->
        {PAge, _} = string:to_integer(Age),
        PAge
    end,
    [
        #user{
            name = Name,
            age = ParseAge(Age)
        }
        || [ Age, Name] <- [ split(Line, ",") || Line <- Lines ]
    ].

%%% printRecords
printRecords([]) -> ok;
printRecords([H|T]) ->
    case H of
        { user, Name, Age } ->
            io:format("~s:~b~n", [Name, Age]),
            printRecords(T);
        _ -> printRecords(T)
    end.

%%% print
print([]) -> ok;
print([H|T]) ->
    io:format("~b~n", [H]),
    print(T).

%%% getSum
getSum([H|T]) -> getSum(T, H).
getSum([], Acc) -> Acc;
getSum([H|T], Acc) -> getSum(T, Acc + H).

%%% getMin
getMin([H|T]) -> getMin(T, H).
getMin([], Min) -> Min;
getMin([H|T], Min) when H < Min -> getMin(T, H);
getMin([_|T], Min) -> getMin(T, Min).

%%% getMax
getMax([H|T]) -> getMax(T, H).
getMax([], Max) -> Max;
getMax([H|T], Max) when H > Max -> getMax(T, H);
getMax([_|T], Max) -> getMax(T, Max).

%%% calc
calc(Fin) ->
    { _, Lines} = readLines(Fin),
    Records = parseToRecords(Lines),
    Ages = [User#user.age || User <- Records],
    % printRecords(Records),
    % print(Ages),
    CountRecords = length(Ages),
    AgeMin = getMin(Ages),
    AgeMax = getMax(Ages),
    Avg = getSum(Ages)/CountRecords,
    io:format("Records count:~b~nAvg:~f, Max:~b, Min:~b~n", [
        CountRecords
        , Avg
        , AgeMax
        , AgeMin
    ]),
    { Avg, AgeMax, AgeMin}.

auto() ->
    { Time, _ } = timer:tc(calc_statistics, calc, ["output.file"]),
    Time / 1000000.