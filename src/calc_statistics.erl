-module(calc_statistics).

-export([calc/1, auto/0, readLines/1, parseToRecords/1, getMin/1, getMax/1]).

-record(user, {
    name,
    age
}).

split(Str, Del) -> string:tokens(Str, Del).

readLines(Fin) ->
    case file:read_file(Fin) of
        { ok, Bin } ->
            BinData = erlang:binary_to_list(Bin),
            Lines = split(BinData, "\n"),
            io:format("[~B]\n", [length(Lines)]),
            { ok, Lines};
        _ ->
            { error, []}
    end.

%%% start parseToRecords
parseToRecords([]) -> [];
parseToRecords(Lines) ->
    % string:to_integer(AgeStr)
    ParseAge = fun(Age) ->
        {PAge, _} = string:to_integer(Age),
        PAge
    end,
    _Splitted = [ split(Line, ",") || Line <- Lines ],
    Records = [ #user{ name = Name, age = ParseAge(Age) } || [ Age, Name] <- _Splitted ],
    Records.
%%% end parseToRecords

%%% start printRecords
printRecords([]) -> ok;
printRecords([H|T]) ->
    case H of
        { user, Name, Age } ->
            io:format("\~s \~s~n", [Name, Age]),
            printRecords(T);
        _ -> printRecords(T)
    end.
%%% end printRecords

%%% start print list
print([]) -> ok;
print([H|T]) ->
    io:format("\~b\n", [H]),
    print(T).
%%% end printlist

%%% start getSum
getSum([H|T]) -> getSum(T, H).
getSum([], Acc) -> Acc;
getSum([H|T], Acc) -> getSum(T, Acc + H).
%%% end getSum

%%% start getMin
getMin([H|T]) -> getMin(T, H).
getMin([], Min) -> Min;
getMin([H|T], Min) when H < Min -> getMin(T, H);
getMin([_|T], Min) -> getMin(T, Min).
%%% end getMin

%%% start getMax
getMax([H|T]) -> getMax(T, H).
getMax([], Max) -> Max;
getMax([H|T], Max) when H > Max -> getMax(T, H);
getMax([_|T], Max) -> getMax(T, Max).
%%% end getMax

calc(Fin) ->
    { _, Lines} = readLines(Fin),
    Records = parseToRecords(Lines),
    % printRecords(Records),
    Ages = [User#user.age || User <- Records],
    print(Ages),
    Len = length(Ages),
    AgeMin = getMin(Ages),
    AgeMax = getMax(Ages),
    AgeSum = getSum(Ages),
    Avg = AgeSum/Len,
    % io:format("Avg:\~B, Max:\~B, Min:\~B", [Avg, AgeMax, AgeMin]),
    { Avg, AgeMax, AgeMin}.

auto() ->
    { Time, _ } = timer:tc(calc_statistics, calc, ["output.file"]),
    Time / 1000000.