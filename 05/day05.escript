-module(day05).
-export([main/1]).

parse_update_list(L) ->
    [begin {Int, _} = string:to_integer(E), Int end || E <- L].

parse_updates(U) ->
    UpdateListsRaw = lists:droplast(re:split(U, "\n")),
    UpdateLists = [re:split(E, ",") || E <- UpdateListsRaw],
    UpdateListsNum = [parse_update_list(L) || L <- UpdateLists],
    UpdateListsNum.

parse_ordering_rule(R) ->
    RuleSplit = string:tokens(R, "|"),
    RuleNum = [begin {Int, _} = string:to_integer(E), Int end || E <- RuleSplit],
    [Left, Right] = RuleNum,
    {Left, Right}.

parse_page_ordering(O) ->
    OrderingRaw = re:split(O, "\n", [{return, list}]),
    OrderingRules = [parse_ordering_rule(E) || E <- OrderingRaw],
    GetKey = fun({K, _}) -> K end,
    GetVal = fun({_, V}) -> V end,
    maps:groups_from_list(GetKey, GetVal, OrderingRules).

parse_input(I) ->
    [PageOrderingRaw, UpdateRaw] = re:split(I, "\n\n", [{return, list}]),
    PageOrdering = parse_page_ordering(PageOrderingRaw),
    Updates = parse_updates(UpdateRaw),
    [PageOrdering, Updates].

check_rule_list(_, [], _) -> true;
check_rule_list(_, Rules, Preceding) ->
    RuleSet = sets:from_list(Rules),
    PrecedingSet = sets:from_list(Preceding),
    Intersect = sets:intersection(RuleSet, PrecedingSet),
    sets:is_empty(Intersect).

is_valid(_, [], _) -> true;
is_valid(RuleMap, [UpdateH | UpdateT], Preceding) ->
    Rule = maps:get(UpdateH, RuleMap, []),
    Current = check_rule_list(UpdateH, Rule, Preceding),
    Current and is_valid(RuleMap, UpdateT, Preceding ++ [UpdateH]).

find_middle_element(List) -> lists:nth(length(List) div 2 + 1, List).

main(_Args) ->
    I = input(),
    [PageOrderingRules, UpdateLists] = parse_input(I),
    IsValFilter = fun(List) -> is_valid(PageOrderingRules, List, []) end,
    {ValidUpdates, InvalidUpdates} = lists:partition(IsValFilter, UpdateLists),

    % Part 1
    Mids = [find_middle_element(E) || E <- ValidUpdates],
    print_solution(lists:sum(Mids)),

    % Part 2
    SortFun = fun(A, B) -> R = maps:get(A, PageOrderingRules, []), lists:member(B, R) end,
    SortedUpdates = [lists:sort(SortFun, E) || E <- InvalidUpdates],
    SortedMids = [find_middle_element(E) || E <- SortedUpdates],
    print_solution(lists:sum(SortedMids)).


print_solution(0) -> io:format("");
print_solution(N) -> io:format("~lp~n", [N]).

input()->
    case io:get_line("") of
      eof -> [];
      Line -> Line ++ input()
    end.
