-module(day05).
-export([main/1]).

main(_Args) ->
    print_solution(input()).

print_solution(0) -> io:format("");
print_solution(N) -> io:format("~s", [N]).
input()->
    case io:get_line("") of
      eof -> [];
      Line -> Line ++ input()
    end.
