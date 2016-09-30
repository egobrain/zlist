-module(zlist_st_tests).

-include_lib("eunit/include/eunit.hrl").

from_list_test_() ->
    [
     {<<"empty">>,
      fun() ->
          Zs = zlist_st:from_list([]),
          ?assertEqual({[], 1}, Zs(1))
      end},
     {<<"one">>,
      fun() ->
          Zs = zlist_st:from_list([1]),
          {[1|Zs2], 1} = Zs(1),
          ?assertEqual({[], 2}, Zs2(2))
      end},
     {<<"many">>,
      fun() ->
          List = lists:seq(1, 100),
          {LastZs, St} = lists:foldl(
              fun(E, {Zs, S}) -> {[E|Zs2], S2} = Zs(S), {Zs2, S2+1} end,
              {zlist_st:from_list(List), 0},
              List),
          ?assertEqual({[], 100}, LastZs(St))
      end}
    ].

seq_test_() ->
    [
     {<<"seq/2">>,
      fun() ->
          Zs = zlist_st:seq(1, 10),
          ?assertEqual({[1,2,3,4,5,6,7,8,9,10], st}, zlist_st:to_list(Zs, st))
      end},
     {<<"seq/3">>,
      fun() ->
          Zs = zlist_st:seq(1, 10, 4),
          ?assertEqual({[1,5,9], st}, zlist_st:to_list(Zs, st))
      end},
     {<<"neg seq/3">>,
      fun() ->
          Zs = zlist_st:seq(3, -4, -1),
          ?assertEqual({[3,2,1,0,-1,-2,-3,-4], st}, zlist_st:to_list(Zs, st))
      end},
     {<<"zero seq/0">>,
      fun() ->
          Zs = zlist_st:seq(3, 3, 0),
          ?assertEqual({[3], st}, zlist_st:to_list(Zs, st))
      end}
    ].

to_list_test() ->
    Zs0 = zlist_st:empty(),
    Zs1 = fun(S1) -> {[1|Zs0], S1} end,
    Zs2 = fun(S2) -> {[2|Zs1], S2} end,
    Zs3 = fun(S3) -> {[3|Zs2], S3} end,
    ?assertEqual({[], st}, zlist_st:to_list(Zs0, st)),
    ?assertEqual({[1], st}, zlist_st:to_list(Zs1, st)),
    ?assertEqual({[2, 1], st}, zlist_st:to_list(Zs2, st)),
    ?assertEqual({[3, 2, 1], st}, zlist_st:to_list(Zs3, st)).

map_test_() ->
    Tests =
        [
         {[], []},
         {[1], [2]},
         {[1,2,3,4], [2,4,6,8]}
        ],
    [
     {iolist_to_binary(io_lib:format("~p", [D])),
      fun() ->
          Zs = zlist_st:from_list(D),
          MZs = zlist_st:map(fun(A) -> A*2 end, Zs),
          ?assertEqual({R, st}, zlist_st:to_list(MZs, st))
      end} || {D, R} <- Tests
    ].

foreach_test_() ->
    K = '$foreach_test_',
    Tests =
        [
         [],
         [1],
         [1,2,3,4]
        ],
    [
     {iolist_to_binary(io_lib:format("~p", [D])),
      fun() ->
          put(K, []),
          st = zlist_st:foreach(fun(E) -> put(K, [E|get(K)]) end, zlist_st:from_list(D), st),
          ?assertEqual(D, lists:reverse(get(K)))
      end} || D <- Tests
    ].

filter_test_() ->
    Tests =
        [
         {[], []},
         {[1], []},
         {[1,2,3,4], [2,4]}
        ],
    [
     {iolist_to_binary(io_lib:format("~p", [D])),
      fun() ->
          Zs = zlist_st:from_list(D),
          MZs = zlist_st:filter(fun(A) -> A rem 2 =:= 0 end, Zs),
          ?assertEqual({R, st}, zlist_st:to_list(MZs, st))
      end} || {D, R} <- Tests
    ].

fold_test_() ->
    Tests =
        [
         [],
         [1],
         [1,2,3,4]
        ],
    [
     {iolist_to_binary(io_lib:format("~p", [D])),
      fun() ->
          Zs = zlist_st:from_list(D),
          {R, st} = zlist_st:fold(fun(E, A) -> [E|A] end, [], Zs, st),
          ?assertEqual(D, lists:reverse(R))
      end} || D <- Tests
    ].

append_test_() ->
    Tests =
        [
         {[1], [2], [1,2]},
         {[], [1,2,3], [1,2,3]},
         {[1,2,3], [], [1,2,3]}
        ],
    [{<<"zlist_st:empty">>, fun() -> {[], st} = (zlist_st:append(zlist_st:empty(), zlist_st:empty()))(st) end} |
     [
      {iolist_to_binary(io_lib:format("~p + ~p = ~p", [A, B, R])),
       fun() ->
           {R, F, st} = zlist_st:take(length(R), zlist_st:append(zlist_st:from_list(A), zlist_st:from_list(B)), st),
           ?assertEqual({[], st}, F(st))
       end} || {A, B, R} <- Tests
     ]].

take_test() ->
    Zs = zlist_st:seq(1, 10),
    {[1], _, st} = zlist_st:take(1, Zs, st),
    {[1,2], _, st} = zlist_st:take(2, Zs, st),
    {_, LastZs, st} = zlist_st:take(100, Zs, st),
    ?assertEqual({[], st}, LastZs(st)).

take_while_test() ->
    {[1,2,3,4,5], Zs1, st} = zlist_st:takewhile(fun(A) -> A < 6 end, zlist_st:seq(1, 10), st),
    ?assertEqual({[6,7,8,9,10], st}, zlist_st:to_list(Zs1, st)),
    {[1,2,3,4], Zs2, st} = zlist_st:takewhile(fun(A) -> A < 6 end, zlist_st:seq(1, 4), st),
    ?assertEqual({[], st}, zlist_st:to_list(Zs2, st)).

over_test() ->
    ?assertEqual(
         {[0,1,3,6,10], st},
         zlist_st:to_list(
             zlist_st:over(fun(A, B) -> {B, A+B} end, 0,
                  zlist_st:seq(1,5)), st)).

recurrent_test() ->
    {Result, _, st} = zlist_st:take(6, zlist_st:recurrent(fun(A) -> A + 1 end, 0), st),
    ?assertEqual([1,2,3,4,5,6], Result).

dropwhen_test() ->
    Zs1 = zlist_st:dropwhen(fun(A) -> A > 5 end, zlist_st:seq(1, 10)),
    ?assertEqual({[1,2,3,4,5], st}, zlist_st:to_list(Zs1, st)),
    Zs2 = zlist_st:dropwhen(fun(A) -> A > 5 end, zlist_st:seq(1, 4)),
    ?assertEqual({[1,2,3,4], st}, zlist_st:to_list(Zs2, st)).

dropwhile_test() ->
    Zs1 = zlist_st:dropwhile(fun(A) -> A =< 5 end, zlist_st:seq(1, 10)),
    ?assertEqual({[6,7,8,9,10], st}, zlist_st:to_list(Zs1, st)),
    Zs2 = zlist_st:dropwhile(fun(A) -> A =< 5 end, zlist_st:seq(1, 5)),
    ?assertEqual({[], st}, zlist_st:to_list(Zs2, st)).

filtermap_test() ->
    Zs = zlist_st:seq(1, 10),
    Zs2 = zlist_st:filtermap(fun(A) ->
        case A rem 2 of
            1 -> {true, A*2};
            0 -> false
        end
    end, Zs),
    ?assertEqual({[2,6,10,14,18], st}, zlist_st:to_list(Zs2, st)).

flatmap_test() ->
    Zs = zlist_st:seq(1, 3),
    Zs2 = zlist_st:flatmap(fun(A) ->
        zlist_st:from_list(lists:seq(A, A+2))
    end, Zs),
    ?assertEqual({[1,2,3,2,3,4,3,4,5], st}, zlist_st:to_list(Zs2, st)).

ciclyc_test() ->
    Zs = zlist_st:seq(1, 3),
    Zs2 = zlist_st:ciclyc(Zs),
    {R, _, st} = zlist_st:take(10, Zs2, st),
    ?assertEqual([1,2,3,1,2,3,1,2,3,1], R).

take_by_test() ->
    ?assertEqual({[[1,2,3],[4,5,6],[7,8]], st}, zlist_st:to_list(zlist_st:take_by(3, zlist_st:seq(1,8)), st)),
    ?assertEqual({[], st}, zlist_st:to_list(zlist_st:take_by(3, zlist_st:empty()), st)),
    ?assertEqual({[[1],[2],[3]], st}, zlist_st:to_list(zlist_st:take_by(1, zlist_st:seq(1,3)), st)),
    ?assertEqual({[[1,2,3]], st}, zlist_st:to_list(zlist_st:take_by(4, zlist_st:seq(1,3)), st)),
    ZsSeq = zlist_st:seq(1, 30),
    GroupedZs = zlist_st:take_by(100, ZsSeq),
    {[_|RestZs], st} = GroupedZs(st),
    ?assertEqual({[], st2}, RestZs(st2)).
