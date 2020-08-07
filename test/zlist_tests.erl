-module(zlist_tests).

-include_lib("eunit/include/eunit.hrl").

from_list_test_() ->
    [
     {<<"empty">>,
      fun() ->
          Z = zlist:from_list([]),
          ?assertEqual([], Z())
      end},
     {<<"one">>,
      fun() ->
          Z = zlist:from_list([1]),
          [1|Z2] = Z(),
          ?assertEqual([], Z2())
      end},
     {<<"many">>,
      fun() ->
          List = lists:seq(1, 100),
          LastZ = lists:foldl(
              fun(E, Z) -> [E|Z2] = Z(), Z2 end,
              zlist:from_list(List),
              List),
          ?assertEqual([], LastZ())
      end}
    ].

seq_test_() ->
    [
     {<<"seq/2">>,
      fun() ->
          Z = zlist:seq(1, 10),
          ?assertEqual([1,2,3,4,5,6,7,8,9,10], zlist:to_list(Z))
      end},
     {<<"seq/3">>,
      fun() ->
          Z = zlist:seq(1, 10, 4),
          ?assertEqual([1,5,9], zlist:to_list(Z))
      end},
     {<<"neg seq/3">>,
      fun() ->
          Z = zlist:seq(3, -4, -1),
          ?assertEqual([3,2,1,0,-1,-2,-3,-4], zlist:to_list(Z))
      end},
     {<<"zero seq/0">>,
      fun() ->
          Z = zlist:seq(3, 3, 0),
          ?assertEqual([3], zlist:to_list(Z))
      end}
    ].

to_list_test() ->
    Z0 = zlist:empty(),
    Z1 = fun() -> [1|Z0] end,
    Z2 = fun() -> [2|Z1] end,
    Z3 = fun() -> [3|Z2] end,
    ?assertEqual([], zlist:to_list(Z0)),
    ?assertEqual([1], zlist:to_list(Z1)),
    ?assertEqual([2, 1], zlist:to_list(Z2)),
    ?assertEqual([3, 2, 1], zlist:to_list(Z3)).

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
          Z = zlist:from_list(D),
          MZ = zlist:map(fun(A) -> A*2 end, Z),
          ?assertEqual(R, zlist:to_list(MZ))
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
          ok = zlist:foreach(fun(E) -> put(K, [E|get(K)]) end, zlist:from_list(D)),
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
          Z = zlist:from_list(D),
          MZ = zlist:filter(fun(A) -> A rem 2 =:= 0 end, Z),
          ?assertEqual(R, zlist:to_list(MZ))
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
          Z = zlist:from_list(D),
          R = zlist:fold(fun(E, A) -> [E|A] end, [], Z),
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
    [{<<"zlist:empty">>, fun() -> [] = (zlist:append(zlist:empty(), zlist:empty()))() end} |
     [
      {iolist_to_binary(io_lib:format("~p + ~p = ~p", [A, B, R])),
       fun() ->
           {R, F} = zlist:take(length(R), zlist:append(zlist:from_list(A), zlist:from_list(B))),
           ?assertEqual([], F())
       end} || {A, B, R} <- Tests
     ]].

take_test() ->
    Z = zlist:seq(1, 10),
    {[1], _} = zlist:take(1, Z),
    {[1,2], _} = zlist:take(2, Z),
    {_, LastZ} = zlist:take(100, Z),
    ?assertEqual([], LastZ()).

take_while_test() ->
    {[1,2,3,4,5], Z1} = zlist:takewhile(fun(A) -> A < 6 end, zlist:seq(1, 10)),
    ?assertEqual([6,7,8,9,10], zlist:to_list(Z1)),
    {[1,2,3,4], Z2} = zlist:takewhile(fun(A) -> A < 6 end, zlist:seq(1, 4)),
    ?assertEqual([], zlist:to_list(Z2)).

over_test() ->
    ?assertEqual(
         [0,1,3,6,10],
         zlist:to_list(
             zlist:over(fun(A, B) -> {B, A+B} end, 0,
                  zlist:seq(1,5)))).

recurrent_test() ->
    {Result, _} = zlist:take(6, zlist:recurrent(fun(A) -> A + 1 end, 0)),
    ?assertEqual([1,2,3,4,5,6], Result).

dropwhen_test() ->
    Z1 = zlist:dropwhen(fun(A) -> A > 5 end, zlist:seq(1, 10)),
    ?assertEqual([1,2,3,4,5], zlist:to_list(Z1)),
    Z2 = zlist:dropwhen(fun(A) -> A > 5 end, zlist:seq(1, 4)),
    ?assertEqual([1,2,3,4], zlist:to_list(Z2)).

dropwhile_test() ->
    Z1 = zlist:dropwhile(fun(A) -> A =< 5 end, zlist:seq(1, 10)),
    ?assertEqual([6,7,8,9,10], zlist:to_list(Z1)),
    Z2 = zlist:dropwhile(fun(A) -> A =< 5 end, zlist:seq(1, 5)),
    ?assertEqual([], zlist:to_list(Z2)).

filtermap_test() ->
    Z = zlist:seq(1, 10),
    Z2 = zlist:filtermap(fun(A) ->
        case A rem 2 of
            1 -> {true, A*2};
            0 -> false
        end
    end, Z),
    ?assertEqual([2,6,10,14,18], zlist:to_list(Z2)).

flatmap_test() ->
    Z = zlist:seq(1, 3),
    Z2 = zlist:flatmap(fun(A) ->
        zlist:from_list(lists:seq(A, A+2))
    end, Z),
    ?assertEqual([1,2,3,2,3,4,3,4,5], zlist:to_list(Z2)).

ciclyc_test() ->
    Z = zlist:seq(1, 3),
    Z2 = zlist:ciclyc(Z),
    {R, _} = zlist:take(10, Z2),
    ?assertEqual([1,2,3,1,2,3,1,2,3,1], R).

take_by_test() ->
    ?assertEqual([[1,2,3],[4,5,6],[7,8]], zlist:to_list(zlist:take_by(3, zlist:seq(1,8)))),
    ?assertEqual([], zlist:to_list(zlist:take_by(3, zlist:empty()))),
    ?assertEqual([[1],[2],[3]], zlist:to_list(zlist:take_by(1, zlist:seq(1,3)))),
    ?assertEqual([[1,2,3]], zlist:to_list(zlist:take_by(4, zlist:seq(1,3)))),
    ZSeq = zlist:seq(1, 30),
    GroupedZ = zlist:take_by(100, ZSeq),
    [_|RestZ] = GroupedZ(),
    ?assertEqual([], RestZ()).

-include_lib("stdlib/include/qlc.hrl").
qlc_test() ->
    Zs = zlist:seq(1, 20),
    Q1 = qlc:q([{C,B,A} || A <- zlist:table(Zs), B <- zlist:table(Zs), C <- zlist:table(Zs),
                           A =< B, A*A + B*B == C*C]),
    Q2 = qlc:sort(Q1, [{order, descending}]),
    Cursor = qlc:cursor(Q2),
    ?assertEqual([{20,16,12},{17,15,8}], qlc:next_answers(Cursor, 2)),
    ?assertEqual([{15,12,9}, {13,12,5},{10,8,6},{5,4,3}], qlc:next_answers(Cursor, 5)), %only 4 available
    ?assertEqual([], qlc:next_answers(Cursor, 1)), %no answers
    ok = qlc:delete_cursor(Cursor).
