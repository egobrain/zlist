-module(zlist).

-export([
         map/2,
         filter/2,
         filtermap/2,
         flatmap/2,
         over/3,
         dropwhen/2,
         dropwhile/2,

         append/2,
         ciclyc/1,

         empty/0,
         seq/2,
         seq/3,
         recurrent/2,

         foreach/2,
         fold/3,
         take/2,
         takewhile/2,
         take_by/2,

         from_list/1,
         to_list/1,

         from_ets/2
        ]).

-type zlist(A) :: fun(() -> maybe_improper_list(A, zlist(A))) | empty_zlist().
-type empty_zlist() :: fun(() -> []).

-export_type([
              zlist/1,
              empty_zlist/0
             ]).

%% =============================================================================
%% API functions
%% =============================================================================

-spec empty() -> empty_zlist().
empty() -> fun() -> [] end.

-spec seq(From, To) -> zlist(integer()) when
      From :: integer(),
      To :: integer().
seq(First, Last) when is_integer(First), is_integer(Last), First-1 =< Last ->
    seq_(First, Last).

seq_(Curr, Last) ->
    fun() ->
        case Curr > Last of
            true -> [];
            false -> [Curr] ++ seq_(Curr+1, Last)
        end
    end.

-spec seq(From, To, Incr) -> zlist(integer()) when
      From :: integer(),
      To :: integer(),
      Incr :: integer().
seq(First, Last, Inc) when is_integer(First), is_integer(Last), is_integer(Inc) ->
    if
        Inc > 0, First - Inc =< Last;
        Inc < 0, First - Inc >= Last ->
            N = (Last - First + Inc) div Inc,
            seq_(N, First, Inc);
        Inc =:= 0, First =:= Last ->
            seq_(1, First, Inc)
    end.

seq_(N, X, D) ->
    fun() ->
        case N of
            0 -> [];
            _ -> [X] ++ seq_(N-1, X+D, D)
        end
    end.

-spec map(fun((A) -> B), zlist(A)) -> zlist(B).
map(Fun, Zlist) ->
    fun() ->
        case Zlist() of
            [Data|Next] ->
                [Fun(Data)] ++ map(Fun, Next);
            Done -> Done
        end
    end.

-spec foreach(fun((A) -> ok), zlist(A)) -> ok.
foreach(Fun, Zlist) ->
    case Zlist() of
        [Data|Next] ->
            _ = Fun(Data),
            foreach(Fun, Next);
        _Done -> ok
    end.

-spec filter(fun((A) -> boolean()), zlist(A)) -> zlist(A).
filter(Fun, Zlist) ->
    fun() ->
        case Zlist() of
            [Data|Next] ->
                case Fun(Data) of
                    true -> [Data] ++ filter(Fun, Next);
                    false -> (filter(Fun, Next))()
                end;
            Done -> Done
        end
    end.


-spec filtermap(fun((A) -> {true, B} | false), zlist(A)) -> zlist(B).
filtermap(Fun, Zlist) ->
    fun() ->
        case Zlist() of
            [Data|Next] ->
                case Fun(Data) of
                    {true, Data2} -> [Data2] ++ filtermap(Fun, Next);
                    false -> (filtermap(Fun, Next))()
                end;
            Done -> Done
        end
    end.

-spec fold(fun((A, S) -> S), S, zlist(A)) -> S.
fold(Fun, State, Zlist) ->
    case Zlist() of
        [Data|Next] ->
            fold(Fun, Fun(Data, State), Next);
        _Done -> State
    end.

-spec flatmap(fun((A) -> zlist(B)), zlist(A)) -> zlist(B).
flatmap(Fun, Zlist) ->
    fun() ->
        case Zlist() of
            [Data|Next] ->
                (append(Fun(Data), flatmap(Fun, Next)))();
            Done -> Done
        end
    end.

-spec over(fun((A,S) -> {B, S}), S, zlist(A)) -> zlist(B).
over(Fun, S, Zlist) ->
    fun() ->
        case Zlist() of
            [Data|Next] ->
                {Value, S2} = Fun(Data, S),
                [Value] ++ over(Fun, S2, Next);
            Done -> Done
        end
    end.

-spec dropwhen(fun((A) -> boolean()), zlist(A)) -> zlist(A).
dropwhen(Fun, Zlist) ->
    fun() ->
        case Zlist() of
            [Data|Next] ->
                case Fun(Data) of
                    true -> [];
                    false -> [Data] ++ dropwhen(Fun, Next)
                end;
            Done -> Done
        end
    end.

-spec dropwhile(fun((A) -> boolean()), zlist(A)) -> zlist(A).
dropwhile(Fun, Zlist) ->
    fun() ->
        case Zlist() of
            [Data|Next]=R ->
                case Fun(Data) of
                    true -> (dropwhile(Fun, Next))();
                    false -> R
                end;
            Done -> Done
        end
    end.

-spec append(zlist(A), zlist(B)) -> zlist(A|B).
append(Zlist1, Zlist2) ->
    fun() ->
        case Zlist1() of
            [Data|Next] -> [Data] ++ append(Next, Zlist2);
            _Done -> Zlist2()
        end
    end.

-spec ciclyc(zlist(A)) -> zlist(A).
ciclyc(Zlist) ->
    (fun Loop(Z) ->
        fun() ->
            case Z() of
                [Data|Next] -> [Data] ++ Loop(Next);
                _Done -> (ciclyc(Zlist))()
            end
        end
    end)(Zlist).

-spec from_list([A]) -> zlist(A).
from_list(List) ->
    fun() ->
        case List of
            [H|T] -> [H] ++ from_list(T);
            _ -> List
        end
    end.

-spec to_list(zlist(A)) -> [A].
to_list(Zlist) -> lists:reverse(fold(fun(H, T) -> [H|T] end, [], Zlist)).

%% Be careful with resulted zlist: do not reuse it after a table was unfixed.
%% Consider to use an unsafe ordered_set.
-spec from_ets(ets:tid(), boolean()) -> zlist(tuple()).
from_ets(T, Safe) ->
    Safe andalso ets:safe_fixtable(T, true),
    from_ets(T, ets:first(T), Safe).

-spec recurrent(fun((A) -> A), A) -> zlist(A).
recurrent(Fun, S) ->
    fun() ->
        Next = Fun(S),
        [Next] ++ recurrent(Fun, Next)
    end.

-spec take(N :: pos_integer(), zlist(A)) -> {[A], zlist(A)}.
take(N, Zlist) when N > 0 ->
    take_(N, [], Zlist).
take_(0, Acc, Z) -> {lists:reverse(Acc), Z};
take_(C, Acc, Z) ->
    case Z() of
        [Data|Next] -> take_(C-1, [Data|Acc], Next);
        _Done -> {lists:reverse(Acc), empty()}
    end.

-spec takewhile(fun((A) -> boolean()), zlist(A)) -> {[A], zlist(A)}.
takewhile(Fun, Zlist) -> takewhile_(Fun, [], Zlist).
takewhile_(Fun, Acc, Z) ->
    case Z() of
        [Data|Next] = R ->
            case Fun(Data) of
                true -> takewhile_(Fun, [Data|Acc], Next);
                false -> {lists:reverse(Acc), fun() -> R end}
            end;
        _Done -> {lists:reverse(Acc), empty()}
    end.

-spec take_by(N :: pos_integer(), zlist(A)) -> zlist([A]).
take_by(N, Zlist) when N > 0 ->
    fun() ->
        case take(N, Zlist) of
            {[], EmptyZ} -> EmptyZ();
            {List, RestZ} -> [List] ++ take_by(N, RestZ)
        end
    end.

%% =============================================================================
%% Internal functions
%% =============================================================================

-spec from_ets(ets:tid(), term(), boolean()) -> zlist(tuple()).
from_ets(T, Key, Safe) ->
    fun() ->
        case Key of
            '$end_of_table' ->
                Safe andalso ets:safe_fixtable(T, false),
                [];
            _ ->
                Next = ets:next(T, Key),
                case ets:lookup(T, Key) of
                    [] -> % a key was deleted
                        (from_ets(T, Next, Safe))();
                    Objects ->
                        Objects ++ from_ets(T, Next, Safe)
                end
        end
    end.
