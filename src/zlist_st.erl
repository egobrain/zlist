-module(zlist_st).

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

         foreach/3,
         fold/4,
         take/3,
         takewhile/3,
         take_by/2,

         from_list/1,
         to_list/2
        ]).

-type zlist_st(A, St) ::
        fun((St) ->
            {maybe_improper_list(A, zlist_st(A, St)), St} |
            {[], St}
        ).
-type empty_zlist_st(St) :: fun((St) -> {[], St}).

-export_type([
    zlist_st/2,
    empty_zlist_st/1
]).

%% =============================================================================
%% API functions
%% =============================================================================

-spec empty() -> empty_zlist_st(St :: any()).
empty() ->
    fun(R) -> {[], R} end.

-spec seq(From, To) -> zlist_st(integer(), St :: any()) when
      From :: integer(),
      To :: integer().
seq(First, Last) when is_integer(First), is_integer(Last), First-1 =< Last ->
    seq_(First, Last).

seq_(Curr, Last) ->
    fun(St) ->
        case Curr > Last of
            true -> {[], St};
            false -> {[Curr] ++ seq_(Curr+1, Last), St}
        end
    end.

-spec seq(From, To, Incr) -> zlist_st(integer(), St :: any()) when
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
    fun(St) ->
        case N of
            0 -> {[], St};
            _ -> {[X] ++ seq_(N-1, X+D, D), St}
        end
    end.

-spec map(fun((A) -> B), zlist_st(A, St)) -> zlist_st(B, St).
map(Fun, ZlistSt) ->
    fun(St) ->
        case ZlistSt(St) of
            {[Data|Next], St2} ->
                {[Fun(Data)] ++ map(Fun, Next), St2};
            Done -> Done
        end
    end.

-spec filter(fun((A) -> boolean()), zlist_st(A, St)) -> zlist_st(A, St).
filter(Fun, ZlistSt) ->
    fun(St) ->
        case ZlistSt(St) of
            {[Data|Next], St2} ->
                case Fun(Data) of
                    true -> {[Data] ++ filter(Fun, Next), St2};
                    false -> (filter(Fun, Next))(St2)
                end;
            Done -> Done
        end
    end.

-spec filtermap(fun((A) -> {true, B} | false), zlist_st(A, St)) -> zlist_st(B, St).
filtermap(Fun, ZlistSt) ->
    fun(St) ->
        case ZlistSt(St) of
            {[Data|Next], St2} ->
                case Fun(Data) of
                    {true, Data2} -> {[Data2] ++ filtermap(Fun, Next), St2};
                    false -> (filtermap(Fun, Next))(St2)
                end;
            Done -> Done
        end
    end.

-spec fold(fun((A, S) -> S), S, zlist_st(A, St), St) -> {S, St}.
fold(Fun, State, ZlistSt, St) ->
    case ZlistSt(St) of
        {[Data|Next], St2} ->
            fold(Fun, Fun(Data, State), Next, St2);
        {[], St2} -> {State, St2}
    end.

-spec flatmap(fun((A) -> zlist_st(B, St)), zlist_st(A, St)) -> zlist_st(B, St).
flatmap(Fun, Zlist) ->
    fun(St) ->
        case Zlist(St) of
            {[Data|Next], St2} ->
                (append(Fun(Data), flatmap(Fun, Next)))(St2);
            Done -> Done
        end
    end.

-spec over(fun((A,S) -> {B, S}), S, zlist_st(A, St)) -> zlist_st(B, St).
over(Fun, S, ZlistSt) ->
    fun(St) ->
        case ZlistSt(St) of
            {[Data|Next], St2} ->
                {Value, S2} = Fun(Data, S),
                {[Value] ++ over(Fun, S2, Next), St2};
            Done -> Done
        end
    end.

-spec dropwhen(fun((A) -> boolean()), zlist_st(A, St)) -> zlist_st(A, St).
dropwhen(Fun, ZlistSt) ->
    fun(St) ->
        case ZlistSt(St) of
            {[Data|Next], St2} ->
                case Fun(Data) of
                    true -> {[], St2};
                    false -> {[Data] ++ dropwhen(Fun, Next), St2}
                end;
            Done -> Done
        end
    end.

-spec dropwhile(fun((A) -> boolean()), zlist_st(A, St)) -> zlist_st(A, St).
dropwhile(Fun, ZlistSt) ->
    fun(St) ->
        case ZlistSt(St) of
            {[Data|Next], St2}=R ->
                case Fun(Data) of
                    true -> (dropwhile(Fun, Next))(St2);
                    false -> R
                end;
            Done -> Done
        end
    end.


-spec append(zlist_st(A, St), zlist_st(A, St)) -> zlist_st(A, St).
append(Zlist1, Zlist2) ->
    fun(St) ->
        case Zlist1(St) of
            {[Data|Next], St2} -> {[Data] ++ append(Next, Zlist2), St2};
            {[], St2} -> Zlist2(St2)
        end
    end.

-spec ciclyc(zlist_st(A, St)) -> zlist_st(A, St).
ciclyc(Zlist) ->
    (fun Loop(Zs) ->
        fun(St) ->
            case Zs(St) of
                {[Data|Next], St2} -> {[Data] ++ Loop(Next), St2};
                {[], St2} -> (ciclyc(Zlist))(St2)
            end
        end
    end)(Zlist).

-spec from_list([A]) -> zlist_st(A, St :: any()).
from_list(List) ->
    fun(St) ->
        case List of
            [H|T] -> {[H] ++ from_list(T), St};
            [] -> {List, St}
        end
    end.

-spec to_list(zlist_st(A, St), St) -> {[A], St}.
to_list(Zlist, St) ->
    {List, St2} = fold(fun(H, T) -> [H|T] end, [], Zlist, St),
    {lists:reverse(List), St2}.

-spec recurrent(fun((A) -> A), A) -> zlist_st(A, St :: any()).
recurrent(Fun, S) ->
    fun(St) ->
        Next = Fun(S),
        {[Next] ++ recurrent(Fun, Next), St}
    end.

-spec foreach(fun((A) -> any()), zlist_st(A, St), St) -> St.
foreach(Fun, ZlistSt, St) ->
    case ZlistSt(St) of
        {[Data|Next], St2} ->
            _ = Fun(Data),
            foreach(Fun, Next, St2);
        {[], St2} -> St2
    end.

-spec take(N :: pos_integer(), zlist_st(A, St), St) -> {[A], zlist_st(A, St), St}.
take(N, ZlistSt, St) when N > 0 ->
    take_(N, [], ZlistSt, St).
take_(0, Acc, Zs, St) -> {lists:reverse(Acc), Zs, St};
take_(C, Acc, Zs, St) ->
    case Zs(St) of
        {[Data|Next], St2} -> take_(C-1, [Data|Acc], Next, St2);
        {[], St2} -> {lists:reverse(Acc), empty(), St2}
    end.

-spec takewhile(fun((A) -> boolean()), zlist_st(A, St), St) -> {[A], zlist_st(A, St), St}.
takewhile(Fun, Zlist, St) -> takewhile_(Fun, [], Zlist, St).
takewhile_(Fun, Acc, Zs, St) ->
    case Zs(St) of
        {[Data|Next], St2} ->
            case Fun(Data) of
                true -> takewhile_(Fun, [Data|Acc], Next, St2);
                false ->
                    NextZs = fun(S) ->
                        {[Data] ++ Next, S}
                    end,
                    {lists:reverse(Acc), NextZs, St2}
            end;
        {[], St2} -> {lists:reverse(Acc), empty(), St2}
    end.

-spec take_by(N :: pos_integer(), zlist_st(A, St)) -> zlist_st([A], St).
take_by(N, Zlist) when N > 0 ->
    fun(St) ->
        case take(N, Zlist, St) of
            {[], _EmptyZ, St2} -> {[], St2};
            {List, RestZ, St2} -> {[List] ++ take_by(N, RestZ), St2}
        end
    end.
