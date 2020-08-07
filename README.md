[![Build Status](https://travis-ci.org/egobrain/zlist.png?branch=master)](https://travis-ci.org/egobrain/zlist)
[![Coveralls](https://img.shields.io/coveralls/egobrain/zlist.svg)](https://coveralls.io/github/egobrain/zlist)
[![GitHub tag](https://img.shields.io/github/tag/egobrain/zlist.svg)](https://github.com/egobrain/zlist)
[![Hex.pm](https://img.shields.io/hexpm/v/zlist.svg)](https://hex.pm/packages/zlist)

# Erlang zlist: a lazy sequences library.
----------------------------------------------------

## Description ##

Unlike https://github.com/vjache/erlang-zlists
zlist() is a function that returns improper list with data and next zlist or
an empty list.
You can use it to organize lazy evolution or backpressure.

Simple infinite iterator:

```erlang
1> SimpleZ = fun Loop() -> [1|Loop] end.
#Fun<erl_eval.44.90072148>

2> zlist:take(10, SimpleZ).
{[1,1,1,1,1,1,1,1,1,1],#Fun<erl_eval.62.90072148>}
```

## Usage ##

### Integrate to your project ###

This is a rebar'ized project, so, if you are already using rebar, just insert a reference
to this git repo at your rebar.config.

### Examples ###

```erlang
1> Z = zlist:recurrent(fun(A) -> A + 1 end, 0).
#Fun<zlist.14.21462417>

2> zlist:take(10, Z).
{[1,2,3,4,5,6,7,8,9,10],#Fun<zlist.14.21462417>}

3> Z2 = zlist:map(fun(A) -> A * 2 end, Z).
#Fun<zlist.3.21462417>

4> zlist:take(10, Z2).
{[2,4,6,8,10,12,14,16,18,20],#Fun<zlist.3.21462417>}

5> ZL = zlist:from_list([1,2,5]).
#Fun<zlist.12.21462417>

6> zlist:to_list(ZL).
[1,2,5]

7> ZR = zlist:ciclyc(ZL).
#Fun<zlist.17.21462417>

8> zlist:take(10, ZR).
{[1,2,5,1,2,5,1,2,5,1],#Fun<zlist.17.21462417>}

9> Z3 = zlist:flatmap(fun(A) -> zlist:from_list([-A, A]) end, Z2).
#Fun<zlist.6.21462417>

10> zlist:take(10, Z3).
{[-2,2,-4,4,-6,6,-8,8,-10,10],#Fun<zlist.10.21462417>}

```

### Warnings !!! ###

With zlist you are working with potentially infinite data.
Don't use ```to_list/1```, ```foreach/2```, ```fold/3``` functions
until you know that zlist is finite state.
Use ```dropwhen/1``` before, or ```take/2``` instead.

### QLC ###

Zlists supports alternative syntax with QLC query language, example:

```
> Zs = zlist:seq(1, 20),
> Q1 = qlc:q([{C,B,A} || A <- zlist:table(Zs), B <- zlist:table(Zs), C <- zlist:table(Zs),
                       A =< B, A*A + B*B == C*C]),
> Q2 = qlc:sort(Q1, [{order, descending}]),
> Cursor = qlc:cursor(Q2).
> qlc:next_answers(Cursor, 2).
> [{20,16,12},{17,15,8}]
>
> qlc:next_answers(Cursor, 5). %only 4 available
> [{15,12,9}, {13,12,5},{10,8,6},{5,4,3}]
>
> qlc:next_answers(Cursor, 1). %no answers available
> []
```

Use this with care, because it's may make your lazy lists strict.
For another QLC functionality, like cache, sorting, joining etc see http://erlang.org/doc/man/qlc.html
