[![Build Status](https://travis-ci.org/egobrain/zlist.png?branch=master)](https://travis-ci.org/egobrain/zlist.png?branch=master)
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
