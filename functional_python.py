#!/usr/bin/env python
"""Calculate the Newton-Raphson Square Roots

Tests:

def sqrt_1(f):
    def fun(app):
        return f(app, 2)
    return fun

f_sq = lambda app: limit(app, 2)
f_sq_2 = sqrt_1(limit)
"""

import math
from collections import namedtuple


def limit(app, rac):
    return (app + rac/app)/2

def epsilon_haskell(s, a, e):
    r = limit(a, s)
    if abs(a - r) < e:
        return r
    else:
        return epsilon_haskell(s, r, e)

def sqrt_haskell(n, app=1, e=0.00001):
    return epsilon_haskell(n, app, e)

# Next try is like on the book Function Python Programming
# I really can not understand the need of having the repeat
# function generator.

def repeat(f, a):
    yield f(a)
    yield from repeat(f, f(a))

def epsilon(gen, e):
    def iterate(gen, e, curr):
        b = next(gen)
        if abs(curr-b) < e:
            return b
        return iterate(gen, e, b)
    return iterate(gen, e, next(gen))

def sqrt(n, app=1, e=0.00001):
    return epsilon(repeat(lambda app: limit(app, n), app), e)


# Prime Number

def isprime(n):
    def isprime(k, coprime):
        """Is k relatively prime to the value coprime?"""
        if k < coprime*coprime:
            return True
        if k % coprime == 0:
            return False
        return isprime(k, coprime+2)
    if n < 2:
        return False
    if n == 2:
        return True
    if n % 2 == 0:
        return False
    return isprime(n, 3)

# Tail recursion
def isprime_tco(p):
    if p < 2:
        return False
    if p == 0:
        return True
    if p % 2 == 0:
        return False
    return not any(p==0 for p in range(3, int(math.sqrt(p))+1, 2))

def my_isprime_tco(n):
        if n < 2:
            return False
        if n == 0:
            return True
        if n % 2 == 0:
            return False

        coprime = 3
        while True:
            if n < coprime*coprime:
                return True
            if n % coprime == 0:
                    return False

            coprime += 2

def primes(n, m):
    if n > m:
        return []
    if not my_isprime_tco(n):
        return primes(n+1, m)
    else:
        return [n] + primes(n+1, m)

def isprime_gen(n):
        if n < 2:
            yield False
        if n == 0:
            yield True
        if n % 2 == 0:
            yield False

        coprime = 3
        while True:
            if n < coprime*coprime:
                yield True
            if n % coprime == 0:
                    yield False

            coprime += 2

def primes_tco(n, m):
    """This is the best solution. It uses a generator"""
    result = []
    for i in range(n, m+1):
        if next(isprime_gen(i)):
            print(i)
            result.append(i)
    return result

# removes $ and , from string
def remove(word, chars):
    if chars:
        return remove(word.replace(chars[0], ""), chars[1:])
    return word

def remove_h(xs, c):
    return "".join(s for s in xs if s!=c)

def norm_num(xs, xc):
    if not xc:
        return xs
    return norm_num(remove_h(xs, xc[0]), xc[1:])

def norm_num_tco(xs, xc):
    while True:
        if not xc:
            return xs
        xs = remove_h(xs, xc[0])
        xc = xc[1:]

# Try to define datastructure with a nametupled
Leaf = namedtuple('Leaf', 'value')
Node = namedtuple('Node', 'left value right')

t = Node(Node(Leaf(1), 3, Leaf(4)), 5, Node(Leaf(6), 7, Leaf(9)))

def occurs(m, t):
    if isinstance(t.value, int) and t.value == m:
        return True
    else:
        if hasattr(t, 'left') and hasattr(t, 'right'):
            return occurs(m, t.left) or occurs(m, t.right)
        elif hasattr(t, 'left') and not hasattr(t, 'right'):
            return occurs(m, t.left) or False
        elif not hasattr(t, 'left') and hasattr(t, 'right'):
            return False or occurs(m, t.right)
        elif not hasattr(t, 'left') and not hasattr(t, 'right'):
            return False

def flaten(t):
    if isinstance(t.value, int):
        if hasattr(t, 'left') and hasattr(t, 'right'):
            return flaten(t.left) + [t.value] + flaten(t.right)
        elif hasattr(t, 'left') and not hasattr(t, 'right'):
            return flaten(t.left)
        elif not hasattr(t, 'left') and hasattr(t, 'right'):
            return flaten(t.right)
        elif not hasattr(t, 'left') and not hasattr(t, 'right'):
            return [t.value]

