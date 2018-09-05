#!/usr/bin/env python

## generate sexps of specified size

import sys, random, string

max_atom_size = 32

def atom(size):
    assert size >= 1
    initial = random.choice(string.ascii_letters)
    alnum = string.ascii_letters + string.digits
    return initial + ''.join(random.choice(alnum)
                             for _ in range(size - 1))

def whitespace(size):
    return ''.join(random.choice('\t\n ')
                   for _ in range(size))

def rand_divide_stream(x):
    while x >= 0:
        if x == 1:
            yield x
            return
        elif x == 0:
            return
        else:
            i = random.randint(2,x)
            x -= i
            yield i

def rand_division(x):
    l = list(rand_divide_stream(x))
    random.shuffle(l)
    return l

def sexp(size):
    assert size <> 0
    if size < max_atom_size: return atom(size)
    else:
        division = rand_division(size - 2)
        v = ['(']
        for d in division:
            if d > 1:
                wslen = min(5, random.randint(1,d - 1))
                v.append(sexp(d - wslen))
                v.append(whitespace(wslen))
            else:
                v.append(atom(1))
        v.append(')')
        return ''.join(v)

if __name__ == '__main__':
    sys.stdout.write(sexp(int(sys.argv[1])))
