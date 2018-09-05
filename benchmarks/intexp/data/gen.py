#!/usr/bin/env python

## generate intexps of specified size

import sys, random, string

keywords = ('let', 'in', 'if', 'then', 'else')

max_atom_size = 18

def variable(size):
    assert size >= 1
    initial = random.choice(string.lowercase)
    alnum = string.ascii_letters + string.digits
    cand = initial + ''.join(random.choice(alnum)
                             for _ in range(size - 1))
    return cand if cand not in keywords else variable(size)
    

def integer(size):
    assert size >= 1
    return ''.join(random.choice(string.digits) for _ in range(size))

def whitespace(size):
    return ''.join(random.choice('\t\n ') for _ in range(size))

def binexp(env, op, size):
    size -= 5
    lsize = random.randrange(1,size-1)
    rsize = size - lsize
    return ''.join(('(', exp(env, lsize), ')', op, '(', exp(env, rsize), ')'))

def exp(env, size):
    if size <= max_atom_size:
        env_ = [x for x in env if len(x) == size]
        if random.choice((False,True)):
            if env_: return random.choice(env)
            else: return integer(size)
        else: return integer(size)
    else:
        ty = random.choice(('+', '-', '*', '=', 'int', 'let', 'if'))
        if ty in ('+', '-', '*', '='): return binexp(env, ty, size)
        elif ty == 'int':
            if size > max_atom_size: return exp(env, size)
            else: return integer(size)
        elif ty == 'let':
            if size < 20: return exp(env, size)
            size -= 11
            varsize = random.randrange(1,min(5, size - 3))
            size -= varsize
            contsize = random.randrange(1, size - 3)
            size -= contsize
            rhssize = size
            var = variable(varsize)
            rhs = exp(env, rhssize)
            cont = exp([var] + env, contsize)
            return ''.join(('let ', var, ' = ', rhs, ' in ',  cont))
        elif ty == 'if':
            if size < 20: return exp(env, size)
            size -= 15
            condsize = random.randrange(1, size - 2)
            size -= condsize
            elsesize = random.randrange(1, size - 1)
            size -= elsesize
            thensize = size
            return ''.join(('if ', exp(env, condsize), ' then ', exp(env, thensize), ' else ',  exp(env, elsesize)))
        else: assert False
            
if __name__ == '__main__':
    sys.stdout.write(exp([], int(sys.argv[1])))
