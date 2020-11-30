# REC-operational-semantics
 Implementation of the call-by-value operational semantics of a recursive language REC

**command syntax:** interp [list-of-variables] [list-of-functions] program


## Test examples:

---

**example1.txt** if (z) then x+y else 0

**example2.txt** Recursive function f1(x) = f(x)+1

**example3.txt** Function f2(f(1(x)) = 1 that illustrates call-by-value function evaluation

**example4.txt** Fibonnaci fib(x) = fib(x-1) + fib(x-2)