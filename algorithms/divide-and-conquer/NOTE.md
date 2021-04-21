### Divide and conquer

breaking things up into smaller problems.

Given a problem of size n divivde problem to n/b, b > 1.

ps: the smartness usually comes in combing small solutions.


### Runing time
T(n) = aT(n/b) + [work of merge]

### Master theorem
Something you can mechanically plug in and get solutions.



### Solving recurrence

Given T(n) = aT(b/n) + c what's the upper bound?
what's O(n)?

##### substition method
1. guess the form (it might be O(n^2))
2. verify by induhction
3. solve for cost

T(n) = 4T(n/2) + n // n^2

Guess: let's instead guess n^3 first.
Assume: T(k) = ck^3 f or k < n
T(n) = 4T(n/2) + n <= 4 * c(n/2)^3 + n
     = 1/2 * c * n^3 + n

     when (1/2 * c * n^3 - n)  >= 0, we have
     cn^3 >= (1/2 * c * n^3 - n)

     this is true when c >= 1, n >= 1


##### recursion tree

T(n) = T(n/4) + T(n/2) + n^2

```
            T(n)
           /    \
       T(n/4)   T(n/2)
      /   \       /  \
T(n/16) T(n/8) T(n/8) T(n/4)
             ...

```

substitute with the constant term

```
            n^2
           /    \
       (n^2)/4   (n^2)/2
      /  \        /    \
(n^2)/16(n^2)/8 (n^2)/8 (n^2)/4
            ...
```

T(n) = n^2 + (n^2)/4 + (n^2)/2 + (n^2)/16 + (n^2)/8 + (n^2)/8 + (n^2)/4 + ...

Expand, simplify.


##### Master theorem

Applies to recurrences of the form
T(n) = aT(n/b) + f(n)

where a >= 1 b > 1, f is asympototically positive

NOTE: n^log(b,a) is the number of leaves in the recursion tree.

```
case 1:
  f(n) = O(n^log(b, a) - e) e > 0
    => T(n) = θ(n^log(b, a))

case 2:
  f(n) = θ(n^log(b, a) (lgn)^k)
    => T(n) = θ(n^log(b, a) (lgn)^k+1)

case 3:
  f(n) = Ω(n^log(b, a) + e)
    & af(n/b) <= (1 - e) f(n) where e > 0
    then T(n) = θ(f(n))
```

