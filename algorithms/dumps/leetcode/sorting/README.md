# Deal with intervals

Intervals are not important to understand an algorithm, but it's probably the most annoying thing when implementing an algorithm. I find I understand how algorithm works, but I can never be confident about the interval choice. Should it starts from 0 or 1? Should it end with r or r - 1? This types of mechanical actions should be able to be speeded up by some analysis and practices.

## Full open interval
- [l, r]: [l, ..., r]
  - ex: [1, 4], [1, 2, 3, 4]
  - length:  r - l + 1
  - l + r - l + 1 = r + 1 => l + lenght = r + 1
- full open interval means element starts from l and end at r.
- the length of open interval is r - l plus 1.

## Full close interval
- (l, r): [l + 1, ..., r - 1]
  - ex: (1, 4), [2, 3]
  - length: r - l - 1
  - l + r - l - 1 = r - 1 => l + lenght = r - 1
- full close interval means element starts from l + 1 and ends at r - 1
- because it doesn't have both end so the lenght is r - l - 1

## Half interval
- [l, r): [l, ..., r - 1]
  - ex: [1, 4), [1, 2, 3)
  - length: r - l
  - l + r - l = r => l + length = r

- (l ,r]: (l + 1, ..., r]
  - ex: (1, 4], [2, 3, 4]
  - length: r - l
  - l + r - l = r => l + length = r

1. two half close intervals are usually interchangeably
2. length of half close interval is just r - l
3. substraction of two numbers gets the close interval.
  - 3 - 2 = 1, it means: 1
    - 1. 3 and 2 have difference
    - 2. |(2, 3]| = 1 or |[2, 3)| = 1 depends on the interpretation.


## Special case on different starting point
- [1, r]
  - ex: [1, 4]. [1, 2, 3, 4]
  - length: r - l + 1 = 4 - 1 + 1 = 4 => |1, r| = r

- (1, r)
  - ex: (1, 4). [2, 3]
  - length: r - l - 1 = 4 - 1 - 1 = 2 => (1, r) = r - 2

- [0, r]
  - ex: [0, ]


## Intervals and boundary

#### Given i in (l, r)
```
for (int i = l + 1; i < r; ++i) ;
```

#### Given i in [l, r]
```
for (int i = l; i <= r; ++i) ;
```

#### Given i in [l, r)
This is the most common case we use
```
for (int i = l; i < r; ++i) ;
```
Note, because it's a half interval, it means length of the array is r - l, if l = 0 the lenght is just r. Because it's right open interval, we never access element at r.

#### Given i in (l, r]
```
for (int i = l + 1; i <= r; ++i) ;
```
This is symmetric with the previous case. |(l, r]| = r - l


## Indices

- If indices are used as boundary, the first thing we deicide is what interval are we using.
- To make the decision we need to know what element should be included, what should not.
- For spliting a consecutive array, right open interval is the most commonly used one.
- For in place operation on an array, close interval is the most commonly used one.
- Open interval is only useful when we want data been strictly restricted in an interval.
- Avoid left open interval because it's equivalent of rigth open interval for most cases, and using two causes inconsistency.


## Imperative programming

- Before writing the loop, first consider what's the loop invariant.
- Think about precondition and post condition.
- Split bit problem into cases. Once a case is clear write it down as comment.

