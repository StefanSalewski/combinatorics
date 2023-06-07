# Module combinatorics
# v0.1 2023-JUN-07
# (c) S. Salewski 2023

##[

A few procedures and iterators to generate permutations, combinations and such.
Similar functionality is availabvle from the algorithm module of Nim's
standard library or by external modules like itertools on GitHub.

]##

from std/algorithm import reverse, reversed, isSorted, sort, sorted, SortOrder
from std/sequtils import toSeq, deduplicate, mapIt
from std/strutils import join

proc isEven(i: int): bool =
  (i and 1) == 0

# To generate lexicographically ordered permutations we can use procs nextPermutation() and prevPermutation().
# nextPermutation() is typically invoked first with a sorted argument, and then generates the next permutation.
# prevPermutation() is typically invoked first with an invers sorted argument, and then generates the previous permutation.
# Both functions return false when called with the final permutation (largest/smallest order).
# The C++ Standard Template Library (STL) provides these functions as well, and variants exist in std/algorithm.

#[
https://en.wikipedia.org/wiki/Permutation#Generation_in_lexicographic_order
The following algorithm generates the next permutation lexicographically after a given permutation. It changes the given permutation in-place.
- Find the largest index k such that a[k] < a[k + 1]. If no such index exists, the permutation is the last permutation.
- Find the largest index l greater than k such that a[k] < a[l].
- Swap the value of a[k] with that of a[l].
- Reverse the sequence from a[k + 1] up to and including the final element a[n].
]#

proc nextPermutation*[T](a: var openArray[T]): bool =
  ## Calculates the next lexicographic permutation, directly modifying `a`.
  ## The result is whether a permutation happened, otherwise we have reached
  ## the last ordered permutation.
  ##
  ## If you start with an unsorted array/seq, the repeated permutations
  ## will **not** give you all permutations but stop with the last.
  var k = a.high - 1
  while k >= 0 and a[k] >= a[k + 1]:
    dec(k)
  if k >= 0:
    var l = a.high
    while a[k] >= a[l]:
      dec(l)
    swap(a[k], a[l])
    a.reverse(k + 1, a.high)
    return true

proc prevPermutation*[T](a: var openArray[T]): bool =
  ## Calculates the previous lexicographic permutation, directly modifying `a`.
  ## The result is whether a permutation happened, otherwise we have reached
  ## the first ordered permutation.
  ##
  ## If you start with an unsorted array/seq, the repeated permutations
  ## will **not** give you all permutations but stop with the first.
  var k = a.high - 1
  while k >= 0 and a[k] <= a[k + 1]:
    dec(k)
  if k >= 0:
    var l = a.high
    while a[k] <= a[l]:
      dec(l)
    swap(a[k], a[l])
    a.reverse(k + 1, a.high)
    return true

# The final Nim iterator
iterator orderedPermutations*[T](input: openArray[T]): seq[T] =
  ## All the permutations created by nextPermutation()
  var a = input.sorted
  while true:
    yield a
    if not nextPermutation(a):
      break

iterator reversedPermutations*[T](input: openArray[T]): seq[T] =
  ## All the permutations created by prevPermutation()
  var a = input.sorted(order = Descending)
  while true:
    yield a
    if not prevPermutation(a):
      break

# Heap's_algorithm for generating unordered permutations
# We start with a recursive version which only prints the results.
# Then we modify that version to generate a seq with all the permutations.
# Note, Nim does not support recursive iterators!
# https://en.wikipedia.org/wiki/Heap's_algorithm
proc permutation[T](a: var openArray[T]; k: int) =
  if k == 1:
    echo a
  else:
    # Generate permutations with k-th unaltered
    # Initially k = length(a)
    permutation(a, k - 1)
    # Generate permutations for k-th swapped with each k-1 initial
    for i in 0 ..< k - 1:
      # Swap choice dependent on parity of k (even or odd)
      if k.isEven:
        swap(a[i], a[k - 1]) # zero-indexed, the k-th is at k-1
      else:
        swap(a[0], a[k - 1])
      permutation(a, k - 1)

proc recPerms[T](a: var openArray[T]; k: int; res: var seq[seq[T]]) =
  if k == 1:
    res.add(@a)
  else:
    # Generate permutations with k-th unaltered
    # Initially k = length(a)
    recPerms(a, k - 1, res)
    # Generate permutations for k-th swapped with each k-1 initial
    for i in 0 ..< k - 1:
      # Swap choice dependent on parity of k (even or odd)
      if k.isEven:
        swap(a[i], a[k - 1]) # zero-indexed, the k-th is at k-1
      else:
        swap(a[0], a[k - 1])
      recPerms(a, k - 1, res)

proc recursiveUnorderedPermutations*[T](a: openArray[T]): seq[seq[T]] =
  ## Returns a seq of all unordered permutations created by a recursive version of
  ## Heap's_algorithm. Typically, we would use the permutations() iterator instead.
  var a = @a
  recPerms(a, a.len, result)

# https://en.wikipedia.org/wiki/Heap's_algorithm
# Non-recursive version: Sedgewick, Robert (4 June 2020)
# https://sedgewick.io/wp-content/uploads/2022/03/2002PermGeneration.pdf
proc generate[T](a: var openArray[T]; n: int) =
  # c is an encoding of the stack state. c[k] encodes the for-loop counter for when generate(k - 1, A) is called
  var c: array[32, int]
  assert(n <= 32)
  echo a
  # i acts similarly to a stack pointer
  var i = 1
  while i < n:
    if c[i] < i:
      if i.isEven:
        swap(a[0], a[i])
      else:
        swap(a[c[i]], a[i])
      echo a
      # Swap has occurred ending the for-loop. Simulate the increment of the for-loop counter
      c[i] += 1
      # Simulate recursive call reaching the base case by bringing the pointer to the base case analog in the array
      i = 1
    else:
      # Calling generate(i+1, A) has ended as the for-loop terminated. Reset the state and simulate popping the stack by incrementing the pointer.
      c[i] = 0
      i += 1

# The final Nim iterator
iterator permutations*[T](input: openArray[T]): seq[T] =
  ## Unordered permutations generated by a non-recursive version of Heap's_algorithm.
  # c is an encoding of the stack state. c[k] encodes the for-loop counter for when generate(k - 1, A) is called
  var c :array[32, int]
  var a: seq[T] = @input
  assert(input.len <= 32)
  yield a
  # i acts similarly to a stack pointer
  var i = 1
  while i < input.len:
    if c[i] < i:
      if i.isEven:
        swap(a[0], a[i])
      else:
        swap(a[c[i]], a[i])
      yield a
      # Swap has occurred ending the for-loop. Simulate the increment of the for-loop counter
      c[i] += 1
      # Simulate recursive call reaching the base case by bringing the pointer to the base case analog in the array
      i = 1
    else:
      # Calling generate(i+1, A) has ended as the for-loop terminated. Reset the state and simulate popping the stack by incrementing the pointer.
      c[i] = 0
      i += 1

# Tests for the permutations
when isMainModule:
  import std/times
  block:
    const input = "1234"
    var s = toSeq(permutations(input))
    assert(s.len == 24)
    var s1, s2, s3, s4: seq[string]
    for el in s: # seq[seq[char]] to seq[string] for easy comparisons
      s1.add((el.mapIt($it)).join)
    assert(s1.len == 24)
    s1.sort
    assert s1.deduplicate(isSorted = true).len == 24

    s = recursiveUnorderedPermutations(input)
    assert(s.len == 24)
    for el in s:
      s2.add((el.mapIt($it)).join)
    assert(s2.len == 24)
    s2.sort
    assert s2.deduplicate(isSorted = true).len == 24
    assert s1 == s2

    s = toSeq(orderedPermutations(input))
    assert(s.len == 24)
    for el in s:
      s3.add((el.mapIt($it)).join)
    assert(s3 == s1)

    s = toSeq(reversedPermutations(input))
    assert(s.len == 24)
    for el in s:
      s4.add((el.mapIt($it)).join)
    assert(s4.reversed == s1)

  echo "Ordered:"
  for el in orderedPermutations([1, 2, 3]):
    echo "=> ", el
  echo "--"
  for el in orderedPermutations([1, 2, 2]):
    echo "=> ", el

  echo "Reversed"
  for el in reversedPermutations(["A", "B", "C"]):
    echo "=> ", el
  echo "--"
  for el in reversedPermutations(["A", "B", "A"]):
    echo "=> ", el

  echo "Unordered, Heap's_algorithm"
  for el in permutations("abc"):
    echo "=> ", el
  echo "--"
  for el in permutations("aba"):
    echo "=> ", el

  echo "Iteration times for permutations of [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]"
  let input = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
  const nres = 3628800
  block:
    var n: int
    let start = cpuTime()
    for el in orderedPermutations(input):
      inc(n)
    let stop = cpuTime()
    assert n == nres
    echo "Time for orderedPermutations: ", stop - start

  block:
    var n: int
    let start = cpuTime()
    for el in reversedPermutations(input):
      inc(n)
    let stop = cpuTime()
    assert n == nres
    echo "Time for reversedPermutations: ", stop - start

  block:
    var n: int
    let start = cpuTime()
    for el in permutations(input):
      inc(n)
    let stop = cpuTime()
    assert n == nres
    echo "Time for permutations: ", stop - start


