# bitwise_interval

PoC about bitwise operations on integer intervals. Given 2 intervals `a` and  `b` (with optional steps), computes the resulting `a & b`,  `a | b`, and  `a ^ b` intervals (and also `~a` and  `~b`) in O(1) regarding the intervals sizes.