/*
Computed entirely by hand.

The operations repeat in blocks very similar instructions, only the lines 5, 6, and 11 vary.
Also, division is either by 1, so no division, or by 26.
If a division by 26 is not performed, there is no chance of the register z becoming 0,
because every step contains a multiplication by 26.

First, simplify the operations.
Let <!=> : (Int, Int) => Int, a <!=> b = if a != b then 1 else 0.
Also, let l_i, m_i, n_i be the variables from the lines 5, 6, and 11, respectively.
Finally, let d_1, ..., d_14 denote the digits of the input number.

Then, in each block of operations one has the following steps

x = (z_(i-1) % 26 + m_i) <!=> d_i

z_i = z_(i-1) / 26 * (25 * x + 1) + (d_i + n_i) * x

Note that when x = 0, n_i is irrelevant.

Now, we can manually compute 14 iterations, where in each case where l_i = 26,
we need to have x = 0, which gives us the equality: d_i = z_(i-1) % 26 + m_i.
Incidentally, each such equality reduces the number of free variables by 1, so in the end we have at most 7 free variables.

I omit the actual equations, because they give insight into the input.
However, the final set of equations contains at most seven variables that are defined in terms of the other seven.
In my case, one such equation was d_i = d_j -8 for some i, j, which has precisely one solution, thus reducing the search space even further.

Finally, going through the equations, one can maximize or minimize each variable as far as possible,
which yields the desired results.
 */
