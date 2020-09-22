#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wint-in-bool-context"
/*
 * CS:APP Data Lab 
 * 
 * Yuan Tong (2018302110332)
 *
 * bits.c - Source file with your solutions to the Lab.
 *          This is the file you will hand in to your instructor.
 *
 * WARNING: Do not include the <stdio.h> header; it confuses the dlc
 * compiler. You can still use printf for debugging without including
 * <stdio.h>, although you might get a compiler warning. In general,
 * it's not good practice to ignore compiler warnings, but in this
 * case it's OK.
 */

/*
 * Answer code is licensed under MIT License.
 *
 * MIT License
 *
 * Copyright (c) 2020 Yuan Tong (tongyuan200097@gmail.com, yuan.tong@whu.edu.cn)
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

/*
 * Explanation to the code is licensed under the
 * Creative Commons Attribution-ShareAlike 4.0 International License.
 * To view a copy of this license, visit http://creativecommons.org/licenses/by-sa/4.0/
 * or send a letter to Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.
 */

/*
 * Other part of the code shall belongs to their corresponding license.
 * Consult CS:APP author for more info about the license of these code.
 */

#if 0
/*
 * Instructions to Students:
 *
 * STEP 1: Read the following instructions carefully.
 */

You will provide your solution to the Data Lab by
editing the collection of functions in this source file.

INTEGER CODING RULES:

  Replace the "return" statement in each function with one
  or more lines of C code that implements the function. Your code
  must conform to the following style:

  int Funct(arg1, arg2, ...) {
      /* brief description of how your implementation works */
      int var1 = Expr1;
      ...
      int varM = ExprM;

      varJ = ExprJ;
      ...
      varN = ExprN;
      return ExprR;
  }

  Each "Expr" is an expression using ONLY the following:
  1. Integer constants 0 through 255 (0xFF), inclusive. You are
      not allowed to use big constants such as 0xffffffff.
  2. Function arguments and local variables (no global variables).
  3. Unary integer operations ! ~
  4. Binary integer operations & ^ | + << >>

  Some of the problems restrict the set of allowed operators even further.
  Each "Expr" may consist of multiple operators. You are not restricted to
  one operator per line.

  You are expressly forbidden to:
  1. Use any control constructs such as if, do, while, for, switch, etc.
  2. Define or use any macros.
  3. Define any additional functions in this file.
  4. Call any functions.
  5. Use any other operations, such as &&, ||, -, or ?:
  6. Use any form of casting.
  7. Use any data type other than int.  This implies that you
     cannot use arrays, structs, or unions.


  You may assume that your machine:
  1. Uses 2s complement, 32-bit representations of integers.
  2. Performs right shifts arithmetically.
  3. Has unpredictable behavior when shifting an integer by more
     than the word size.

EXAMPLES OF ACCEPTABLE CODING STYLE:
  /*
   * pow2plus1 - returns 2^x + 1, where 0 <= x <= 31
   */
  int pow2plus1(int x) {
     /* exploit ability of shifts to compute powers of 2 */
     return (1 << x) + 1;
  }

  /*
   * pow2plus4 - returns 2^x + 4, where 0 <= x <= 31
   */
  int pow2plus4(int x) {
     /* exploit ability of shifts to compute powers of 2 */
     int result = (1 << x);
     result += 4;
     return result;
  }

FLOATING POINT CODING RULES

For the problems that require you to implent floating-point operations,
the coding rules are less strict.  You are allowed to use looping and
conditional control.  You are allowed to use both ints and unsigneds.
You can use arbitrary integer and unsigned constants.

You are expressly forbidden to:
  1. Define or use any macros.
  2. Define any additional functions in this file.
  3. Call any functions.
  4. Use any form of casting.
  5. Use any data type other than int or unsigned.  This means that you
     cannot use arrays, structs, or unions.
  6. Use any floating point data types, operations, or constants.


NOTES:
  1. Use the dlc (data lab checker) compiler (described in the handout) to
     check the legality of your solutions.
  2. Each function has a maximum number of operators (! ~ & ^ | + << >>)
     that you are allowed to use for your implementation of the function.
     The max operator count is checked by dlc. Note that '=' is not
     counted; you may use as many of these as you want without penalty.
  3. Use the btest test harness to check your functions for correctness.
  4. Use the BDD checker to formally verify your functions
  5. The maximum number of ops for each function is given in the
     header comment for each function. If there are any inconsistencies
     between the maximum ops in the writeup and in this file, consider
     this file the authoritative source.

/*
 * STEP 2: Modify the following functions according the coding rules.
 *
 *   IMPORTANT. TO AVOID GRADING SURPRISES:
 *   1. Use the dlc compiler to check that your solutions conform
 *      to the coding rules.
 *   2. Use the BDD checker to formally verify that your solutions produce
 *      the correct answers.
 */


#endif
/* Copyright (C) 1991-2018 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */
/* This header is separate from features.h so that the compiler can
   include it implicitly at the start of every compilation.  It must
   not itself include <features.h> or any other header that includes
   <features.h> because the implicit include comes before any feature
   test macros that may be defined in a source file before it first
   explicitly includes a system header.  GCC knows the name of this
   header in order to preinclude it.  */
/* glibc's intent is to support the IEC 559 math functionality, real
   and complex.  If the GCC (4.9 and later) predefined macros
   specifying compiler intent are available, use them to determine
   whether the overall intent is to support these features; otherwise,
   presume an older compiler has intent to support these features and
   define these macros by default.  */
/* wchar_t uses Unicode 10.0.0.  Version 10.0 of the Unicode Standard is
   synchronized with ISO/IEC 10646:2017, fifth edition, plus
   the following additions from Amendment 1 to the fifth edition:
   - 56 emoji characters
   - 285 hentaigana
   - 3 additional Zanabazar Square characters */
/* We do not support C11 <threads.h>.  */
/*
 * bitAnd - x&y using only ~ and |
 *   Example: bitAnd(6, 5) = 4
 *   Legal ops: ~ |
 *   Max ops: 8
 *   Rating: 1
 */
int bitAnd(int x, int y) {
  // 4 ops. De Morgan's laws is our friend.

  //   x & y
  // = !!(x & y)
  // = !(!x || !y)

  return ~((~x) | (~y));
}
/*
 * getByte - Extract byte n from word x
 *   Bytes numbered from 0 (LSB) to 3 (MSB)
 *   Examples: getByte(0x12345678,1) = 0x56
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 6
 *   Rating: 2
 */
int getByte(int x, int n) {
  // 3 ops. Easy.

  // 1 byte is 8 bit, so multiply n by 8 (n << 3) to get shift step.
  // Then shift x right to lowest byte, and mask out other stuffs.

  return (0xff & (x >> (n << 3)));
}
/*
 * logicalShift - shift x to the right by n, using a logical shift
 *   Can assume that 0 <= n <= 31
 *   Examples: logicalShift(0x87654321,4) = 0x08765432
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 20
 *   Rating: 3
 */
int logicalShift(int x, int n) {
  // 8 ops. I **REALLY** want subtract.

  // The most straight approach is to mask out dangling sign when shifting.
  // But generate mask it self also need logical right shift 0xffffffff, so we encountered a loop.
  // However for masking, we can generate it in another way: left shift 1 and minus 1.
  // eg. (1 << 5) - 1 = 0b11111, to make low t bit 1, we need to shift left t...
  // Well, for most cases. This approach doesn't work when t is 32, because left shift 32 is UB,
  // and many CPUs refuse to give intuitive result - clear out bits and return 0.
  // Anyway, we can always do it manually. when t is 32, we want a mask 0xffffffff to preserve all the bits.
  // If we can set all bits when t is 32, and keep all bits unchanged unless, we are done.
  // Noticing that t never go beyond 32, so if we shift t right 5 bits, then it will be 1 for 32 and 0 for other.
  // To set everything conditionally, we need OR, so our goal is 0xffffffff(-1) for m = 32 and 0x0 unless.
  // And everything connected. just negate (t >> 5), OR the mask, then AND the arithmetic shift result.
  // Let's write it down.

//  int t = 32 - n;
//  return (x >> n) & ((((1 << t) - 1)) | (-(t >> 5)));

  // Still not finished, because we don't have subtract. But '-a' can be rewritten as '~a + 1', so by using more
  // operators, we can finish the task.

  // 11 ops, 3 more just for workaround about subtract :(

  int t = 33 + (~n);
  return (x >> n) & ((((1 << t) + (~0))) | (((~(t >> 5))) + 1));

  // However, there's always concerns about UBs, some machines cry for that,
  // and some people are just paranoids. So let's make a UB-free approach.

  // An easy but dirty method goes below. (1 << t) - 1 can be anything when t = 32, so we simply reuse r to add it to t.
  // this is equivalent to minus 1 from t when t is 32, prevented left shift 32.

  // 12 ops. One more.
//   int t = 33 + (~n);
//   int r = ((~(t >> 5))) + 1;
//   t += r;
//   return (x >> n) & ((((1 << t) + (~0))) | r);

  // But I want to try a different way. If we know the number is positive or negative, we can do
  // both cases easily. So let's do both. In positive case arithmetic and logical shift is equivalent.
  // In negative case, we shift sign and digits differently. For digits, just mask out sign bit.
  // For sign, we can shift 1 from another side, and this works amazingly - bits to shift always falls into valid range.
  // Then we build a binary selector to emulate condition. The selector should be 0xffffffff for one case
  // and 0x0 for another. Then use formula (f_case & selector) | (0_case & ~selector) the work is done.

  // 14 ops, Expensive, but interesting.
//    int resultPositive = x >> n;
//    int resultNegative = ((x & ~(1 << 31)) >> n) | (1 << (32 + (~n)));
//    int resultSelector = x >> 31;
//    return (resultPositive & ~resultSelector) | (resultNegative & resultSelector);
}
/*
 * bitCount - returns count of number of 1's in word
 *   Examples: bitCount(5) = 2, bitCount(7) = 3
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 40
 *   Rating: 4
 */
int bitCount(int x) {
  // __builtin_popcount by GCC, standardized in C++20 as std::popcount. Even got hardware opcode.

  // Classic question has classic solution.
  // We treat the x as a packed 32x1bit vector, then do a SIMD-like operation to add nearby numbers.
  // Specifically, shift x to make even bits to odd places, and add the even masked version together.
  // After that we got a packed 16x2bit vector, and do the same thing again, except now even bits
  // should changed to even positions. Repeatedly do this until we got a single 1x32bit vector,
  // and we are done.
  // Following code do the equivalent, but optimized to use less ops.

  // 30 ops,  17 if we can use all constants
  int n_0x1111 = (0x11 << 8) | 0x11;
  int n_0x11111111 = (n_0x1111 << 16) | n_0x1111;
  int n_0x33333333 = (n_0x11111111 << 1) + n_0x11111111;
  int n_0x55555555 = (n_0x11111111 << 2) + n_0x11111111;
  int n_0x0f0f = (0x0f << 8) | 0x0f;
  int n_0x0f0f0f0f = (n_0x0f0f << 16) | n_0x0f0f;

  x = (x & n_0x55555555) + ((x >> 1) & (n_0x55555555));
  x = (x & n_0x33333333) + ((x >> 2) & (n_0x33333333));
  x = ((x + (x >> 4)) & n_0x0f0f0f0f);
  x = x + (x << 8) + (x << 16) + (x << 24);

  return x >> 24;
}
/*
 * bang - Compute !x without using !
 *   Examples: bang(3) = 0, bang(0) = 1
 *   Legal ops: ~ & ^ | + << >>
 *   Max ops: 12
 *   Rating: 4
 */
int bang(int x) {
  // At least one of x and -x is negative, unless x is 0.
  // OR x and -x, we got sign bit 1 for !0 and 0 for 0.
  // Then we right shift to spread the sign.
  // For !0, we got 0xffffffff, for 0, we got 0x0.
  // Add 1, and we are done.

  // 5 ops
  return ((x | (~x + 1)) >> 31) + 1;
}
/*
 * tmin - return minimum two's complement integer
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 4
 *   Rating: 1
 */
int tmin(void) {
  // No comment.

  // 1 op. Why here? I already used it before.
  return 1 << 31;
}
/*
 * fitsBits - return 1 if x can be represented as an
 *  n-bit, two's complement integer.
 *   1 <= n <= 32
 *   Examples: fitsBits(5,3) = 0, fitsBits(-4,3) = 1
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 15
 *   Rating: 2
 */
int fitsBits(int x, int n) {
  // Check if 0 <= i < 2^n is easy. We just shift right to clear low bits,
  // and check if the left is 0.
  // So we add 2^(n-1) to x to make smallest representable negative number 0,
  // and biggest representable positive number 2^n - 1.
  // But this approach doesn't work when n is 32 - however, anything we got should
  // fit in 32 bit, so just blindly return 1 is ok.
  // 8 ops.
//  int t = (x + (1 << (n + (~0)))) >> n;
//  return ((((t | (~t + 1)) >> 31) & 1) ^ 1) | (n >> 5);
  return (!((x + (1 << (n + (~0)))) >> n)) | (n >> 5);
}

/*
 * divpwr2 - Compute x/(2^n), for 0 <= n <= 30
 *  Round toward zero
 *   Examples: divpwr2(15,1) = 7, divpwr2(-33,4) = -2
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 15
 *   Rating: 2
 */
int divpwr2(int x, int n) {
  // right arithmetic shift do what we want. Well, sort of.
  // We need to change round rule For negative case.
  // That is, if there's remainder, we add 1 to result.
  // We left shift to make only remainder left, if that's not 0, then remainder exist.
  // But as what happened in logicalShift, left shift 32 doesn't work as excepted, so we explicit
  // detect if n is 0. If both n and remainder is not 0, and we are dealing with a negative,
  // then add 1 to result.

  // 11 ops.
  return (x >> n) + ((!((!(x << (33 + (~n)))) | (!n))) & (x >> 31));
}
/*
 * negate - return -x
 *   Example: negate(1) = -1.
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 5
 *   Rating: 2
 */
int negate(int x) {
  // Trivial.

  // 2 ops. Really, why ask me now after I used before??
  return (~x) + 1;
}
/*
 * isPositive - return 1 if x > 0, return 0 otherwise
 *   Example: isPositive(-1) = 0.
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 8
 *   Rating: 3
 */
int isPositive(int x) {
  // It's easy to check if x >= 0 and if x != 0. Combine them together.

  // 4 ops.
  return !((x >> 31) | (!x));
}
/*
 * isLessOrEqual - if x <= y  then return 1, else return 0
 *   Example: isLessOrEqual(4,5) = 1.
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 24
 *   Rating: 3
 */
int isLessOrEqual(int x, int y) {
  // We can split LessOrEqual into less and equal.
  // For equal, XOR is more reliable and suitable than arithmetic operations to check identity, so let's use it.
  // For less, if x and y have same sign, then a subtract tells everything in its sign bit.
  // If they have different signs, then check x's sign: A negative is always less than a positive.
  // Combine the result in different cases, and we are done.

  // 13 ops.
  int xor = x ^ y;
  int different = xor >> 31;
  return (!xor) | (((((x + (~y) + 1) & (~different)) | (x & different)) >> 31) & 1);
}
/*
 * ilog2 - return floor(log base 2 of x), where x > 0
 *   Example: ilog2(16) = 4
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 90
 *   Rating: 4
 */
int ilog2(int x) {
  // __builtin_clz by GCC, standardized in C++20 as std::countl_zero
  // Again a classic question. We use bisection method.
  // First detect if we have 16 zeros, if there is, trim it.
  // Then 8, 4, 2, 1, and we are done.

  // 25 ops.
  int q;
#pragma clang diagnostic push
#pragma ide diagnostic ignored "Simplify"
  int r = (!!(x >> 16)) << 4; x = x >> r;
  q = (!!(x >>  8)) << 3; x = x >> q; r = r | q;
  q = (!!(x >>  4)) << 2; x = x >> q; r = r | q;
  q = (!!(x >>  2)) << 1; x = x >> q; r = r | q;
  r |= (x >> 1);
#pragma clang diagnostic pop

  return r;
}
/*
 * float_neg - Return bit-level equivalent of expression -f for
 *   floating point argument f.
 *   Both the argument and result are passed as unsigned int's, but
 *   they are to be interpreted as the bit-level representations of
 *   single-precision floating point values.
 *   When argument is NaN, return argument.
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 10
 *   Rating: 2
 */
unsigned float_neg(unsigned uf) {
  // That is mainly detecting nan.
  // First strip out sign, then nan is bigger than anything other in int representation.
  // And what's left is easy.

  // 3 ops.
  return ((uf & 0x7fffffff) > 0x7f800000) ? uf : (uf ^ 0x80000000);
}
/*
 * float_i2f - Return bit-level equivalent of expression (float) x
 *   Result is returned as unsigned int, but
 *   it is to be interpreted as the bit-level representation of a
 *   single-precision floating point values.
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 30
 *   Rating: 4
 */
unsigned float_i2f(int x) {
  // 29 ops, rounding uses a lot

  int sign;
  unsigned abs_x;
  int lo;
  int shift;
  unsigned exponent;
  int detect;
  int t1;
  int carry;

  // If x is 0, then just return 0. The following don't want to consider 0 case.
  if (x) {
    // retrieve sign and abs value of given int
    sign = x >> 31;
    abs_x = (x + sign) ^ sign;

    // find the index from lowest of highest 1
    lo = 31;
    while (lo) {
      if (abs_x & (1 << lo)) {
        break;
      }
      lo = lo - 1;
    }

    // should we shift left or right?
    if (lo > 23) {
      // shift left, the nasty case.
      shift = lo - 23;
      // temp variable 1. We have to save operators.
      t1 = shift - 1;
      // `detect` aligns bits for rounding at lowest position
      detect = abs_x >> t1;
      // indicates if we should carry.
      carry = 0;
      //    >=1/2?      odd now?        >1/2?
      if (((detect) & ((detect >> 1) | (t1 && (abs_x << (33 - shift))))) & 1) {
        // if condition satisfied, carry.
        carry = 1;
      }
      // shift to position
      abs_x = abs_x >> shift;
      // carry
      abs_x += carry;
      // if we go over 2
      if (abs_x & 0x01000000) {
        lo += 1;
      }
    } else {
      // shift right, the easy case.
      abs_x = abs_x << (23 - lo);
    }

    // apply bias to exponent
    exponent = lo + 127;

    // assemble the result
    return (sign & 0x80000000) | (exponent << 23) | (abs_x & 0x007fffff);
  }

  // 0 for 0
  return 0;
}
/*
 * float_twice - Return bit-level equivalent of expression 2*f for
 *   floating point argument f.
 *   Both the argument and result are passed as unsigned int's, but
 *   they are to be interpreted as the bit-level representation of
 *   single-precision floating point values.
 *   When argument is NaN, return argument
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 30
 *   Rating: 4
 */
unsigned float_twice(unsigned uf) {
  // 12 ops

  int uf_abs = uf & 0x7fffffff;
  int sign = uf & 0x80000000;
  int exp;
  exp = uf & 0x7f800000;

  // nan, return itself
  if (uf_abs > 0x7f800000) {
    return uf;
  }

  // normal number
  if (exp) {
    // multiply 2 will be inf, apply sign
    if (uf_abs >= 0x7f000000) {
      return sign | 0x7f800000;
    }

    // normal case, add 1 to exp
    return (uf & 0x807fffff) | (exp + 0x00800000);
  }

  // subnormal number, multiply fraction by 2. Carry works automatically.
  return sign | ((uf & 0x007fffff) << 1);
}
#pragma clang diagnostic pop