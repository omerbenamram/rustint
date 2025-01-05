#include <cassert>
#include <iostream>
#include "rustint/lib.hpp"

using namespace rustint;

void test_basic_arithmetic()
{
  RInt<i32> a(5);
  RInt<i32> b(3);

  // Test addition with overflow checking
  auto sum = a.checked_add(b);
  assert(sum.has_value() && sum->val == 8);

  // Test subtraction with overflow checking
  auto diff = a.checked_sub(b);
  assert(diff.has_value() && diff->val == 2);

  // Test multiplication with overflow checking
  auto prod = a.checked_mul(b);
  assert(prod.has_value() && prod->val == 15);
}

void test_bit_operations()
{
  RInt<u8> x(0b10110010);

  // Test bit counting
  assert(x.count_ones().val == 4);
  assert(x.count_zeros().val == 4);

  // Test bit reversal
  assert(x.reverse_bits().val == 0b01001101);

  // Test rotation
  assert(x.rotate_left(2).val == 0b11001010);
  assert(x.rotate_right(2).val == 0b10101100);
}

void test_signed_operations()
{
  RInt<i8> pos(42);
  RInt<i8> neg(-42);

  // Test abs
  assert(neg.abs().val == 42);
  assert(pos.abs().val == 42);

  // Test sign checks
  assert(pos.is_positive());
  assert(!pos.is_negative());
  assert(neg.is_negative());
  assert(!neg.is_positive());
}

int main()
{
  test_basic_arithmetic();
  test_bit_operations();
  test_signed_operations();
  std::cout << "All tests passed!\n";
  return 0;
}
