#pragma once

#include <concepts>
#include <cstdint>
#include <cstdlib>
#include <limits>
#include <tuple>

namespace rustint {

struct TODO_SIGNATURE {};
inline void TODO_BODY() {};

template <typename T> struct RInt {
  T val;

  RInt(T val) : val{val} {}

  constexpr auto abs(this RInt self, TODO_SIGNATURE other)
    requires std::signed_integral<T>
  {
    return TODO_BODY();
  }

  constexpr auto abs_diff(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto borrowing_sub(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto carrying_add(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto carrying_mul(this RInt self, TODO_SIGNATURE other)
    requires std::unsigned_integral<T>
  {
    return TODO_BODY();
  }

  constexpr auto cast_signed(this RInt self, TODO_SIGNATURE other)
    requires std::unsigned_integral<T>
  {
    return TODO_BODY();
  }

  constexpr auto cast_unsigned(this RInt self, TODO_SIGNATURE other)
    requires std::signed_integral<T>
  {
    return TODO_BODY();
  }

  constexpr auto checked_abs(this RInt self, TODO_SIGNATURE other)
    requires std::signed_integral<T>
  {
    return TODO_BODY();
  }

  constexpr auto checked_add(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto checked_add_signed(this RInt self, TODO_SIGNATURE other)
    requires std::unsigned_integral<T>
  {
    return TODO_BODY();
  }

  constexpr auto checked_add_unsigned(this RInt self, TODO_SIGNATURE other)
    requires std::signed_integral<T>
  {
    return TODO_BODY();
  }

  constexpr auto checked_div(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto checked_div_euclid(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto checked_ilog(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto checked_ilog10(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto checked_ilog2(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto checked_isqrt(this RInt self, TODO_SIGNATURE other)
    requires std::signed_integral<T>
  {
    return TODO_BODY();
  }

  constexpr auto checked_mul(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto checked_neg(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto checked_next_multiple_of(this RInt self,
                                          TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto checked_next_power_of_two(this RInt self, TODO_SIGNATURE other)
    requires std::unsigned_integral<T>
  {
    return TODO_BODY();
  }

  constexpr auto checked_pow(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto checked_rem(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto checked_rem_euclid(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto checked_shl(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto checked_shr(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto checked_signed_diff(this RInt self, TODO_SIGNATURE other)
    requires std::unsigned_integral<T>
  {
    return TODO_BODY();
  }

  constexpr auto checked_sub(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto checked_sub_unsigned(this RInt self, TODO_SIGNATURE other)
    requires std::signed_integral<T>
  {
    return TODO_BODY();
  }

  constexpr auto count_ones(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto count_zeros(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto div_ceil(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto div_euclid(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto div_floor(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto from_be(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto from_be_bytes(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto from_le(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto from_le_bytes(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto from_ne_bytes(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto from_str_radix(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto ilog(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto ilog10(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto ilog2(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto is_multiple_of(this RInt self, TODO_SIGNATURE other)
    requires std::unsigned_integral<T>
  {
    return TODO_BODY();
  }

  constexpr auto is_negative(this RInt self, TODO_SIGNATURE other)
    requires std::signed_integral<T>
  {
    return TODO_BODY();
  }

  constexpr auto is_positive(this RInt self, TODO_SIGNATURE other)
    requires std::signed_integral<T>
  {
    return TODO_BODY();
  }

  constexpr auto is_power_of_two(this RInt self, TODO_SIGNATURE other)
    requires std::unsigned_integral<T>
  {
    return TODO_BODY();
  }

  constexpr auto isqrt(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto leading_ones(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto leading_zeros(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto max_value(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto midpoint(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto min_value(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto next_multiple_of(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto next_power_of_two(this RInt self, TODO_SIGNATURE other)
    requires std::unsigned_integral<T>
  {
    return TODO_BODY();
  }

  constexpr auto overflowing_abs(this RInt self, TODO_SIGNATURE other)
    requires std::signed_integral<T>
  {
    return TODO_BODY();
  }

  constexpr auto overflowing_add(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto overflowing_add_signed(this RInt self, TODO_SIGNATURE other)
    requires std::unsigned_integral<T>
  {
    return TODO_BODY();
  }

  constexpr auto overflowing_add_unsigned(this RInt self, TODO_SIGNATURE other)
    requires std::signed_integral<T>
  {
    return TODO_BODY();
  }

  constexpr auto overflowing_div(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto overflowing_div_euclid(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto overflowing_mul(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto overflowing_neg(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto overflowing_pow(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto overflowing_rem(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto overflowing_rem_euclid(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto overflowing_shl(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto overflowing_shr(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto overflowing_sub(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto overflowing_sub_unsigned(this RInt self, TODO_SIGNATURE other)
    requires std::signed_integral<T>
  {
    return TODO_BODY();
  }

  constexpr auto pow(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto rem_euclid(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto reverse_bits(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto rotate_left(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto rotate_right(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto saturating_abs(this RInt self, TODO_SIGNATURE other)
    requires std::signed_integral<T>
  {
    return TODO_BODY();
  }

  constexpr auto saturating_add(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto saturating_add_signed(this RInt self, TODO_SIGNATURE other)
    requires std::unsigned_integral<T>
  {
    return TODO_BODY();
  }

  constexpr auto saturating_add_unsigned(this RInt self, TODO_SIGNATURE other)
    requires std::signed_integral<T>
  {
    return TODO_BODY();
  }

  constexpr auto saturating_div(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto saturating_mul(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto saturating_neg(this RInt self, TODO_SIGNATURE other)
    requires std::signed_integral<T>
  {
    return TODO_BODY();
  }

  constexpr auto saturating_pow(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto saturating_sub(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto saturating_sub_unsigned(this RInt self, TODO_SIGNATURE other)
    requires std::signed_integral<T>
  {
    return TODO_BODY();
  }

  constexpr auto signum(this RInt self, TODO_SIGNATURE other)
    requires std::signed_integral<T>
  {
    return TODO_BODY();
  }

  constexpr auto strict_abs(this RInt self, TODO_SIGNATURE other)
    requires std::signed_integral<T>
  {
    return TODO_BODY();
  }

  constexpr auto strict_add(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto strict_add_signed(this RInt self, TODO_SIGNATURE other)
    requires std::unsigned_integral<T>
  {
    return TODO_BODY();
  }

  constexpr auto strict_add_unsigned(this RInt self, TODO_SIGNATURE other)
    requires std::signed_integral<T>
  {
    return TODO_BODY();
  }

  constexpr auto strict_div(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto strict_div_euclid(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto strict_mul(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto strict_neg(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto strict_pow(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto strict_rem(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto strict_rem_euclid(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto strict_shl(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto strict_shr(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto strict_sub(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto strict_sub_unsigned(this RInt self, TODO_SIGNATURE other)
    requires std::signed_integral<T>
  {
    return TODO_BODY();
  }

  constexpr auto swap_bytes(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto to_be(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto to_be_bytes(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto to_le(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto to_le_bytes(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto to_ne_bytes(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto trailing_ones(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto trailing_zeros(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto unbounded_shl(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto unbounded_shr(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto unchecked_add(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto unchecked_mul(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto unchecked_neg(this RInt self, TODO_SIGNATURE other)
    requires std::signed_integral<T>
  {
    return TODO_BODY();
  }

  constexpr auto unchecked_shl(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto unchecked_shr(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto unchecked_sub(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto unsigned_abs(this RInt self, TODO_SIGNATURE other)
    requires std::signed_integral<T>
  {
    return TODO_BODY();
  }

  constexpr auto widening_mul(this RInt self, TODO_SIGNATURE other)
    requires std::unsigned_integral<T>
  {
    return TODO_BODY();
  }

  constexpr auto wrapping_abs(this RInt self, TODO_SIGNATURE other)
    requires std::signed_integral<T>
  {
    return TODO_BODY();
  }

  constexpr auto wrapping_add(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto wrapping_add_signed(this RInt self, TODO_SIGNATURE other)
    requires std::unsigned_integral<T>
  {
    return TODO_BODY();
  }

  constexpr auto wrapping_add_unsigned(this RInt self, TODO_SIGNATURE other)
    requires std::signed_integral<T>
  {
    return TODO_BODY();
  }

  constexpr auto wrapping_div(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto wrapping_div_euclid(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto wrapping_mul(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto wrapping_neg(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto wrapping_next_power_of_two(this RInt self,
                                            TODO_SIGNATURE other)
    requires std::unsigned_integral<T>
  {
    return TODO_BODY();
  }

  constexpr auto wrapping_pow(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto wrapping_rem(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto wrapping_rem_euclid(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto wrapping_shl(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto wrapping_shr(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto wrapping_sub(this RInt self, TODO_SIGNATURE other) {
    return TODO_BODY();
  }

  constexpr auto wrapping_sub_unsigned(this RInt self, TODO_SIGNATURE other)
    requires std::signed_integral<T>
  {
    return TODO_BODY();
  }
};

} // namespace rustint
