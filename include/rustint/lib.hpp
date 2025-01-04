#pragma once

#include <climits>
#include <concepts>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <limits>
#include <optional>
#include <tuple>

#define rustint_unimplemented()                                                \
  do {                                                                         \
    std::fprintf(stderr, "UNIMPLEMENTED at %s:%s:%s", __FILE__, __LINE__,      \
                 __func__);                                                    \
    std::abort();                                                              \
  } while (false)

namespace rustint {

using i8 = std::int8_t;
using u8 = std::uint8_t;
using i16 = std::int16_t;
using u16 = std::uint16_t;
using i32 = std::int32_t;
using u32 = std::uint32_t;
using i64 = std::int64_t;
using u64 = std::uint64_t;
using i128 = __int128;
using u128 = unsigned __int128;
using isize = std::make_signed_t<std::size_t>;
using usize = std::size_t;

/// TODO: Remove these two.
struct TODO_SIGNATURE {};
inline void TODO_BODY() {};

template <typename T> T abs_diff(T x, T y) { return x > y ? x - y : y - x; }

template <typename T> std::tuple<T, bool> borrowing_sub(T x, T y, bool borrow) {
  rustint_unimplemented();
}

template <typename T> std::tuple<T, bool> carrying_add(T x, T y, bool carry) {
  rustint_unimplemented();
}

template <typename T> std::tuple<T, T> carrying_mul(T x, T y) {
  rustint_unimplemented();
}

template <typename T>
std::optional<T> checked_abs(T x)
  requires std::signed_integral<T>
{
  return x == std::numeric_limits<T>::min() ? std::nullopt
                                            : std::make_optional(std::abs(x));
}

template <typename T> std::optional<T> checked_add(T x, T y) {
  rustint_unimplemented();
}

template <std::integral T> class RInt {
private:
  using TU = std::make_unsigned_t<T>;
  using TS = std::make_signed_t<T>;

public:
  T val;

  constexpr RInt(T val) : val{val} {}
  constexpr auto operator()() const { return val; }
  static constexpr auto from(T val) { return RInt(val); }

  static constexpr auto MAX() { return RInt(std::numeric_limits<T>::max()); }
  static constexpr auto MIN() { return RInt(std::numeric_limits<T>::min()); }
  static constexpr auto BITS() { return u32(CHAR_BIT * sizeof(T)); }

  constexpr auto abs(this RInt self)
    requires std::signed_integral<T>
  {
    return RInt(std::abs(self.val));
  }

  constexpr auto abs_diff(this RInt self, RInt rhs) {
    return RInt(rustint::abs_diff(self.val, rhs.val));
  }

  constexpr auto borrowing_sub(this RInt self, RInt rhs, bool borrow) {
    auto [res, borrow_out] = rustint::borrowing_sub(self.val, rhs.val, borrow);
    return std::make_tuple(RInt(res), borrow_out);
  }

  constexpr auto carrying_add(this RInt self, RInt rhs, bool carry) {
    auto [res, carry_out] = rustint::carrying_add(self.val, rhs.val, carry);
    return std::make_tuple(RInt(res), carry_out);
  }

  constexpr auto carrying_mul(this RInt self, RInt rhs)
    requires std::unsigned_integral<T>
  {
    auto [res, carry] = rustint::carrying_mul(self.val, rhs);
    return std::make_tuple(RInt(res), RInt(carry));
  }

  constexpr auto cast_signed(this RInt self)
    requires std::unsigned_integral<T>
  {
    return RInt<TS>(static_cast<TS>(self.val));
  }

  constexpr auto cast_unsigned(this RInt self)
    requires std::signed_integral<T>
  {
    return RInt<TU>(static_cast<TU>(self.val));
  }

  constexpr auto checked_abs(this RInt self)
    requires std::signed_integral<T>
  {
    return rustint::checked_abs(self.val).transform(from);
  }

  constexpr auto checked_add(this RInt self, RInt rhs) {
    return rustint::checked_add(self.val, rhs.val).transform(from);
  }

  constexpr auto checked_add_signed(this RInt self, RInt<TS> rhs)
    requires std::unsigned_integral<T>
  {
    return TODO_BODY();
  }

  constexpr auto checked_add_unsigned(this RInt self, RInt<TU> rhs)
    requires std::signed_integral<T>
  {
    return TODO_BODY();
  }

  constexpr auto checked_div(this RInt self, RInt rhs) { return TODO_BODY(); }

  constexpr auto checked_div_euclid(this RInt self, RInt rhs) {
    return TODO_BODY();
  }

  constexpr auto checked_ilog(this RInt self, RInt base) { return TODO_BODY(); }

  constexpr auto checked_ilog10(this RInt self) { return TODO_BODY(); }

  constexpr auto checked_ilog2(this RInt self) { return TODO_BODY(); }

  constexpr auto checked_isqrt(this RInt self)
    requires std::signed_integral<T>
  {
    return TODO_BODY();
  }

  constexpr auto checked_mul(this RInt self, RInt rhs) { return TODO_BODY(); }

  constexpr auto checked_neg(this RInt self) { return TODO_BODY(); }

  constexpr auto checked_next_multiple_of(this RInt self, RInt rhs) {
    return TODO_BODY();
  }

  constexpr auto checked_next_power_of_two(this RInt self)
    requires std::unsigned_integral<T>
  {
    return TODO_BODY();
  }

  constexpr auto checked_pow(this RInt self, u32 exp) { return TODO_BODY(); }

  constexpr auto checked_rem(this RInt self, RInt rhs) { return TODO_BODY(); }

  constexpr auto checked_rem_euclid(this RInt self, RInt rhs) {
    return TODO_BODY();
  }

  constexpr auto checked_shl(this RInt self, u32 rhs) { return TODO_BODY(); }

  constexpr auto checked_shr(this RInt self, u32 rhs) { return TODO_BODY(); }

  constexpr auto checked_signed_diff(this RInt self, RInt rhs)
    requires std::unsigned_integral<T>
  {
    return TODO_BODY();
  }

  constexpr auto checked_sub(this RInt self, RInt rhs) { return TODO_BODY(); }

  constexpr auto checked_sub_unsigned(this RInt self, RInt<TU> rhs)
    requires std::signed_integral<T>
  {
    return TODO_BODY();
  }

  constexpr auto count_ones(this RInt self) { return TODO_BODY(); }

  constexpr auto count_zeros(this RInt self) { return TODO_BODY(); }

  constexpr auto div_ceil(this RInt self, RInt rhs) { return TODO_BODY(); }

  constexpr auto div_euclid(this RInt self, RInt rhs) { return TODO_BODY(); }

  constexpr auto div_floor(this RInt self, RInt rhs) { return TODO_BODY(); }

  static constexpr auto from_be(RInt x) { return TODO_BODY(); }

  constexpr auto from_be_bytes(this RInt self, TODO_SIGNATURE rhs) {
    return TODO_BODY();
  }

  static constexpr auto from_le(RInt x) { return TODO_BODY(); }

  constexpr auto from_le_bytes(this RInt self, TODO_SIGNATURE rhs) {
    return TODO_BODY();
  }

  constexpr auto from_ne_bytes(this RInt self, TODO_SIGNATURE rhs) {
    return TODO_BODY();
  }

  constexpr auto from_str_radix(this RInt self, TODO_SIGNATURE rhs) {
    return TODO_BODY();
  }

  constexpr auto ilog(this RInt self, RInt base) { return TODO_BODY(); }

  constexpr auto ilog10(this RInt self) { return TODO_BODY(); }

  constexpr auto ilog2(this RInt self) { return TODO_BODY(); }

  constexpr auto is_multiple_of(this RInt self, TODO_SIGNATURE rhs)
    requires std::unsigned_integral<T>
  {
    return TODO_BODY();
  }

  constexpr auto is_negative(this RInt self, TODO_SIGNATURE rhs)
    requires std::signed_integral<T>
  {
    return TODO_BODY();
  }

  constexpr auto is_positive(this RInt self, TODO_SIGNATURE rhs)
    requires std::signed_integral<T>
  {
    return TODO_BODY();
  }

  constexpr auto is_power_of_two(this RInt self, TODO_SIGNATURE rhs)
    requires std::unsigned_integral<T>
  {
    return TODO_BODY();
  }

  constexpr auto isqrt(this RInt self) { return TODO_BODY(); }

  constexpr auto leading_ones(this RInt self) { return TODO_BODY(); }

  constexpr auto leading_zeros(this RInt self) { return TODO_BODY(); }

  static constexpr auto max_value() { return MAX(); }

  static constexpr auto midpoint() { return TODO_BODY(); }

  static constexpr auto min_value() { return MIN(); }

  constexpr auto next_multiple_of(this RInt self, TODO_SIGNATURE rhs) {
    return TODO_BODY();
  }

  constexpr auto next_power_of_two(this RInt self, TODO_SIGNATURE rhs)
    requires std::unsigned_integral<T>
  {
    return TODO_BODY();
  }

  constexpr auto overflowing_abs(this RInt self, TODO_SIGNATURE rhs)
    requires std::signed_integral<T>
  {
    return TODO_BODY();
  }

  constexpr auto overflowing_add(this RInt self, TODO_SIGNATURE rhs) {
    return TODO_BODY();
  }

  constexpr auto overflowing_add_signed(this RInt self, TODO_SIGNATURE rhs)
    requires std::unsigned_integral<T>
  {
    return TODO_BODY();
  }

  constexpr auto overflowing_add_unsigned(this RInt self, TODO_SIGNATURE rhs)
    requires std::signed_integral<T>
  {
    return TODO_BODY();
  }

  constexpr auto overflowing_div(this RInt self, TODO_SIGNATURE rhs) {
    return TODO_BODY();
  }

  constexpr auto overflowing_div_euclid(this RInt self, TODO_SIGNATURE rhs) {
    return TODO_BODY();
  }

  constexpr auto overflowing_mul(this RInt self, TODO_SIGNATURE rhs) {
    return TODO_BODY();
  }

  constexpr auto overflowing_neg(this RInt self, TODO_SIGNATURE rhs) {
    return TODO_BODY();
  }

  constexpr auto overflowing_pow(this RInt self, TODO_SIGNATURE rhs) {
    return TODO_BODY();
  }

  constexpr auto overflowing_rem(this RInt self, TODO_SIGNATURE rhs) {
    return TODO_BODY();
  }

  constexpr auto overflowing_rem_euclid(this RInt self, TODO_SIGNATURE rhs) {
    return TODO_BODY();
  }

  constexpr auto overflowing_shl(this RInt self, TODO_SIGNATURE rhs) {
    return TODO_BODY();
  }

  constexpr auto overflowing_shr(this RInt self, TODO_SIGNATURE rhs) {
    return TODO_BODY();
  }

  constexpr auto overflowing_sub(this RInt self, TODO_SIGNATURE rhs) {
    return TODO_BODY();
  }

  constexpr auto overflowing_sub_unsigned(this RInt self, TODO_SIGNATURE rhs)
    requires std::signed_integral<T>
  {
    return TODO_BODY();
  }

  constexpr auto pow(this RInt self, u32 exp) { return TODO_BODY(); }

  constexpr auto rem_euclid(this RInt self, TODO_SIGNATURE rhs) {
    return TODO_BODY();
  }

  constexpr auto reverse_bits(this RInt self) { return TODO_BODY(); }

  constexpr auto rotate_left(this RInt self, u32 n) { return TODO_BODY(); }

  constexpr auto rotate_right(this RInt self, u32 n) { return TODO_BODY(); }

  constexpr auto saturating_abs(this RInt self)
    requires std::signed_integral<T>
  {
    return TODO_BODY();
  }

  constexpr auto saturating_add(this RInt self, TODO_SIGNATURE rhs) {
    return TODO_BODY();
  }

  constexpr auto saturating_add_signed(this RInt self, TODO_SIGNATURE rhs)
    requires std::unsigned_integral<T>
  {
    return TODO_BODY();
  }

  constexpr auto saturating_add_unsigned(this RInt self, TODO_SIGNATURE rhs)
    requires std::signed_integral<T>
  {
    return TODO_BODY();
  }

  constexpr auto saturating_div(this RInt self, TODO_SIGNATURE rhs) {
    return TODO_BODY();
  }

  constexpr auto saturating_mul(this RInt self, TODO_SIGNATURE rhs) {
    return TODO_BODY();
  }

  constexpr auto saturating_neg(this RInt self, TODO_SIGNATURE rhs)
    requires std::signed_integral<T>
  {
    return TODO_BODY();
  }

  constexpr auto saturating_pow(this RInt self, TODO_SIGNATURE rhs) {
    return TODO_BODY();
  }

  constexpr auto saturating_sub(this RInt self, TODO_SIGNATURE rhs) {
    return TODO_BODY();
  }

  constexpr auto saturating_sub_unsigned(this RInt self, TODO_SIGNATURE rhs)
    requires std::signed_integral<T>
  {
    return TODO_BODY();
  }

  constexpr auto signum(this RInt self, TODO_SIGNATURE rhs)
    requires std::signed_integral<T>
  {
    return TODO_BODY();
  }

  constexpr auto strict_abs(this RInt self, TODO_SIGNATURE rhs)
    requires std::signed_integral<T>
  {
    return TODO_BODY();
  }

  constexpr auto strict_add(this RInt self, TODO_SIGNATURE rhs) {
    return TODO_BODY();
  }

  constexpr auto strict_add_signed(this RInt self, TODO_SIGNATURE rhs)
    requires std::unsigned_integral<T>
  {
    return TODO_BODY();
  }

  constexpr auto strict_add_unsigned(this RInt self, TODO_SIGNATURE rhs)
    requires std::signed_integral<T>
  {
    return TODO_BODY();
  }

  constexpr auto strict_div(this RInt self, TODO_SIGNATURE rhs) {
    return TODO_BODY();
  }

  constexpr auto strict_div_euclid(this RInt self, TODO_SIGNATURE rhs) {
    return TODO_BODY();
  }

  constexpr auto strict_mul(this RInt self, TODO_SIGNATURE rhs) {
    return TODO_BODY();
  }

  constexpr auto strict_neg(this RInt self) { return TODO_BODY(); }

  constexpr auto strict_pow(this RInt self, TODO_SIGNATURE rhs) {
    return TODO_BODY();
  }

  constexpr auto strict_rem(this RInt self, TODO_SIGNATURE rhs) {
    return TODO_BODY();
  }

  constexpr auto strict_rem_euclid(this RInt self, TODO_SIGNATURE rhs) {
    return TODO_BODY();
  }

  constexpr auto strict_shl(this RInt self, TODO_SIGNATURE rhs) {
    return TODO_BODY();
  }

  constexpr auto strict_shr(this RInt self, TODO_SIGNATURE rhs) {
    return TODO_BODY();
  }

  constexpr auto strict_sub(this RInt self, TODO_SIGNATURE rhs) {
    return TODO_BODY();
  }

  constexpr auto strict_sub_unsigned(this RInt self, TODO_SIGNATURE rhs)
    requires std::signed_integral<T>
  {
    return TODO_BODY();
  }

  constexpr auto swap_bytes(this RInt self, TODO_SIGNATURE rhs) {
    return TODO_BODY();
  }

  constexpr auto to_be(this RInt self) { return TODO_BODY(); }

  constexpr auto to_be_bytes(this RInt self) { return TODO_BODY(); }

  constexpr auto to_le(this RInt self) { return TODO_BODY(); }

  constexpr auto to_le_bytes(this RInt self) { return TODO_BODY(); }

  constexpr auto to_ne_bytes(this RInt self) { return TODO_BODY(); }

  constexpr auto trailing_ones(this RInt self) { return TODO_BODY(); }

  constexpr auto trailing_zeros(this RInt self) { return TODO_BODY(); }

  constexpr auto unbounded_shl(this RInt self, TODO_SIGNATURE rhs) {
    return TODO_BODY();
  }

  constexpr auto unbounded_shr(this RInt self, TODO_SIGNATURE rhs) {
    return TODO_BODY();
  }

  constexpr auto unchecked_add(this RInt self, TODO_SIGNATURE rhs) {
    return TODO_BODY();
  }

  constexpr auto unchecked_mul(this RInt self, TODO_SIGNATURE rhs) {
    return TODO_BODY();
  }

  constexpr auto unchecked_neg(this RInt self, TODO_SIGNATURE rhs)
    requires std::signed_integral<T>
  {
    return TODO_BODY();
  }

  constexpr auto unchecked_shl(this RInt self, TODO_SIGNATURE rhs) {
    return TODO_BODY();
  }

  constexpr auto unchecked_shr(this RInt self, TODO_SIGNATURE rhs) {
    return TODO_BODY();
  }

  constexpr auto unchecked_sub(this RInt self, TODO_SIGNATURE rhs) {
    return TODO_BODY();
  }

  constexpr auto unsigned_abs(this RInt self, TODO_SIGNATURE rhs)
    requires std::signed_integral<T>
  {
    return TODO_BODY();
  }

  constexpr auto widening_mul(this RInt self, TODO_SIGNATURE rhs)
    requires std::unsigned_integral<T>
  {
    return TODO_BODY();
  }

  constexpr auto wrapping_abs(this RInt self, TODO_SIGNATURE rhs)
    requires std::signed_integral<T>
  {
    return TODO_BODY();
  }

  constexpr auto wrapping_add(this RInt self, TODO_SIGNATURE rhs) {
    return TODO_BODY();
  }

  constexpr auto wrapping_add_signed(this RInt self, TODO_SIGNATURE rhs)
    requires std::unsigned_integral<T>
  {
    return TODO_BODY();
  }

  constexpr auto wrapping_add_unsigned(this RInt self, TODO_SIGNATURE rhs)
    requires std::signed_integral<T>
  {
    return TODO_BODY();
  }

  constexpr auto wrapping_div(this RInt self, TODO_SIGNATURE rhs) {
    return TODO_BODY();
  }

  constexpr auto wrapping_div_euclid(this RInt self, TODO_SIGNATURE rhs) {
    return TODO_BODY();
  }

  constexpr auto wrapping_mul(this RInt self, TODO_SIGNATURE rhs) {
    return TODO_BODY();
  }

  constexpr auto wrapping_neg(this RInt self, TODO_SIGNATURE rhs) {
    return TODO_BODY();
  }

  constexpr auto wrapping_next_power_of_two(this RInt self, TODO_SIGNATURE rhs)
    requires std::unsigned_integral<T>
  {
    return TODO_BODY();
  }

  constexpr auto wrapping_pow(this RInt self, u32 exp) { return TODO_BODY(); }

  constexpr auto wrapping_rem(this RInt self, TODO_SIGNATURE rhs) {
    return TODO_BODY();
  }

  constexpr auto wrapping_rem_euclid(this RInt self, TODO_SIGNATURE rhs) {
    return TODO_BODY();
  }

  constexpr auto wrapping_shl(this RInt self, TODO_SIGNATURE rhs) {
    return TODO_BODY();
  }

  constexpr auto wrapping_shr(this RInt self, TODO_SIGNATURE rhs) {
    return TODO_BODY();
  }

  constexpr auto wrapping_sub(this RInt self, TODO_SIGNATURE rhs) {
    return TODO_BODY();
  }

  constexpr auto wrapping_sub_unsigned(this RInt self, TODO_SIGNATURE rhs)
    requires std::signed_integral<T>
  {
    return TODO_BODY();
  }
};

} // namespace rustint
