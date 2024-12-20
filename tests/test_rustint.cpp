#include <climits>

#include "rustint/lib.hpp"

using rustint::RInt;

int main() {
  auto r = RInt(INT_MIN).checked_abs();
  return 0;
}
