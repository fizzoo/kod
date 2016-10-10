#include <algorithm>
#include <cassert>
#include <cmath>
#include <iostream>
#include <vector>
#include <cstdint>

using namespace std;

uint64_t representation(uint64_t y, uint64_t b){
  vector<char> str;
  uint64_t rep = 0;

  while (y > 0) {
    if ((y % b) >= 10) {
      return 0;
    }
    str.push_back(y % b);
    y /= b;
  }

  for (int i = str.size()-1; i >= 0; --i) {
    rep *= 10;
    rep += str[i];
  }

  return rep;
}

uint64_t findb(uint64_t actualage, uint64_t wantage) {
  vector<uint64_t> bwantrep;

  while (wantage > 0) {
    bwantrep.push_back(wantage % 10);
    wantage /= 10;
  }
  std::reverse(bwantrep.begin(), bwantrep.end());

  uint64_t b, minb = 10, maxb = 1UL << 62, age, tmp;
  while (1) {
    age = 0;
    b = minb + ((maxb - minb) / 2);
    for (uint64_t i = 0; i < bwantrep.size(); ++i) {
      tmp = age * b;
      age = age != 0 && tmp / age != b ? 1UL << 62 : tmp;
      age += bwantrep[i];
    }
    if (actualage == age) {
      return b;
    } else if (age > actualage) {
      maxb = b - 1;
    } else {
      minb = b + 1;
    }
    if (minb == maxb - 1 || minb == maxb) {
      age = 0;
      for (uint64_t i = 0; i < bwantrep.size(); ++i) {
        tmp = age * minb;
        age = age != 0 && tmp / age != minb ? 1UL << 62 : tmp;
        age += bwantrep[i];
      }
      if (actualage == age) {
        return minb;
      }
      age = 0;
      for (uint64_t i = 0; i < bwantrep.size(); ++i) {
        tmp = age * maxb;
        age = age != 0 && tmp / age != maxb ? 1UL << 62 : tmp;
        age += bwantrep[i];
      }
      if (actualage == age) {
        return maxb;
      }

      return 0;
    }
  }

  return 0;
}

int main() {
  uint64_t y, l, b;

  std::cin >> y >> l;
  b = y - 1;

  for (uint64_t i = l; i <= 9999; ++i) {
    uint64_t lel = findb(y, i);
    if (lel) {
      std::cout << lel;
      return 0;
    }
  }

  b = b < 100000 ? b : 100000;
  while (b >= 10) {
    if (representation(y, b) >= l) {
      std::cout << b;
      return 0;
    }
    --b;
  }

  throw 3;
}
