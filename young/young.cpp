#include <algorithm>
#include <cassert>
#include <cmath>
#include <iostream>
#include <vector>
#include <cstdint>

using namespace std;

bool representable(uint64_t y, uint64_t b) {
  while (y > 0) {
    if ((y % b) >= 10) {
      return false;
    }
    y /= b;
  }

  return true;
}
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

  uint64_t b, minb = 10, maxb = 100000, age;
  while (1) {
    age = 0;
    b = (minb + maxb) / 2;
    for (uint64_t i = 0; i < bwantrep.size(); ++i) {
      age *= b;
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
        age *= minb;
        age += bwantrep[i];
      }
      if (actualage == age) {
        return minb;
      }
      age = 0;
      for (uint64_t i = 0; i < bwantrep.size(); ++i) {
        age *= maxb;
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
  uint64_t y, l, b, sum, maxb, minb, tmpl;

  std::cin >> y >> l;

  // std::vector<uint64_t> l_rep;
  // tmpl = l;
  // while (tmpl > 0) {
  //   l_rep.push_back(tmpl % 10);
  //   tmpl /= 10;
  // }
  // std::reverse(l_rep.begin(), l_rep.end());

  b = y;
  // // Get a maximum possible b, but this one is not certain to be representable
  // minb = 10;
  // maxb = y;
  // while (1) {
  //   b = (minb + maxb) / 2;
  //   sum = 0;

  //   for (uint64_t i = 0; i < l_rep.size(); ++i) {
  //     sum *= b;
  //     sum += l_rep[i];
  //   }

  //   // std::cout << minb << "." << b << "." << maxb << " - " << sum << "." << y << std::endl;
  //   if (sum == y) {
  //     // std::cout << "instawon" << std::endl;
  //     std::cout << b << std::endl;
  //     return 0;
  //   } else if (sum > y) {
  //     maxb = b - 1;
  //   } else {
  //     minb = b + 1;
  //   }
  //   if (minb == maxb) {
  //     b = maxb;
  //     break;
  //   }
  // }

  if (b > 10000) { // Else probably not worth
    for (uint64_t i = l; i < 1000000; ++i) {
      uint64_t lel = findb(y, i);
      if (lel) {
        // std::cerr << "lelled out at " << i << std::endl;
        std::cout << lel << std::endl;
        return 0;
      }
    }
  }

  b = b < 1000000 ? b : 1000000;
  while (b >= 10) {
    if (representation(y, b) >= l) {
      // std::cout << "slowshit" << std::endl;
      std::cout << b << std::endl;
      return 0;
    }
    --b;
  }
}
