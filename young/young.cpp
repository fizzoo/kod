#include <algorithm>
#include <cassert>
#include <cmath>
#include <iostream>
#include <vector>

using namespace std;

bool representable(unsigned long y, int b) {
  while (y > 0) {
    if ((y % b) >= 10) {
      return false;
    }
    y /= b;
  }

  return true;
}

int findb(int actualage, int wantage) {
  vector<int> bwantrep;

  while (wantage > 0) {
    bwantrep.push_back(wantage % 10);
    wantage /= 10;
  }
  std::reverse(bwantrep.begin(), bwantrep.end());

  unsigned long b, minb = 10, maxb = 100000, age;
  while (1) {
    age = 0;
    b = (minb + maxb) / 2;
    for (int i = 0; i < bwantrep.size(); ++i) {
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
      for (int i = 0; i < bwantrep.size(); ++i) {
        age *= minb;
        age += bwantrep[i];
      }
      if (actualage == age) {
        return minb;
      }
      age = 0;
      for (int i = 0; i < bwantrep.size(); ++i) {
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
  unsigned long y, l, b, sum, maxb, minb, tmpl;

  std::cin >> y >> l;
  tmpl = l;

  std::vector<unsigned long> l_rep;

  while (tmpl > 0) {
    l_rep.push_back(tmpl % 10);
    tmpl /= 10;
  }
  std::reverse(l_rep.begin(), l_rep.end());

  // Get a maximum possible b, but this one is not certain to be representable
  minb = 10;
  maxb = y;
  while (1) {
    b = (minb + maxb) / 2;
    sum = 0;

    for (int i = 0; i < l_rep.size(); ++i) {
      sum *= b;
      sum += l_rep[i];
    }

    if (sum == y) {
      // std::cout << "instawon" << std::endl;
      std::cout << b << std::endl;
      return 0;
    } else if (sum >= y) {
      maxb = b - 1;
    } else {
      minb = b + 1;
    }
    if (minb == maxb || minb == maxb + 1) {
      b = maxb + 1;
      break;
    }
  }

  if (b > 1000) { // Else probably not worth
    for (int i = l; i < 1000000; ++i) {
      int lel = findb(y, i);
      if (lel) {
        // std::cout << "lelled out at " << i << std::endl;
        std::cout << lel << std::endl;
        return 0;
      }
    }
  }

  b = b < 10000000 ? b : 10000000;
  while (b > 10) {
    if (representable(y, b)) {
      // std::cout << "slowshit" << std::endl;
      std::cout << b << std::endl;
      return 0;
    }
    --b;
  }
}
