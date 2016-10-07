#include <algorithm>
#include <cmath>
#include <iostream>
#include <vector>
using namespace std;


bool representable(unsigned long y, int b) {
  while(y > 0){
    if ((y % b) >= 10) {
      return false;
    }
    y /= b;
  }

  return true;
}

int main() {
  unsigned long y, l, b, sum, tmpl;

  std::cin >> y >> l;

  std::vector<unsigned long> l_rep;

  while (l > 0) {
    l_rep.push_back(l % 10);
    l /= 10;
  }
  std::reverse(l_rep.begin(), l_rep.end());

  b = 10;
  while (1) {
    sum = 0;
    tmpl = l;
    for (int i = 0; i < l_rep.size(); ++i) {
      sum *= b;
      sum += l_rep[i];
    }

    if (sum >= y) {
      break;
    } else {
      ++b;
    }
#ifdef FETT
    std::cout << sum << std::endl;
#endif
  }

  while (b > 10) {
    if (representable(y, b)){
      break;
    }
    --b;
  }

  std::cout << b << std::endl;
}
