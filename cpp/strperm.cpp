#include <iostream>
#include <string>
#include <algorithm>
using namespace std;

int main() {
  string in;
  cin >> in;
  int perms = 1;
  for (int i = in.length(); i; --i){
    perms *= i;
  }

  for (int i = 0; i < perms; ++i) {
    cout << in << endl;
    next_permutation(in.begin(), in.end());
  }
}
