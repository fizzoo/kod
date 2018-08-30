#include <iostream>
#include <vector>

class Heap {
public:
  std::vector<int> d = {};

  void percolate(int i) {
    while ((i > 0) && (d[i] < d[i / 2])) {
      std::swap(d[i], d[i / 2]);
      i /= 2;
    }
  }
  void insert(int i) {
    d.push_back(i);
    percolate(d.size() - 1);
  }
  int get_first() { return d[0]; }
  void rm_first() {
    std::swap(d[0], d[d.size() - 1]);
    d.pop_back();
    int i = 0;
    while (i * 2 + 2 < d.size()) { // break if none of left,right exist
      bool right = d[i * 2 + 2] < d[i * 2 + 1];
      if (d[i * 2 + right + 1] < d[i]) {
        std::swap(d[i * 2 + right + 1], d[i]);
      }
      i = i*2 + right + 1;
    }
    if (i * 2 + 1 < d.size()) { // left one
      if (d[i * 2 + 1] < d[i]) {
        std::swap(d[i * 2 + 1], d[i]);
      }
    }
  }
};

int main() {
  Heap h;
  h.insert(1);
  h.insert(2);
  h.insert(3);
  h.insert(0);
  h.insert(5);
  h.insert(-1);
  h.insert(-2);
  h.insert(10);
  h.rm_first();
  h.rm_first();
  for (auto &i : h.d) {
    std::cout << i << std::endl;
  }
}
