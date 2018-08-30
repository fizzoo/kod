#include <iostream>
#include <set>
#include <vector>
using namespace std;

struct Tree {
  int d = -1;
  Tree *l = nullptr, *r = nullptr;

  void insert(Tree *t) {
    if (t->d > d) {
      if (r) {
        r->insert(t);
      } else {
        r = t;
      }
    } else {
      if (l) {
        l->insert(t);
      } else {
        l = t;
      }
    }
  }
};

Tree *make_tree(vector<int> in) {
  Tree *trees = new Tree[in.size()];

  for (int i = 0; i < in.size(); ++i) {
    trees[i].d = in[i];
  }
  for (int i = 1; i < in.size(); ++i) {
    trees[0].insert(trees + i);
  }

  return trees;
}

bool tree_equal(Tree *t1, Tree *t2) {
  // different -> false
  if (!t1->l != !t2->l)
    return false;
  if (!t1->r != !t2->r)
    return false;

  bool left = true, right = true;
  if (t1->l && t2->l) {
    left = tree_equal(t1->l, t2->l);
  }
  if (t1->r && t2->r) {
    right = tree_equal(t1->r, t2->r);
  }
  return left && right;
}

void print_tree(Tree *t) {
  if (!t->l && !t->r){
    std::cout << t->d;
    return;
  }
  std::cout << "(";
  if (t->l) {
    print_tree(t->l);
  } else {
    std::cout << ".";
  }
  std::cout << ",";
  if (t->r) {
    print_tree(t->r);
  } else {
    std::cout << ".";
  }
  std::cout << ")";
}

int main() {
  int nr_trees, nr_layers;
  cin >> nr_trees;
  cin >> nr_layers;
  vector<vector<int>> in(nr_trees);

  for (int t = 0; t < nr_trees; ++t) {
    for (int l = 0; l < nr_layers; ++l) {
      int x;
      cin >> x;
      in[t].push_back(x);
    }
  }

  vector<Tree *> trees;
  for (auto &vec : in) {
    trees.push_back(make_tree(vec));
  }

  for (int i = 0; i < trees.size(); ++i) {
    for (int j = i + 1; j < trees.size(); ++j) {
      if (tree_equal(trees[i], trees[j])) {
        trees.erase(trees.begin() + j);
        --j;
      }
    }
  }

  std::cout << trees.size() << std::endl;
}
