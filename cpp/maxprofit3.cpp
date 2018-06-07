#include <iostream>
#include <vector>
#include <algorithm>
using namespace std;

typedef vector<int> V;

void print_v(const V &v){
  for (int i = 0; i < v.size(); ++i) {
    cout << v[i] << " ";
  }
  cout << endl;
}

int maxprofit(V prices){
  if (prices.size() < 2){
    return 0;
  }
  auto res = V(prices.size(), 0);
  auto max_from_right(res);
  auto min_from_left(res);
  // prof_left: if i sell on i, how much can i profit if i buy at [0, i-1]?
  auto prof_left(res);
  // prof_right: if i buy on i, how much can i profit if i sell at [i+1, end]?
  auto prof_right(res);

  int m = prices.back();
  for (int i = max_from_right.size()-1; i >= 0; --i) {
    m = max(m, prices[i]);
    max_from_right[i] = m;
  }
  m = prices.front();
  for (int i = 0; i < min_from_left.size(); ++i) {
    m = min(m, prices[i]);
    min_from_left[i] = m;
  }

  int maxprofit = 0;

  for (int i = 1; i < prices.size(); ++i) {
    prof_left[i] = max(prof_left[i-1], prices[i] - min_from_left[i]);
  }
  for (int i = prices.size()-2; i >= 0; --i) {
    prof_right[i] = max(prof_right[i+1], max_from_right[i+1] - prices[i]);
  }

  // print_v(prices);
  // cout << endl;
  // print_v(max_from_right);
  // print_v(prof_right);
  // cout << endl;
  // print_v(min_from_left);
  // print_v(prof_left);

  int maxprof = 0;
  for (int i = 1; i < prices.size()-1; ++i) {
    maxprof = max(maxprof, prof_left[i] + prof_right[i+1]);
  }

  return max(maxprof, prof_left.back());
}


int main(){
  V t1 = {7,1,5,3,6,4};
  V t2 = {7,6,3,2,1};
  V t3 = {1,2,3,4,5};
  V t4 = {3,3,5,0,0,3,1,4};
  cout << maxprofit(t3);
}
