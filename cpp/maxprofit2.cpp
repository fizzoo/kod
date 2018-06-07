#include <iostream>
#include <vector>
using namespace std;

typedef vector<int> V;

int maxprofit(V prices){
  if (prices.size() == 0){
    return 0;
  }

  int lowest = prices[0];
  int prev = prices[0];
  int profit = 0;
  for (const auto p : prices){
    if (p < prev){
      // SELL SELL SELL
      profit += prev - lowest;
      lowest = p;
    }
    prev = p;
  }
  profit += max(0, prev - lowest);
  return profit;
}

int main(){
  V t1 = {7,1,5,3,6,4};
  V t2 = {7,6,3,2,1};
  V t3 = {1,2,3,4,5};
  cout << maxprofit(t3);
}
