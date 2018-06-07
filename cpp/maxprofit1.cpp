#include <iostream>
#include <vector>
using namespace std;

typedef vector<int> V;

int maxprofit(V prices){
  if (prices.size() == 0){
    return 0;
  }

  int lowest = prices[0];
  int max_profit = 0;
  for (const auto p : prices){
    lowest = min(lowest, p);
    max_profit = max(max_profit, p - lowest);
  }
  return max_profit;
}

int main(){
  V t1 = {7,1,5,3,6,4};
  V t2 = {7,6,3,2,1};
  cout << maxprofit(t2);
}
