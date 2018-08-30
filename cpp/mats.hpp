#include <iostream>
#include <vector>
using namespace std;

typedef vector<vector<int>> Mat;

Mat init_mat(int ny, int nx, int val){
  return vector<vector<int>>(ny, vector<int>(nx, val));
}

Mat init_like(const Mat &m, int val){
  auto ny = m.size();
  auto nx = m[0].size();
  return init_mat(ny, nx, val);
}

void print_mat(const Mat& m){
  for(int y = 0; y < m.size(); ++y){
    for(int x = 0; x < m[0].size(); ++x){
      cout << m[y][x] << " ";
    }
    cout << endl;
  }
  cout << endl;
}
