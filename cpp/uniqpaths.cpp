#include "mats.hpp"

// m x n grid, can move down or right, how many unique paths are there?

int step(int y, int x, Mat *lens){
  auto len = (*lens)[y][x];
  if (len >= 0) {
    return len;
  }

  len = step(y+1, x, lens) + step(y, x+1, lens);
  (*lens)[y][x] = len;
  return len;
}

void fill_rhs(Mat *lens){
  auto maxy = lens->size();
  auto maxx = (*lens)[0].size();

  for (int y = 0; y < maxy; ++y){
    (*lens)[y][maxx-1] = 1;
  }
  for (int x = 0; x < maxx; ++x){
    (*lens)[maxy-1][x] = 1;
  }

}

int uniqpaths(int m, int n){
  Mat lens = init_mat(n, m, -1);
  fill_rhs(&lens);
  return step(0, 0, &lens);
}

int main(){
  Mat lens = init_mat(3, 7, -1);
  fill_rhs(&lens);
  auto len = step(0, 0, &lens);
  print_mat(lens);
  cout << len;
}
