#include "mats.hpp"

// m x n grid, can move down or right, how many unique paths are there? With obstacles, so input is a matrix instead.

int step(int y, int x, Mat *lens){
  auto len = (*lens)[y][x];
  if (len >= 0) {
    return len;
  }

  len = step(y+1, x, lens) + step(y, x+1, lens);
  (*lens)[y][x] = len;
  return len;
}

void set_obstacles(const Mat *obst, Mat *lens){
  auto maxy = lens->size();
  auto maxx = (*lens)[0].size();

  for (int y = 0; y < maxy; ++y) {
    for (int x = 0; x < maxx; ++x) {
      if ((*obst)[y][x] == 1){
        (*lens)[y][x] = 0;
      }
    }
  }
}

void fill_rhs(Mat *lens){
  auto maxy = lens->size();
  auto maxx = (*lens)[0].size();

  int val = 1;
  for (int y = maxy-1; y >= 0; --y){
    if ((*lens)[y][maxx-1] == 0){
      val = 0;
    }
    (*lens)[y][maxx-1] = val;
  }
  val = 1;
  for (int x = maxx-1; x >= 0; --x){
    if ((*lens)[maxy-1][x] == 0){
      val = 0;
    }
    (*lens)[maxy-1][x] = val;
  }

}

int uniqpaths(Mat obst){
  Mat lens = init_like(obst, -1);
  set_obstacles(&obst, &lens);
  fill_rhs(&lens);
  return step(0, 0, &lens);
}

int main(){
  Mat obst = init_mat(3, 3, 0);
  obst[1][1] = 1;
  Mat lens = init_like(obst, -1);
  set_obstacles(&obst, &lens);
  fill_rhs(&lens);
  auto len = step(0, 0, &lens);
  print_mat(obst);
  print_mat(lens);
  cout << len;
}
