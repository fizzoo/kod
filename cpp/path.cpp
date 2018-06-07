#include "mats.hpp"

// Given a matrix, find the longest increasing path (N/S/E/W, can turn at any point) in O(n*m).

int step(int y, int x, const Mat *vals, Mat *lens){
  auto val = (*vals)[y][x];
  auto len = (*lens)[y][x];

  if (len > 0){
    return len;
  }

  auto maxy = vals->size() - 1;
  auto maxx = (*vals)[0].size() - 1;

  if (x < maxx && (*vals)[y][x+1] > val){
    len = max(len, step(y, x+1, vals, lens) +1);
  }
  if (x > 0 && (*vals)[y][x-1] > val){
    len = max(len, step(y, x-1, vals, lens) +1);
  }
  if (y < maxy && (*vals)[y+1][x] > val){
    len = max(len, step(y+1, x, vals, lens) +1);
  }
  if (y > 0 && (*vals)[y-1][x] > val){
    len = max(len, step(y-1, x, vals, lens) +1);
  }

  (*lens)[y][x] = len;
  return len;
}

void zero_if_highest(int y, int x, const Mat *vals, Mat *lens){
  auto val = (*vals)[y][x];
  auto maxy = vals->size() - 1;
  auto maxx = (*vals)[0].size() - 1;

  bool highest = true;
  if (x < maxx && (*vals)[y][x+1] > val){
    highest = false;
  }
  if (x > 0 && (*vals)[y][x-1] > val){
    highest = false;
  }
  if (y < maxy && (*vals)[y+1][x] > val){
     highest = false;
   }
   if (y > 0 && (*vals)[y-1][x] > val){
     highest = false;
   }

   if (highest){
     (*lens)[y][x] = 0;
   }
}

int main(){
  auto test1 = init_mat(5, 5, 0);
  for(int y = 0; y < 5; ++y){
    for(int x = 0; x < 5; ++x){
      test1[y][x] = y*5 + x;
    }
  }

  auto test2 = test1;
  test2[0][2] = 0;
  test2[1][2] = 0;
  test2[2][2] = 0;
  test2[2][1] = 0;
  test2[2][0] = 0;
  test2[4][4] = 0;

  auto test3 = init_mat(4, 4, 0);
  test3[0][1] = 1;
  test3[0][2] = 2;
  test3[0][3] = 3;
  test3[1][3] = 4;
  test3[2][3] = 5;
  test3[3][3] = 6;
  test3[3][2] = 7;
  test3[3][1] = 8;
  test3[3][0] = 9;
  test3[2][0] = 10;
  test3[1][0] = 11;
  test3[1][1] = 12;
  test3[1][2] = 13;
  test3[2][2] = 14;
  test3[2][1] = 15;

  auto inp = test3;
  auto lens = init_like(inp, -1);

  for(int y = 0; y < inp.size(); ++y){
    for(int x = 0; x < inp[0].size(); ++x){
      zero_if_highest(y, x, &inp, &lens);
    }
  }
  for(int y = 0; y < inp.size(); ++y){
    for(int x = 0; x < inp[0].size(); ++x){
      step(y, x, &inp, &lens);
    }
  }
  int longest = -1;
  for(int y = 0; y < inp.size(); ++y){
    for(int x = 0; x < inp[0].size(); ++x){
      longest = max(longest, lens[y][x]);
    }
  }

  print_mat(inp);
  print_mat(lens);
  cout << longest;
}
