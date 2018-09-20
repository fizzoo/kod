int max3(int a, int b, int c){
    return max(a, max(b, c));
}

int min3(int a, int b, int c){
    return min(a, min(b, c));
}

int Solution::solve(vector<int> &A, vector<int> &B, vector<int> &C) {
    auto a = A.begin(), b = B.begin(), c = C.begin();
    auto ae = A.end(), be = B.end(), ce = C.end();
    sort(a, ae);
    sort(b, be);
    sort(c, ce);
    
    int mini = INT_MAX;
    while(a != ae && b != be && c != ce){
        mini = min(mini, max3(*a, *b, *c)-min3(*a, *b, *c));
        if (*a <= *b && *a <= *c){
            ++a;
        } else if (*b <= *a && *b <= *c){
            ++b;
        } else {
            ++c;
        }
    }
    
    return mini;
}
