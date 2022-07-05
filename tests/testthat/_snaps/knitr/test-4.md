
Test cpp11 integration.

``` cpp
double mean_cpp(doubles x) {
  int n = x.size();
  double total = 0;
  for (double value : x) {
     total += value;
  }
  return total / n;
}
```

![](test-4_files/figure-gfm//1-cpp11-mean.svg)<!-- -->
