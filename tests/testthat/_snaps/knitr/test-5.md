
Simple SVG image at end

<div class="asciicast">

<pre>
##  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s"                    
## [20] "t" "u" "v" "w" "x" "y" "z"                                                                    
##                                                                                                     
</pre>

</div>

`eval = FALSE` works. Need to `echo = TRUE` explicitly, the default is
no echo.

``` r
stop("do not run me")
```

Config in header is OK.

<div class="asciicast">

<pre>
##  [55]  55  56  57  58  59  60  61  62  6
## 3  64  65  66  67  68  69  70  71  72   
##  [73]  73  74  75  76  77  78  79  80  8
## 1  82  83  84  85  86  87  88  89  90   
##  [91]  91  92  93  94  95  96  97  98  9
## 9 100                                   
##                                         
</pre>

</div>

`eval = TRUE` + `echo = TRUE`

``` r
1:10
```

<div class="asciicast">

<pre>
##  [1]  1  2  3  4  5  6  7  8  9 10                                                                  
##                                                                                                     
</pre>

</div>

`include = FALSE`
