Algorytm mrówkowy w sortowanie tablicy
========================================================

```r
source("mrowki.sort.tests.r")
```

Zastosowanie algorytmu mrówkowego do problemu sortowania tablicy z różnymi parametrami algorytmu (ilość mrówek, początkowa wielkość feromonów, współczynnik degradacji feromonów) oraz różnej instancji problemu



1. Wielkosc problemu 7
----------

1.1 Test 1: 
_____
  - Ilość mrówek = 1, 
  - współczynnik degradacji 0.9,
  - startowe feromony 0.5


```r
mrowki.sort.test(7, 1, 0.9, 0.5)
```

```
## [1] "Sortujemy!"
## [1]  4.306  8.101 -3.892  2.435  8.997 -4.027 -5.539
## [1] "=============="
## [1] -5.539 -4.027 -3.892  2.435  4.306  8.101  8.997
## [1] "Sortujemy!"
## [1] -2.2026 -6.1260 -9.6779  0.2162 -1.4022  9.2374 -1.9743
## [1] "=============="
## [1] -9.6779 -6.1260 -2.2026 -1.9743 -1.4022  0.2162  9.2374
## [1] "Sortujemy!"
## [1] -4.4483 -6.8017 -3.9393 -9.9188 -8.0420 -0.8286 -1.3120
## [1] "=============="
## [1] -9.9188 -8.0420 -6.8017 -4.4483 -3.9393 -1.3120 -0.8286
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-21.png) ![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-22.png) ![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-23.png) 


1.2 Test 2: 
_____
  - Ilość mrówek = 2, 
  - współczynnik degradacji 0.9,
  - startowe feromony 0.5


```r
mrowki.sort.test(7, 2, 0.9, 0.5)
```

```
## [1] "Sortujemy!"
## [1] -2.1465  3.6027 -4.1823 -5.7819 -2.3496 -0.4308  1.7946
## [1] "=============="
## [1] -5.7819 -4.1823 -2.3496 -2.1465 -0.4308  1.7946  3.6027
## [1] "Sortujemy!"
## [1] -3.849 -7.082  4.794 -1.684  5.155  1.708  8.067
## [1] "=============="
## [1] -7.082 -3.849 -1.684  1.708  4.794  5.155  8.067
## [1] "Sortujemy!"
## [1]  7.6740  2.5412 -4.3067  0.5175  1.5974  5.1437  9.1985
## [1] "=============="
## [1] -4.3067  0.5175  1.5974  2.5412  5.1437  7.6740  9.1985
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-31.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-32.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-33.png) 


1.3 Test 3: 
_____
  - Ilość mrówek = 4, 
  - współczynnik degradacji 0.9,
  - startowe feromony 0.5


```r
mrowki.sort.test(7, 4, 0.9, 0.5)
```

```
## [1] "Sortujemy!"
## [1] -8.492 -6.371  3.918 -8.145 -2.800 -1.822 -6.695
## [1] "=============="
## [1] -8.492 -8.145 -6.695 -6.371 -2.800 -1.822  3.918
## [1] "Sortujemy!"
## [1]  2.078  3.206  3.072  3.956 -4.740  7.731 -8.500
## [1] "=============="
## [1] -8.500 -4.740  2.078  3.072  3.206  3.956  7.731
## [1] "Sortujemy!"
## [1]  0.6793  1.5498 -1.1863 -8.3361  6.7752 -3.9927 -9.5055
## [1] "=============="
## [1] -9.5055 -8.3361 -3.9927 -1.1863  0.6793  1.5498  6.7752
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-41.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-42.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-43.png) 


1.4 Test 4: 
_____
  - Ilość mrówek = 10, 
  - współczynnik degradacji 0.9,
  - startowe feromony 0.5


```r
mrowki.sort.test(7, 10, 0.9, 0.5)
```

```
## [1] "Sortujemy!"
## [1] -4.95568  4.98036 -3.76299  5.35347 -0.06085  5.94600  9.25620
## [1] "=============="
## [1] -4.95568 -3.76299 -0.06085  4.98036  5.35347  5.94600  9.25620
## [1] "Sortujemy!"
## [1] -0.5414  6.5569 -1.9914  9.4349  2.5851 -5.8778 -1.4818
## [1] "=============="
## [1] -5.8778 -1.9914 -1.4818 -0.5414  2.5851  6.5569  9.4349
## [1] "Sortujemy!"
## [1]  7.5922 -0.4763  7.7100 -1.2862 -8.9837 -7.2558  0.1974
## [1] "=============="
## [1] -8.9837 -7.2558 -1.2862 -0.4763  0.1974  7.5922  7.7100
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-51.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-52.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-53.png) 


1.5 Test 5: 
_____
  - Ilość mrówek = 2, 
  - współczynnik degradacji 0.9,
  - startowe feromony 0.5


```r
mrowki.sort.test(7, 2, 0.9, 0.5)
```

```
## [1] "Sortujemy!"
## [1]  5.6520 -7.6098 -4.6324  5.9174  4.8189  1.3529  0.7379
## [1] "=============="
## [1] -7.6098 -4.6324  0.7379  1.3529  4.8189  5.6520  5.9174
## [1] "Sortujemy!"
## [1] -2.1323  2.6372 -1.4991  7.3186  2.1169 -7.5195  0.7153
## [1] "=============="
## [1] -7.5195 -2.1323 -1.4991  0.7153  2.1169  2.6372  7.3186
## [1] "Sortujemy!"
## [1]  0.4816  2.5151  8.4777  5.9395 -0.4381 -0.7520 -8.3676
## [1] "=============="
## [1] -8.3676 -0.7520 -0.4381  0.4816  2.5151  5.9395  8.4777
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-61.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-62.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-63.png) 


1.6 Test 6: 
_____
  - Ilość mrówek = 2, 
  - współczynnik degradacji 0.76,
  - startowe feromony 0.5


```r
mrowki.sort.test(7, 2, 0.75, 0.5)
```

```
## [1] "Sortujemy!"
## [1]  3.275 -5.844  0.704 -2.254 -4.331 -1.441  1.641
## [1] "=============="
## [1] -5.844 -4.331 -2.254 -1.441  0.704  1.641  3.275
## [1] "Sortujemy!"
## [1] -1.419  6.069  6.965  1.273 -7.444 -2.500  5.681
## [1] "=============="
## [1] -7.444 -2.500 -1.419  1.273  5.681  6.069  6.965
## [1] "Sortujemy!"
## [1]  9.016 -1.376 -3.012 -1.870  3.272  1.688 -2.495
## [1] "=============="
## [1] -3.012 -2.495 -1.870 -1.376  1.688  3.272  9.016
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-71.png) ![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-72.png) ![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-73.png) 


1.7 Test 7: 
_____
  - Ilość mrówek = 2, 
  - współczynnik degradacji 0.6,
  - startowe feromony 0.5


```r
mrowki.sort.test(7, 2, 0.5, 0.5)
```

```
## [1] "Sortujemy!"
## [1] -4.0600  0.4673 -7.6145  0.4739 -8.9904 -2.3781  3.1826
## [1] "=============="
## [1] -8.9904 -7.6145 -4.0600 -2.3781  0.4673  0.4739  3.1826
## [1] "Sortujemy!"
## [1]  6.9930 -6.3606  8.5199 -2.4700  0.5046 -6.7956 -3.0976
## [1] "=============="
## [1] -6.7956 -6.3606 -3.0976 -2.4700  0.5046  6.9930  8.5199
## [1] "Sortujemy!"
## [1]  8.6970 -8.2957  5.4695  8.7141 -2.0572  0.9615  9.0904
## [1] "=============="
## [1] -8.2957 -2.0572  0.9615  5.4695  8.6970  8.7141  9.0904
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-81.png) ![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-82.png) ![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-83.png) 


1.8 Test 8: 
_____
  - Ilość mrówek = 2, 
  - współczynnik degradacji 0.25,
  - startowe feromony 0.5


```r
mrowki.sort.test(7, 2, 0.25, 0.5)
```

```
## [1] "Sortujemy!"
## [1] -9.267 -3.295 -6.152 -0.784 -1.276  2.160  3.243
## [1] "=============="
## [1] -9.267 -6.152 -3.295 -1.276 -0.784  2.160  3.243
## [1] "Sortujemy!"
## [1] -4.787 -5.280  8.955  4.847 -1.081 -5.833  4.928
## [1] "=============="
## [1] -5.833 -5.280 -4.787 -1.081  4.847  4.928  8.955
## [1] "Sortujemy!"
## [1]  9.1248 -1.6356  6.7549  6.9910 -0.9222 -0.1420 -1.2518
## [1] "=============="
## [1] -1.6356 -1.2518 -0.9222 -0.1420  6.7549  6.9910  9.1248
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-91.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-92.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-93.png) 


1.9 Test 9: 
_____
  - Ilość mrówek = 2, 
  - współczynnik degradacji 0.5,
  - startowe feromony 0.9


```r
mrowki.sort.test(7, 1, 0.5, 0.9)
```

```
## [1] "Sortujemy!"
## [1]  8.656  3.734  5.399 -2.278  3.168  2.099  8.901
## [1] "=============="
## [1] -2.278  2.099  3.168  3.734  5.399  8.656  8.901
## [1] "Sortujemy!"
## [1] -5.090  1.315 -6.117  9.292  7.076 -4.107  6.958
## [1] "=============="
## [1] -6.117 -5.090 -4.107  1.315  6.958  7.076  9.292
## [1] "Sortujemy!"
## [1] -4.0990 -3.6676  1.0647 -1.1916 -3.9344  2.4155  0.6362
## [1] "=============="
## [1] -4.0990 -3.9344 -3.6676 -1.1916  0.6362  1.0647  2.4155
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-101.png) ![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-102.png) ![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-103.png) 


1.10 Test 10: 
_____
  - Ilość mrówek = 2, 
  - współczynnik degradacji 0.5,
  - startowe feromony 0.75


```r
mrowki.sort.test(7, 2, 0.5, 0.75)
```

```
## [1] "Sortujemy!"
## [1]  8.818  7.444 -3.754  7.104 -9.323  4.462 -5.356
## [1] "=============="
## [1] -9.323 -5.356 -3.754  4.462  7.104  7.444  8.818
## [1] "Sortujemy!"
## [1]  0.9525 -4.6203 -0.1192  9.7358 -1.4484  5.9411  7.6235
## [1] "=============="
## [1] -4.6203 -1.4484 -0.1192  0.9525  5.9411  7.6235  9.7358
## [1] "Sortujemy!"
## [1]  7.255 -9.987  9.615 -3.564  1.973  5.406  1.329
## [1] "=============="
## [1] -9.987 -3.564  1.329  1.973  5.406  7.255  9.615
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-111.png) ![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-112.png) ![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-113.png) 


1.11 Test 11: 
_____
  - Ilość mrówek = 2, 
  - współczynnik degradacji 0.5,
  - startowe feromony 0.5


```r
mrowki.sort.test(7, 2, 0.5, 0.5)
```

```
## [1] "Sortujemy!"
## [1]  2.509  6.303  8.872  6.136  9.129 -5.235 -9.557
## [1] "=============="
## [1] -9.557 -5.235  2.509  6.136  6.303  8.872  9.129
## [1] "Sortujemy!"
## [1]  6.2812 -0.6024 -9.0912 -2.3175  9.9692 -7.8005 -6.3475
## [1] "=============="
## [1] -9.0912 -7.8005 -6.3475 -2.3175 -0.6024  6.2812  9.9692
## [1] "Sortujemy!"
## [1]  3.5986  5.5584 -5.7759  0.4325 -4.8784  6.1481 -4.4585
## [1] "=============="
## [1] -5.7759 -4.8784 -4.4585  0.4325  3.5986  5.5584  6.1481
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-121.png) ![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-122.png) ![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-123.png) 


1.12 Test 12: 
_____
  - Ilość mrówek = 2, 
  - współczynnik degradacji 0.5,
  - startowe feromony 0.25


```r
mrowki.sort.test(7, 2, 0.5, 0.25)
```

```
## [1] "Sortujemy!"
## [1] -6.567  3.116  6.481  6.848 -7.115  9.343  2.813
## [1] "=============="
## [1] -7.115 -6.567  2.813  3.116  6.481  6.848  9.343
## [1] "Sortujemy!"
## [1] -8.130  1.029 -8.924  1.208  4.004 -7.549 -8.705
## [1] "=============="
## [1] -8.924 -8.705 -8.130 -7.549  1.029  1.208  4.004
## [1] "Sortujemy!"
## [1] -9.087  8.736 -2.318 -4.642 -4.761 -2.119 -9.101
## [1] "=============="
## [1] -9.101 -9.087 -4.761 -4.642 -2.318 -2.119  8.736
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-131.png) ![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-132.png) ![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-133.png) 

