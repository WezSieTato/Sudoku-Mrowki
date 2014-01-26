Algorytm mrówkowy w sortowanie tablicy
========================================================

```r
source("mrowki.sort.tests.r")
```

```
## [1] "Sortujemy!"
## [1]  9.141 -4.626  7.524 -1.181  4.746
## [1] "=============="
## [1] -4.626 -1.181  4.746  7.524  9.141
```

Zastosowanie algorytmu mrówkowego do problemu sortowania tablicy z różnymi parametrami algorytmu (ilość mrówek, początkowa wielkość feromonów, współczynnik degradacji feromonów) oraz różnej instancji problemu



```r
mrowki.sort.test(5, 1, 0.3, 0.6)
```

```
## [1] "Sortujemy!"
## [1] -8.891 -7.830 -5.113 -3.192  3.656
## [1] "=============="
## [1] -8.891 -7.830 -5.113 -3.192  3.656
```



