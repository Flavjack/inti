# Remove outliers

Use the method M4 in Bernal Vasquez (2016). Bonferroni Holm test to
judge residuals standardized by the re scaled MAD (BH MADR).

## Usage

``` r
outliers_remove(data, trait, model, drop_na = TRUE)
```

## Arguments

- data:

  Experimental design data frame with the factors and traits.

- trait:

  Name of the trait.

- model:

  The fixed or random effects in the model.

- drop_na:

  drop NA values from the data.frame

## Value

list. 1. Table with date without outliers. 2. The outliers in the
dataset.

## Details

Function to remove outliers in MET experiments

## References

Bernal Vasquez, Angela Maria, et al. “Outlier Detection Methods for
Generalized Lattices: A Case Study on the Transition from ANOVA to
REML.” Theoretical and Applied Genetics, vol. 129, no. 4, Apr. 2016.

## Examples

``` r
library(inti)

rmout <- potato %>% outliers_remove(
  data = .
  , trait ="stemdw"
  , model = "0 + treat*geno + (1|bloque)"
  , drop_na = FALSE
  )

rmout
#> $data
#>        treat geno bloque stemdw
#> 1     sequia  G01     II  14.87
#> 2     sequia  G02     IV   8.63
#> 3   irrigado  G01    III     NA
#> 4     sequia  G02      I   6.58
#> 5   irrigado  G03     II  12.63
#> 6   irrigado  G04      V  17.46
#> 7   irrigado  G01      I  15.32
#> 8   irrigado  G05     IV  14.55
#> 9     sequia  G06     II  21.19
#> 10    sequia  G05      I     NA
#> 11  irrigado  G01     II  18.13
#> 12    sequia  G07     II   3.70
#> 13  irrigado  G08     II  12.48
#> 14  irrigado  G06    III  29.49
#> 15  irrigado  G09    III  16.96
#> 16  irrigado  G10     II   8.20
#> 17    sequia  G11      I   7.90
#> 18    sequia  G12    III   9.19
#> 19  irrigado  G07      I   2.48
#> 20  irrigado  G04     II  20.75
#> 21  irrigado  G13     II  18.97
#> 22  irrigado  G14    III  14.57
#> 23  irrigado  G04     IV  18.84
#> 24    sequia  G04      V   8.79
#> 25    sequia  G08      V   8.17
#> 26    sequia  G04    III  12.53
#> 27    sequia  G01     IV  16.26
#> 28  irrigado  G10      I  11.19
#> 29  irrigado  G08      V  11.18
#> 30  irrigado  G02      V  12.14
#> 31  irrigado  G07    III   4.78
#> 32  irrigado  G08      I  12.52
#> 33  irrigado  G14      V  23.96
#> 34  irrigado  G03      I  11.18
#> 35    sequia  G13    III   7.79
#> 36    sequia  G01      V  11.97
#> 37    sequia  G03      I   9.03
#> 38  irrigado  G15    III  11.17
#> 39  irrigado  G03     IV  12.20
#> 40  irrigado  G09     IV  18.17
#> 41  irrigado  G11     II   4.90
#> 42    sequia  G03      V   8.73
#> 43    sequia  G11    III   5.56
#> 44  irrigado  G06      V  23.77
#> 45    sequia  G05      V     NA
#> 46    sequia  G08     IV   8.44
#> 47  irrigado  G11     IV   7.53
#> 48    sequia  G11     II   3.11
#> 49  irrigado  G10    III  14.77
#> 50    sequia  G06     IV  17.45
#> 51    sequia  G09      I  13.36
#> 52  irrigado  G11      I   7.27
#> 53    sequia  G11     IV   5.72
#> 54  irrigado  G15     IV  11.76
#> 55  irrigado  G13     IV  19.83
#> 56    sequia  G14      V  12.94
#> 57  irrigado  G02     IV  14.01
#> 58  irrigado  G09     II  19.20
#> 59  irrigado  G02    III  12.12
#> 60    sequia  G08    III  10.10
#> 61  irrigado  G06     II  24.35
#> 62    sequia  G13     IV  11.52
#> 63    sequia  G14    III  13.37
#> 64    sequia  G04     II  15.02
#> 65  irrigado  G11    III  10.32
#> 66  irrigado  G07     II   1.71
#> 67  irrigado  G08     IV  14.28
#> 68    sequia  G05     IV     NA
#> 69  irrigado  G04      I  12.80
#> 70  irrigado  G11      V   7.99
#> 71  irrigado  G12      I  19.60
#> 72    sequia  G14     IV  13.97
#> 73    sequia  G07    III   3.09
#> 74  irrigado  G03    III   8.56
#> 75    sequia  G01      I  10.44
#> 76    sequia  G04      I  13.73
#> 77    sequia  G03     II   8.33
#> 78  irrigado  G15     II  11.78
#> 79    sequia  G12     IV  12.30
#> 80    sequia  G12      I  13.91
#> 81    sequia  G08      I   5.14
#> 82    sequia  G05     II     NA
#> 83    sequia  G02     II   8.46
#> 84    sequia  G10      I   9.84
#> 85    sequia  G15      I  11.43
#> 86  irrigado  G07      V   1.71
#> 87    sequia  G10      V   6.36
#> 88    sequia  G13     II  12.34
#> 89    sequia  G07      V   2.71
#> 90    sequia  G03    III   7.16
#> 91    sequia  G15     IV  11.19
#> 92    sequia  G13      I  12.23
#> 93    sequia  G03     IV   8.37
#> 94  irrigado  G10      V  11.74
#> 95    sequia  G13      V  11.82
#> 96    sequia  G09     II  17.02
#> 97  irrigado  G14     IV  17.89
#> 98  irrigado  G01      V  13.80
#> 99    sequia  G01    III  15.37
#> 100 irrigado  G06     IV  33.52
#> 101   sequia  G04     IV  12.56
#> 102 irrigado  G15      V  12.13
#> 103 irrigado  G13    III  17.36
#> 104 irrigado  G02     II  12.58
#> 105   sequia  G08     II  10.31
#> 106 irrigado  G04    III  19.29
#> 107   sequia  G02      V   8.39
#> 108   sequia  G06      V  13.12
#> 109 irrigado  G15      I  12.14
#> 110 irrigado  G13      V  18.16
#> 111 irrigado  G05      V  12.03
#> 112   sequia  G09    III  16.71
#> 113   sequia  G09      V  10.97
#> 114   sequia  G10     II   7.44
#> 115 irrigado  G07     IV   4.06
#> 116 irrigado  G05      I  13.07
#> 117 irrigado  G02      I   8.54
#> 118   sequia  G05    III     NA
#> 119 irrigado  G12     II  17.81
#> 120   sequia  G15    III  10.95
#> 121 irrigado  G13      I  16.27
#> 122   sequia  G14     II  17.86
#> 123   sequia  G12     II  16.82
#> 124   sequia  G15     II  11.82
#> 125 irrigado  G09      V  14.22
#> 126   sequia  G06      I  16.22
#> 127   sequia  G09     IV  14.02
#> 128   sequia  G15      V  10.32
#> 129 irrigado  G14      I  19.93
#> 130   sequia  G06    III  17.45
#> 131 irrigado  G01     IV  16.97
#> 132 irrigado  G12    III  19.78
#> 133   sequia  G12      V  14.22
#> 134 irrigado  G12      V  17.61
#> 135   sequia  G11      V   3.95
#> 136 irrigado  G12     IV  19.87
#> 137 irrigado  G09      I  16.05
#> 138   sequia  G02    III   9.76
#> 139   sequia  G07      I   2.97
#> 140 irrigado  G08    III  11.61
#> 141 irrigado  G06      I  26.46
#> 142 irrigado  G10     IV     NA
#> 143 irrigado  G03      V  10.76
#> 144   sequia  G07     IV   0.97
#> 145 irrigado  G05    III  15.19
#> 146   sequia  G14      I  10.62
#> 147   sequia  G10    III  11.27
#> 148 irrigado  G14     II  17.86
#> 149 irrigado  G05     II  16.57
#> 150   sequia  G10     IV   6.58
#> 
#> $outliers
#>        treat geno bloque stemdw       resi   res_MAD  rawp.BHStud index
#> 3   irrigado  G01    III  24.19   6.520276  4.031041 5.553035e-05     3
#> 10    sequia  G05      I  11.14 -13.467719 -8.326170 0.000000e+00    10
#> 45    sequia  G05      V  11.52 -13.006525 -8.041046 8.881784e-16    45
#> 68    sequia  G05     IV  80.65  54.860861 33.916722 0.000000e+00    68
#> 82    sequia  G05     II  11.65 -13.422893 -8.298457 0.000000e+00    82
#> 118   sequia  G05    III  10.02 -14.963724 -9.251048 0.000000e+00   118
#> 142 irrigado  G10     IV   5.03  -5.949139 -3.677946 2.351195e-04   142
#>             adjp        bholm out_flag
#> 3   5.553035e-05 8.051901e-03  OUTLIER
#> 10  0.000000e+00 0.000000e+00  OUTLIER
#> 45  8.881784e-16 1.296740e-13  OUTLIER
#> 68  0.000000e+00 0.000000e+00  OUTLIER
#> 82  0.000000e+00 0.000000e+00  OUTLIER
#> 118 0.000000e+00 0.000000e+00  OUTLIER
#> 142 2.351195e-04 3.385720e-02  OUTLIER
#> 
  
```
