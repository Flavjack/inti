# Remove outliers using mixed models

Use the method M4 in Bernal Vasquez (2016). Bonferroni Holm test to
judge residuals standardized by the re scaled MAD (BH MADR).

## Usage

``` r
remove_outliers(data, formula, drop_na = FALSE, plot_diag = FALSE)
```

## Arguments

- data:

  Experimental design data frame with the factors and traits.

- formula:

  mixed model formula.

- drop_na:

  drop NA values from the data.frame

- plot_diag:

  Diagnostic plot based in the raw and clean data

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

rmout <- potato %>%
  remove_outliers(data = .
  , formula = stemdw ~ 0 + (1|bloque) + treat*geno
  , plot_diag = FALSE
  , drop_na = FALSE
  )
#> fixed-effect model matrix is rank deficient so dropping 1 column / coefficient

rmout
#> $data
#> $data$raw
#>     index bloque    treat geno stemdw
#> 1       1     II   sequia  G01  14.87
#> 2       2     IV   sequia  G02   8.63
#> 3       3    III irrigado  G01  24.19
#> 4       4      I   sequia  G02   6.58
#> 5       5     II irrigado  G03  12.63
#> 6       6      V irrigado  G04  17.46
#> 7       7      I irrigado  G01  15.32
#> 8       8     IV irrigado  G05  14.55
#> 9       9     II   sequia  G06  21.19
#> 10     10      I   sequia  G05  11.14
#> 11     11     II irrigado  G01  18.13
#> 12     12     II   sequia  G07   3.70
#> 13     13     II irrigado  G08  12.48
#> 14     14    III irrigado  G06  29.49
#> 15     15    III irrigado  G09  16.96
#> 16     16     II irrigado  G10   8.20
#> 17     17      I   sequia  G11   7.90
#> 18     18    III   sequia  G12   9.19
#> 19     19      I irrigado  G07   2.48
#> 20     20     II irrigado  G04  20.75
#> 21     21     II irrigado  G13  18.97
#> 22     22    III irrigado  G14  14.57
#> 23     23     IV irrigado  G04  18.84
#> 24     24      V   sequia  G04   8.79
#> 25     25      V   sequia  G08   8.17
#> 26     26    III   sequia  G04  12.53
#> 27     27     IV   sequia  G01  16.26
#> 28     28      I irrigado  G10  11.19
#> 29     29      V irrigado  G08  11.18
#> 30     30      V irrigado  G02  12.14
#> 31     31    III irrigado  G07   4.78
#> 32     32      I irrigado  G08  12.52
#> 33     33      V irrigado  G14  23.96
#> 34     34      I irrigado  G03  11.18
#> 35     35    III   sequia  G13   7.79
#> 36     36      V   sequia  G01  11.97
#> 37     37      I   sequia  G03   9.03
#> 38     38    III irrigado  G15  11.17
#> 39     39     IV irrigado  G03  12.20
#> 40     40     IV irrigado  G09  18.17
#> 41     41     II irrigado  G11   4.90
#> 42     42      V   sequia  G03   8.73
#> 43     43    III   sequia  G11   5.56
#> 44     44      V irrigado  G06  23.77
#> 45     45      V   sequia  G05  11.52
#> 46     46     IV   sequia  G08   8.44
#> 47     47     IV irrigado  G11   7.53
#> 48     48     II   sequia  G11   3.11
#> 49     49    III irrigado  G10  14.77
#> 50     50     IV   sequia  G06  17.45
#> 51     51      I   sequia  G09  13.36
#> 52     52      I irrigado  G11   7.27
#> 53     53     IV   sequia  G11   5.72
#> 54     54     IV irrigado  G15  11.76
#> 55     55     IV irrigado  G13  19.83
#> 56     56      V   sequia  G14  12.94
#> 57     57     IV irrigado  G02  14.01
#> 58     58     II irrigado  G09  19.20
#> 59     59    III irrigado  G02  12.12
#> 60     60    III   sequia  G08  10.10
#> 61     61     II irrigado  G06  24.35
#> 62     62     IV   sequia  G13  11.52
#> 63     63    III   sequia  G14  13.37
#> 64     64     II   sequia  G04  15.02
#> 65     65    III irrigado  G11  10.32
#> 66     66     II irrigado  G07   1.71
#> 67     67     IV irrigado  G08  14.28
#> 68     68     IV   sequia  G05  80.65
#> 69     69      I irrigado  G04  12.80
#> 70     70      V irrigado  G11   7.99
#> 71     71      I irrigado  G12  19.60
#> 72     72     IV   sequia  G14  13.97
#> 73     73    III   sequia  G07   3.09
#> 74     74    III irrigado  G03   8.56
#> 75     75      I   sequia  G01  10.44
#> 76     76      I   sequia  G04  13.73
#> 77     77     II   sequia  G03   8.33
#> 78     78     II irrigado  G15  11.78
#> 79     79     IV   sequia  G12  12.30
#> 80     80      I   sequia  G12  13.91
#> 81     81      I   sequia  G08   5.14
#> 82     82     II   sequia  G05  11.65
#> 83     83     II   sequia  G02   8.46
#> 84     84      I   sequia  G10   9.84
#> 85     85      I   sequia  G15  11.43
#> 86     86      V irrigado  G07   1.71
#> 87     87      V   sequia  G10   6.36
#> 88     88     II   sequia  G13  12.34
#> 89     89      V   sequia  G07   2.71
#> 90     90    III   sequia  G03   7.16
#> 91     91     IV   sequia  G15  11.19
#> 92     92      I   sequia  G13  12.23
#> 93     93     IV   sequia  G03   8.37
#> 94     94      V irrigado  G10  11.74
#> 95     95      V   sequia  G13  11.82
#> 96     96     II   sequia  G09  17.02
#> 97     97     IV irrigado  G14  17.89
#> 98     98      V irrigado  G01  13.80
#> 99     99    III   sequia  G01  15.37
#> 100   100     IV irrigado  G06  33.52
#> 101   101     IV   sequia  G04  12.56
#> 102   102      V irrigado  G15  12.13
#> 103   103    III irrigado  G13  17.36
#> 104   104     II irrigado  G02  12.58
#> 105   105     II   sequia  G08  10.31
#> 106   106    III irrigado  G04  19.29
#> 107   107      V   sequia  G02   8.39
#> 108   108      V   sequia  G06  13.12
#> 109   109      I irrigado  G15  12.14
#> 110   110      V irrigado  G13  18.16
#> 111   111      V irrigado  G05  12.03
#> 112   112    III   sequia  G09  16.71
#> 113   113      V   sequia  G09  10.97
#> 114   114     II   sequia  G10   7.44
#> 115   115     IV irrigado  G07   4.06
#> 116   116      I irrigado  G05  13.07
#> 117   117      I irrigado  G02   8.54
#> 118   118    III   sequia  G05  10.02
#> 119   119     II irrigado  G12  17.81
#> 120   120    III   sequia  G15  10.95
#> 121   121      I irrigado  G13  16.27
#> 122   122     II   sequia  G14  17.86
#> 123   123     II   sequia  G12  16.82
#> 124   124     II   sequia  G15  11.82
#> 125   125      V irrigado  G09  14.22
#> 126   126      I   sequia  G06  16.22
#> 127   127     IV   sequia  G09  14.02
#> 128   128      V   sequia  G15  10.32
#> 129   129      I irrigado  G14  19.93
#> 130   130    III   sequia  G06  17.45
#> 131   131     IV irrigado  G01  16.97
#> 132   132    III irrigado  G12  19.78
#> 133   133      V   sequia  G12  14.22
#> 134   134      V irrigado  G12  17.61
#> 135   135      V   sequia  G11   3.95
#> 136   136     IV irrigado  G12  19.87
#> 137   137      I irrigado  G09  16.05
#> 138   138    III   sequia  G02   9.76
#> 139   139      I   sequia  G07   2.97
#> 140   140    III irrigado  G08  11.61
#> 141   141      I irrigado  G06  26.46
#> 142   142     IV irrigado  G10   5.03
#> 143   143      V irrigado  G03  10.76
#> 144   144     IV   sequia  G07   0.97
#> 145   145    III irrigado  G05  15.19
#> 146   146      I   sequia  G14  10.62
#> 147   147    III   sequia  G10  11.27
#> 148   148     II irrigado  G14  17.86
#> 149   149     II irrigado  G05  16.57
#> 150   150     IV   sequia  G10   6.58
#> 
#> $data$clean
#>     index bloque    treat geno stemdw
#> 1       1     II   sequia  G01  14.87
#> 2       2     IV   sequia  G02   8.63
#> 3       3    III irrigado  G01     NA
#> 4       4      I   sequia  G02   6.58
#> 5       5     II irrigado  G03  12.63
#> 6       6      V irrigado  G04  17.46
#> 7       7      I irrigado  G01  15.32
#> 8       8     IV irrigado  G05  14.55
#> 9       9     II   sequia  G06  21.19
#> 10     10      I   sequia  G05     NA
#> 11     11     II irrigado  G01  18.13
#> 12     12     II   sequia  G07   3.70
#> 13     13     II irrigado  G08  12.48
#> 14     14    III irrigado  G06  29.49
#> 15     15    III irrigado  G09  16.96
#> 16     16     II irrigado  G10   8.20
#> 17     17      I   sequia  G11   7.90
#> 18     18    III   sequia  G12   9.19
#> 19     19      I irrigado  G07   2.48
#> 20     20     II irrigado  G04  20.75
#> 21     21     II irrigado  G13  18.97
#> 22     22    III irrigado  G14  14.57
#> 23     23     IV irrigado  G04  18.84
#> 24     24      V   sequia  G04   8.79
#> 25     25      V   sequia  G08   8.17
#> 26     26    III   sequia  G04  12.53
#> 27     27     IV   sequia  G01  16.26
#> 28     28      I irrigado  G10  11.19
#> 29     29      V irrigado  G08  11.18
#> 30     30      V irrigado  G02  12.14
#> 31     31    III irrigado  G07   4.78
#> 32     32      I irrigado  G08  12.52
#> 33     33      V irrigado  G14  23.96
#> 34     34      I irrigado  G03  11.18
#> 35     35    III   sequia  G13   7.79
#> 36     36      V   sequia  G01  11.97
#> 37     37      I   sequia  G03   9.03
#> 38     38    III irrigado  G15  11.17
#> 39     39     IV irrigado  G03  12.20
#> 40     40     IV irrigado  G09  18.17
#> 41     41     II irrigado  G11   4.90
#> 42     42      V   sequia  G03   8.73
#> 43     43    III   sequia  G11   5.56
#> 44     44      V irrigado  G06  23.77
#> 45     45      V   sequia  G05     NA
#> 46     46     IV   sequia  G08   8.44
#> 47     47     IV irrigado  G11   7.53
#> 48     48     II   sequia  G11   3.11
#> 49     49    III irrigado  G10  14.77
#> 50     50     IV   sequia  G06  17.45
#> 51     51      I   sequia  G09  13.36
#> 52     52      I irrigado  G11   7.27
#> 53     53     IV   sequia  G11   5.72
#> 54     54     IV irrigado  G15  11.76
#> 55     55     IV irrigado  G13  19.83
#> 56     56      V   sequia  G14  12.94
#> 57     57     IV irrigado  G02  14.01
#> 58     58     II irrigado  G09  19.20
#> 59     59    III irrigado  G02  12.12
#> 60     60    III   sequia  G08  10.10
#> 61     61     II irrigado  G06  24.35
#> 62     62     IV   sequia  G13  11.52
#> 63     63    III   sequia  G14  13.37
#> 64     64     II   sequia  G04  15.02
#> 65     65    III irrigado  G11  10.32
#> 66     66     II irrigado  G07   1.71
#> 67     67     IV irrigado  G08  14.28
#> 68     68     IV   sequia  G05     NA
#> 69     69      I irrigado  G04  12.80
#> 70     70      V irrigado  G11   7.99
#> 71     71      I irrigado  G12  19.60
#> 72     72     IV   sequia  G14  13.97
#> 73     73    III   sequia  G07   3.09
#> 74     74    III irrigado  G03   8.56
#> 75     75      I   sequia  G01  10.44
#> 76     76      I   sequia  G04  13.73
#> 77     77     II   sequia  G03   8.33
#> 78     78     II irrigado  G15  11.78
#> 79     79     IV   sequia  G12  12.30
#> 80     80      I   sequia  G12  13.91
#> 81     81      I   sequia  G08   5.14
#> 82     82     II   sequia  G05     NA
#> 83     83     II   sequia  G02   8.46
#> 84     84      I   sequia  G10   9.84
#> 85     85      I   sequia  G15  11.43
#> 86     86      V irrigado  G07   1.71
#> 87     87      V   sequia  G10   6.36
#> 88     88     II   sequia  G13  12.34
#> 89     89      V   sequia  G07   2.71
#> 90     90    III   sequia  G03   7.16
#> 91     91     IV   sequia  G15  11.19
#> 92     92      I   sequia  G13  12.23
#> 93     93     IV   sequia  G03   8.37
#> 94     94      V irrigado  G10  11.74
#> 95     95      V   sequia  G13  11.82
#> 96     96     II   sequia  G09  17.02
#> 97     97     IV irrigado  G14  17.89
#> 98     98      V irrigado  G01  13.80
#> 99     99    III   sequia  G01  15.37
#> 100   100     IV irrigado  G06  33.52
#> 101   101     IV   sequia  G04  12.56
#> 102   102      V irrigado  G15  12.13
#> 103   103    III irrigado  G13  17.36
#> 104   104     II irrigado  G02  12.58
#> 105   105     II   sequia  G08  10.31
#> 106   106    III irrigado  G04  19.29
#> 107   107      V   sequia  G02   8.39
#> 108   108      V   sequia  G06  13.12
#> 109   109      I irrigado  G15  12.14
#> 110   110      V irrigado  G13  18.16
#> 111   111      V irrigado  G05  12.03
#> 112   112    III   sequia  G09  16.71
#> 113   113      V   sequia  G09  10.97
#> 114   114     II   sequia  G10   7.44
#> 115   115     IV irrigado  G07   4.06
#> 116   116      I irrigado  G05  13.07
#> 117   117      I irrigado  G02   8.54
#> 118   118    III   sequia  G05     NA
#> 119   119     II irrigado  G12  17.81
#> 120   120    III   sequia  G15  10.95
#> 121   121      I irrigado  G13  16.27
#> 122   122     II   sequia  G14  17.86
#> 123   123     II   sequia  G12  16.82
#> 124   124     II   sequia  G15  11.82
#> 125   125      V irrigado  G09  14.22
#> 126   126      I   sequia  G06  16.22
#> 127   127     IV   sequia  G09  14.02
#> 128   128      V   sequia  G15  10.32
#> 129   129      I irrigado  G14  19.93
#> 130   130    III   sequia  G06  17.45
#> 131   131     IV irrigado  G01  16.97
#> 132   132    III irrigado  G12  19.78
#> 133   133      V   sequia  G12  14.22
#> 134   134      V irrigado  G12  17.61
#> 135   135      V   sequia  G11   3.95
#> 136   136     IV irrigado  G12  19.87
#> 137   137      I irrigado  G09  16.05
#> 138   138    III   sequia  G02   9.76
#> 139   139      I   sequia  G07   2.97
#> 140   140    III irrigado  G08  11.61
#> 141   141      I irrigado  G06  26.46
#> 142   142     IV irrigado  G10     NA
#> 143   143      V irrigado  G03  10.76
#> 144   144     IV   sequia  G07   0.97
#> 145   145    III irrigado  G05  15.19
#> 146   146      I   sequia  G14  10.62
#> 147   147    III   sequia  G10  11.27
#> 148   148     II irrigado  G14  17.86
#> 149   149     II irrigado  G05  16.57
#> 150   150     IV   sequia  G10   6.58
#> 
#> 
#> $outliers
#>     index bloque    treat geno stemdw       resi   res_MAD  rawp.BHStud
#> 3       3    III irrigado  G01  24.19   6.520276  4.031041 5.553035e-05
#> 10     10      I   sequia  G05  11.14 -13.467719 -8.326170 0.000000e+00
#> 45     45      V   sequia  G05  11.52 -13.006525 -8.041046 8.881784e-16
#> 68     68     IV   sequia  G05  80.65  54.860861 33.916722 0.000000e+00
#> 82     82     II   sequia  G05  11.65 -13.422893 -8.298457 0.000000e+00
#> 118   118    III   sequia  G05  10.02 -14.963724 -9.251048 0.000000e+00
#> 142   142     IV irrigado  G10   5.03  -5.949139 -3.677946 2.351195e-04
#>             adjp        bholm out_flag
#> 3   5.553035e-05 8.051901e-03  OUTLIER
#> 10  0.000000e+00 0.000000e+00  OUTLIER
#> 45  8.881784e-16 1.296740e-13  OUTLIER
#> 68  0.000000e+00 0.000000e+00  OUTLIER
#> 82  0.000000e+00 0.000000e+00  OUTLIER
#> 118 0.000000e+00 0.000000e+00  OUTLIER
#> 142 2.351195e-04 3.385720e-02  OUTLIER
#> 
#> $diagplot
#> NULL
#> 
#> $model
#> $model$raw
#> Linear mixed model fit by REML ['lmerMod']
#> Formula: stemdw ~ 0 + (1 | bloque) + treat * geno
#>    Data: rawdt
#> REML criterion at convergence: 822.7055
#> Random effects:
#>  Groups   Name        Std.Dev.
#>  bloque   (Intercept) 0.8331  
#>  Residual             6.0516  
#> Number of obs: 150, groups:  bloque, 5
#> Fixed Effects:
#>       treatirrigado          treatsequia              genoG02  
#>              17.682               13.782               -5.804  
#>             genoG03              genoG04              genoG05  
#>              -6.616                0.146               -3.400  
#>             genoG06              genoG07              genoG08  
#>               9.836              -14.734               -5.268  
#>             genoG09              genoG10              genoG11  
#>              -0.762               -7.496              -10.080  
#>             genoG12              genoG13              genoG14  
#>               1.252                0.436                1.160  
#>             genoG15  treatsequia:genoG02  treatsequia:genoG03  
#>              -5.886                0.386                1.158  
#> treatsequia:genoG04  treatsequia:genoG05  treatsequia:genoG06  
#>              -1.402               14.614               -6.532  
#> treatsequia:genoG07  treatsequia:genoG08  treatsequia:genoG09  
#>               3.640               -0.082                1.396  
#> treatsequia:genoG10  treatsequia:genoG11  treatsequia:genoG12  
#>               2.012                1.546               -1.746  
#> treatsequia:genoG13  treatsequia:genoG14  treatsequia:genoG15  
#>              -3.078               -1.190                3.246  
#> 
#> $model$clean
#> Linear mixed model fit by REML ['lmerMod']
#> Formula: stemdw ~ 0 + (1 | bloque) + treat * geno
#>    Data: cleandt
#> REML criterion at convergence: 537.8671
#> Random effects:
#>  Groups   Name        Std.Dev.
#>  bloque   (Intercept) 0.6007  
#>  Residual             2.0454  
#> Number of obs: 143, groups:  bloque, 5
#> Fixed Effects:
#>       treatirrigado          treatsequia              genoG02  
#>            16.09325             13.78200             -4.21525  
#>             genoG03              genoG04              genoG05  
#>            -5.02725              1.73475             -1.81125  
#>             genoG06              genoG07              genoG08  
#>            11.42475            -13.14525             -3.67925  
#>             genoG09              genoG10              genoG11  
#>             0.82675             -4.51258             -8.49125  
#>             genoG12              genoG13              genoG14  
#>             2.84075              2.02475              2.74875  
#>             genoG15  treatsequia:genoG02  treatsequia:genoG03  
#>            -4.29725             -1.20275             -0.43075  
#> treatsequia:genoG04  treatsequia:genoG06  treatsequia:genoG07  
#>            -2.99075             -8.12075              2.05125  
#> treatsequia:genoG08  treatsequia:genoG09  treatsequia:genoG10  
#>            -1.67075             -0.19275             -0.97142  
#> treatsequia:genoG11  treatsequia:genoG12  treatsequia:genoG13  
#>            -0.04275             -3.33475             -4.66675  
#> treatsequia:genoG14  treatsequia:genoG15  
#>            -2.77875              1.65725  
#> fit warnings:
#> fixed-effect model matrix is rank deficient so dropping 1 column / coefficient
#> 
#> 
  
```
