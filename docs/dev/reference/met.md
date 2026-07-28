# Swedish cultivar trial data

The datasets were obtained from official Swedish cultivar tests. Dry
matter yield was analyzed. All trials were laid out as alpha-designs
with two replicates. Within each replicate, there were five to seven
incomplete blocks.

## Usage

``` r
met
```

## Format

A data frame with 1069 rows and 8 variables:

- zone:

  Sweden is divided into three different agricultural zones: South,
  Middle, and North

- location:

  Locations: 18 location in the Zones

- rep:

  Replications (4): number of replication in the experiment

- alpha:

  Incomplete blocks (8) in the alpha-designs

- cultivar:

  Cultivars (30): genotypes evaluated

- yield:

  Yield in kg/ha

- year:

  Year (1): 2016

- env:

  enviroment (18): combination zone + location + year

## Source

[doi:10.1002/csc2.20177](https://doi.org/10.1002/csc2.20177)
