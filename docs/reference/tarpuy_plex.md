# Fieldbook plan information

Information for build a plan for an experiment (PLEX)

## Usage

``` r
tarpuy_plex(
  data = NULL,
  title = NULL,
  short_title = NULL,
  objective = NULL,
  references = NULL,
  plan = NULL,
  institutions = NULL,
  researchers = NULL,
  manager = NULL,
  location = NULL,
  altitude = NULL,
  georeferencing = NULL,
  environment = NULL,
  start = NA,
  end = NA,
  project = NULL,
  repository = NULL,
  manuscript = NULL,
  album = NULL,
  nfactor = 2,
  design = "rcbd",
  rep = 4,
  zigzag = FALSE,
  nrows = NA,
  serie = 1000,
  seed = 0,
  qrcode = "{project}{plots}",
  aug_blocks = NA,
  aug_block_size = NA,
  aug_random = TRUE
)
```

## Arguments

- data:

  Data with the fieldbook information.

- title:

  Project title.

- short_title:

  Short description of the project.

- objective:

  The objectives of the project.

- references:

  References.

- plan:

  General description of the project (M & M).

- institutions:

  Institutions involved in the project.

- researchers:

  Persons involved in the project.

- manager:

  Persons responsible of the collection of the data.

- location:

  Location of the project.

- altitude:

  Altitude of the experiment (m.a.s.l).

- georeferencing:

  Georeferencing information.

- environment:

  Environment of the experiment (greenhouse, lab, etc).

- start:

  The date of the start of the experiments.

- end:

  The date of the end of the experiments.

- project:

  Name or ID for the fieldbook/project.

- repository:

  link to the repository.

- manuscript:

  link for manuscript.

- album:

  link with the photos of the project.

- nfactor:

  Number of factors for the design.

- design:

  Type of design.

- rep:

  Number of replication.

- zigzag:

  Experiment layout in zigzag `[logic: FALSE]`

- nrows:

  Experimental design dimension by rows `[numeric: value]`

- serie:

  Number of digits in the plots.

- seed:

  Seed for the randomization.

- qrcode:

  QR code template used to concatenate fieldbook identifiers.

- aug_blocks:

  Number of blocks for augmented design.

- aug_block_size:

  Number of plots per block for augmented design.

- aug_random:

  Logical. Randomize entries allocation in augmented design.

## Value

data frame or list of arguments:

1.  info

2.  variables

3.  design

4.  logbook

5.  timetable

6.  budget

## Details

Provide the information available.
