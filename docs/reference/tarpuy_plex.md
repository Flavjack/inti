# Fieldbook plan information

Information for build a plan for an experiment (PLEX)

## Usage

``` r
tarpuy_plex(
  data = NULL,
  title = NULL,
  objectives = NULL,
  hypothesis = NULL,
  rationale = NULL,
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
  about = NULL,
  fieldbook = NULL,
  project = NULL,
  repository = NULL,
  manuscript = NULL,
  album = NULL,
  nfactor = 2,
  design = "rcbd",
  rep = 3,
  zigzag = FALSE,
  nrows = NA,
  serie = 1000,
  seed = 0,
  qrcode = "{fbname}{plots}{factors}"
)
```

## Arguments

- data:

  Data with the fieldbook information.

- title:

  Project title.

- objectives:

  The objectives of the project.

- hypothesis:

  What are the expected results.

- rationale:

  Based in which evidence is planned the experiment.

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

- about:

  Short description of the project.

- fieldbook:

  Name or ID for the fieldbook/project.

- project:

  link for project.

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

  Concatenate the QR code `[character: {fbname}{plots}{factors}]`

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
