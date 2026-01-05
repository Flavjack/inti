# Journal Club Tombola

Function for arrange journal club schedule

## Usage

``` r
jc_tombola(
  data,
  members,
  papers = 1,
  group = NA,
  gr_lvl = NA,
  status = NA,
  st_lvl = "active",
  frq = 7,
  date = NA,
  seed = NA
)
```

## Arguments

- data:

  Data frame withe members and their information.

- members:

  Columns with the members names.

- papers:

  Number of paper by meeting

- group:

  Column for arrange the group.

- gr_lvl:

  Levels in the groups for the arrange. See details.

- status:

  Column with the status of the members.

- st_lvl:

  Level to confirm the assistance in the JC. See details.

- frq:

  Number of the day for each session.

- date:

  Date when start the first session of JC.

- seed:

  Number for replicate the results (default = date).

## Value

data frame with the schedule for the JC

## Details

The function could consider n levels for `gr_lvl`. In the case of more
levels using "both" or "all" will be the combination. The suggested
levels for `st_lvl` are: active or spectator. Only the "active" members
will enter in the schedule.
