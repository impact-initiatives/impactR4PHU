# run_fsl_plaus_html_report

run_fsl_plaus_html_report

## Usage

``` r
run_fsl_plaus_html_report(
  .dataset = NULL,
  uuid_var = "_uuid",
  yes_no_team = "no",
  team_var = "team",
  group_var = "enum_id",
  output_file = "fsl_quality_report.pdf",
  output_dir = "reports"
)
```

## Arguments

- .dataset:

  Main Dataset

- uuid_var:

  the name of the variable that indicates the uuid column

- team_var:

  the name of the variable that indicates the team column, if it exists

- group_var:

  the name of the variable that indicates the grouping column, usually
  enumerator ID column

- output_file:

  the name of the output file

- output_dir:

  the directory to export the output file

## Value

an HTML rmarkdown file with FSL quality summaries
