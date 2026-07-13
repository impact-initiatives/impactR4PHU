# check_wash_flags

check_wash_flags

## Usage

``` r
check_wash_flags(
  .dataset,
  data_container_loop = NULL,
  container_type = "wash_container_type",
  container_litre_other = "wash_container_litre_other",
  container_journey_collection = "wash_container_journey_collection",
  num_containers = "wash_num_containers",
  wash_water_collect_time = "wash_water_collect_time",
  value_on_premise = "on_premise",
  num_hh = "num_hh",
  uuid = "uuid"
)
```

## Arguments

- .dataset:

  the raw dataset

- data_container_loop:

  loop_dataset for containers. By default NULL

- container_type:

  the name of the variable that indicates the type of the container
  including in the choice the amount per liter (ex: choicex_20l)

- container_litre_other:

  the name of the variable that indicates the amount per liter for other
  containers

- container_journey_collection:

  the name of the variable that indicates the amount of journeys done

- num_containers:

  the name of the variable that indicates the number of containers per
  HH

- wash_water_collect_time:

  the name of the variable that indicates if water on premise

- value_on_premise:

  the value of the choice indicating the water is collected on premise

- num_hh:

  the name of the variable that indicates the number of people per HH

- uuid:

  uuid variable

## Value

a dataframe that includes all the logical flags related to WASH This
includes: If loop containers available: - litre_per_day_per_person -
litre_z_score - flag_sd_litre - flag_low_litre - flag_high_litre -
flag_high_container - flag_no_container

## Examples

``` r
df <- data.frame(
  num_hh = c("3","4"),
  wash_num_containers = c("2","3"),
  enumerator = c("1","2"),
  wash_water_collect_time = c("on_premise", "num_minutes"),
  uuid = c("uuid_1","uuid_2")
)

df_containers <- data.frame(
  uuid = c("uuid_1","uuid_1","uuid_2","uuid_2","uuid_2"),
  wash_container_type = c("bucket_20l", "bucket_14l", "jerry_can_10l",
                          "collapsible_jerry_can_5l" ,"other"),
  wash_container_litre_other = c(NA,NA,NA,NA,"30"),
  wash_container_journey_collection = c("1","2", "1","1","1")
)

check_wash_flags(
  .dataset = df,
  data_container_loop = df_containers
)
#> Joining with `by = join_by(uuid)`
#>   num_hh wash_num_containers enumerator wash_water_collect_time   uuid
#> 1      3                   2          1              on_premise uuid_1
#> 2      4                   3          2             num_minutes uuid_2
#>   litre_per_day_per_hh litre_per_day_per_person litre_z_score flag_sd_litre
#> 1                   48                    16.00     0.7071068             0
#> 2                   45                    11.25    -0.7071068             0
#>   flag_low_litre flag_high_litre flag_high_container flag_no_container
#> 1              0               0                   0                 0
#> 2              0               0                   0                 0
```
