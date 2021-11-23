data\_unemployment\_covid
================
Xinyuan Liu
11/19/2021

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.4     v dplyr   1.0.7
    ## v tidyr   1.1.3     v stringr 1.4.0
    ## v readr   2.0.1     v forcats 0.5.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'

    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
library(httr)
library(readxl)
library(plyr)
```

    ## ------------------------------------------------------------------------------

    ## You have loaded plyr after dplyr - this is likely to cause problems.
    ## If you need functions from both plyr and dplyr, please load plyr first, then dplyr:
    ## library(plyr); library(dplyr)

    ## ------------------------------------------------------------------------------

    ## 
    ## Attaching package: 'plyr'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     arrange, count, desc, failwith, id, mutate, rename, summarise,
    ##     summarize

    ## The following object is masked from 'package:purrr':
    ## 
    ##     compact

``` r
unemploy = read_csv("data_unemployment.csv")
```

    ## Rows: 2900 Columns: 5

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (3): Series ID, Period, Label
    ## dbl (2): Year, Value

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
clean_unemploy = 
  unemploy %>% 
  janitor::clean_names() %>% 
  mutate(series_id = replace(series_id, series_id == "LASST010000000000003", "Alabama")) %>% 
  mutate(series_id = replace(series_id, series_id == "LASST020000000000003", "Alaska")) %>% 
  mutate(series_id = replace(series_id, series_id == "LASST040000000000003", "Arizona")) %>% 
  mutate(series_id = replace(series_id, series_id == "LASST050000000000003", "Arkansas")) %>% 
  mutate(series_id = replace(series_id, series_id == "LASST060000000000003", "California")) %>% 
  mutate(series_id = replace(series_id, series_id == "LASST080000000000003", "Colorado")) %>% 
  mutate(series_id = replace(series_id, series_id == "LASST090000000000003", "Connecticut")) %>% 
  mutate(series_id = replace(series_id, series_id == "LASST100000000000003", "Delaware")) %>% 
  mutate(series_id = replace(series_id, series_id == "LASST120000000000003", "Florida")) %>% 
  mutate(series_id = replace(series_id, series_id == "LASST130000000000003", "Georgia")) %>% 
  mutate(series_id = replace(series_id, series_id == "LASST150000000000003", "Hawaii")) %>% 
  mutate(series_id = replace(series_id, series_id == "LASST160000000000003", "Idaho")) %>% 
  mutate(series_id = replace(series_id, series_id == "LASST170000000000003", "Illinois")) %>% 
  mutate(series_id = replace(series_id, series_id == "LASST180000000000003", "Indiana")) %>% 
  mutate(series_id = replace(series_id, series_id == "LASST190000000000003", "Iowa")) %>% 
  mutate(series_id = replace(series_id, series_id == "LASST200000000000003", "Kansas")) %>% 
  mutate(series_id = replace(series_id, series_id == "LASST210000000000003", "Kentucky")) %>% 
  mutate(series_id = replace(series_id, series_id == "LASST220000000000003", "Louisiana")) %>% 
  mutate(series_id = replace(series_id, series_id == "LASST230000000000003", "Maine")) %>% 
  mutate(series_id = replace(series_id, series_id == "LASST240000000000003", "Maryland")) %>% 
  mutate(series_id = replace(series_id, series_id == "LASST250000000000003", "Massachusetts")) %>% 
  mutate(series_id = replace(series_id, series_id == "LASST260000000000003", "Michigan")) %>% 
  mutate(series_id = replace(series_id, series_id == "LASST270000000000003", "Minnesota")) %>% 
  mutate(series_id = replace(series_id, series_id == "LASST280000000000003", "Mississippi")) %>% 
  mutate(series_id = replace(series_id, series_id == "LASST290000000000003", "Missouri")) %>% 
  mutate(series_id = replace(series_id, series_id == "LASST300000000000003", "Montana")) %>% 
  mutate(series_id = replace(series_id, series_id == "LASST310000000000003", "Nebraska")) %>% 
  mutate(series_id = replace(series_id, series_id == "LASST320000000000003", "Nevada")) %>% 
  mutate(series_id = replace(series_id, series_id == "LASST330000000000003", "New Hampshire")) %>% 
  mutate(series_id = replace(series_id, series_id == "LASST340000000000003", "New Jersey")) %>% 
  mutate(series_id = replace(series_id, series_id == "LASST350000000000003", "New Mexico")) %>% 
  mutate(series_id = replace(series_id, series_id == "LASST360000000000003", "New York")) %>% 
  mutate(series_id = replace(series_id, series_id == "LASST370000000000003", "North Carolina")) %>% 
  mutate(series_id = replace(series_id, series_id == "LASST380000000000003", "North Dakota")) %>% 
  mutate(series_id = replace(series_id, series_id == "LASST390000000000003", "Ohio")) %>% 
  mutate(series_id = replace(series_id, series_id == "LASST400000000000003", "Oklahoma")) %>% 
  mutate(series_id = replace(series_id, series_id == "LASST410000000000003", "Oregon")) %>% 
  mutate(series_id = replace(series_id, series_id == "LASST420000000000003", "Pennsylvania")) %>% 
  mutate(series_id = replace(series_id, series_id == "LASST440000000000003", "Rhode Island")) %>% 
  mutate(series_id = replace(series_id, series_id == "LASST450000000000003", "South Carolina")) %>% 
  mutate(series_id = replace(series_id, series_id == "LASST460000000000003", "South Dakota")) %>% 
  mutate(series_id = replace(series_id, series_id == "LASST470000000000003", "Tennessee")) %>% 
  mutate(series_id = replace(series_id, series_id == "LASST480000000000003", "Texas")) %>% 
  mutate(series_id = replace(series_id, series_id == "LASST490000000000003", "Utah")) %>% 
  mutate(series_id = replace(series_id, series_id == "LASST500000000000003", "Vermont")) %>% 
  mutate(series_id = replace(series_id, series_id == "LASST510000000000003", "Virginia")) %>% 
  mutate(series_id = replace(series_id, series_id == "LASST530000000000003", "Washington")) %>% 
  mutate(series_id = replace(series_id, series_id == "LASST540000000000003", "West Virginia")) %>% 
  mutate(series_id = replace(series_id, series_id == "LASST550000000000003", "Wisconsin")) %>% 
  mutate(series_id = replace(series_id, series_id == "LASST560000000000003", "Wyoming"))
```

``` r
covid = GET("https://data.cdc.gov/resource/9mfq-cb36.csv", query = list("$limit" = 50000)) %>% content("parsed")
new = covid %>% 
  filter(state == "CA") %>% 
  separate(submission_date, into = c("year", "month", "day"), "-") %>% 
  arrange(desc(year), desc(month), desc(day))
```
