library(tidyverse)
library(readxl)
library(writexl)

# Helper functions

## tidying the data
get_gender_longer <- function(df){
    df <- df |> pivot_longer(
        cols = c("total", "male", "female"),
        names_to = "gender",
        values_to = "ratio")
    return(df)
}

## adjusting the format of the region data
adjusting_format_after2021 <- function(df, y){
    df <- df |> separate_wider_delim(
        cols = region, 
        delim = " ",
        names = c("region", "2"),
        too_many = "drop"
    ) |> 
        select(c(-2))|> 
        mutate(
            region = if_else(region == "總計", "全國", region)
        ) |> 
        mutate(
            year = y
        ) |> 
        select(
            year, region, gender, ratio
        )
    return(df)
}

adjusting_format_before2020 <- function(df, y){
    df <- df |> separate_wider_position(
        cols = region, 
        widths = c(region = 3, 1),
        too_many = "drop"
    ) |> 
        mutate(
            region = if_else(region == "總計T", "全國", region)
        ) |> 
        mutate(
            year = y
        ) |> 
        select(
            year, region, gender, ratio
        )
    return(df)
}


## get the ratio data from 2021 to 2023
get_ratio_after2021 <- function(y){
    # read the data from an excel file
    df <- read_xlsx("data/2.3.1身心障礙者人數按類別及縣市別分.xlsx", 
                    sheet = y,
                    range = anchored("A8", dim = c(23, 17)),
                    col_names = as.character(1:17)) |>
        select(c(c("1", "15","16", "17")))  |> # selecting columns we want
        rename("region" = "1",
               "total" = "15",
               "male" = "16",
               "female" = "17") # rename the columns
    
    # tidying the data
    df <- get_gender_longer(df)
    
    # adjusting the format of the region data
    df <- adjusting_format_after2021(df, y)
    return(df)
}

## get the ratio data from 2012 to 2021
get_ratio_before2020 <- function(y){
    # read the data from an excel file
    df <- read_xlsx("data/2.3.1身心障礙者人數按類別及縣市別分.xlsx", 
                    sheet = y,
                    range = anchored("A8", dim = c(23, 24)),
                    col_names = as.character(1:24)) |>
        select(c(c("1", "22","23", "24")))  |> # selecting columns we want
        rename("region" = "1",
               "total" = "22",
               "male" = "23",
               "female" = "24") # rename the columns
    
    # tidying the data
    df <- get_gender_longer(df)
    
    # adjusting the format of the region data
    df <- adjusting_format_before2020(df, y)
    return(df)
}

# main section
df_all <- NA

for(y in 2012:2023){
    if (y <= 2020) {
        df <- get_ratio_before2020(as.character(y))
    } else {
        df <- get_ratio_after2021(as.character(y))
    }
    
    if (is_tibble(df_all)) {
        df_all <- rbind(df_all, df)
    } else {
        df_all <- df
    }
}

#write_csv(df_all, "data/佔總人數比率.csv")
