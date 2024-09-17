library(tidyverse)
library(readxl)
library(writexl)


# read the names of each class from the file
class_name <- read_xlsx("data/2.3.1身心障礙者人數按類別及縣市別分.xlsx",
                        sheet = "2023",
                        range = anchored("E4", dim = c(2, 10))) |> 
              colnames()

# construct the column names of the dataset
column_names <- c("region", class_name)

# selecting columns we want
col_selected <- as.character(c(1, 5:14))

# read data from the excel file
df <- read_xlsx("data/2.3.1身心障礙者人數按類別及縣市別分.xlsx",
                sheet = "2023",
                range = anchored("A8", dim = c(23, 14)),
                col_names = as.character(1:14)) |> 
      select(all_of(col_selected))
colnames(df) <- column_names

# tidying the data
df <- df |> pivot_longer(
    cols = all_of(class_name),
    names_to = "class",
    values_to = "number") |> 
    mutate(number = as.integer(number))|> 
    separate_wider_delim(
        cols = region, 
        delim = " ",
        names = c("region", "2"),
        too_many = "drop"
    ) |> 
    select(c(-2)) |> 
    mutate(
        region = if_else(region == "總計", "全國", region)
        #adjusting string in the region column
    )


#write_csv(df, "data/各類別人數.csv")
