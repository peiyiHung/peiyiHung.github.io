library(tidyverse)
library(scales)

income_data <- c(1059731,
                 1079648,
                 1090554,
                 1108569,
                 1136708)

income <- tibble(
    year = seq(2019, 2023, by = 1),
    income = income_data
)

income |> 
    ggplot(aes(x = year, y = income)) +
    geom_bar(stat = "identity", 
             width = 0.6,
             fill = "#219B9D") +
    scale_x_continuous(
        expand = c(0.05, 0)
    ) +
    scale_y_continuous(
        expand = c(0, 0),
        limits = c(0, 1200000)
    ) +
    labs(
        x = "",
        y = "",
    ) +
    theme(
        axis.line.x = element_line(),
        axis.ticks.length.y = unit(0, "mm"),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 14),
        panel.background = element_blank()
    ) +
    geom_text(aes(y = income + 30000,
                  label = dollar(income)))
ggsave("./img/income_draft.png", 
       width = 8, height = 6,
       dpi = 300)
