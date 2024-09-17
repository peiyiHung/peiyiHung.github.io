library(tidyverse)

generate_bar_plot <- function(reg, 
                              marked_threshold = 10,
                              mark_shift = 1500,
                              mark_color = "#00B8A9",
                              breaks_x = waiver(),
                              labels_x = waiver(),
                              limits_x = NULL,
                              title = "",
                              subtitle = "",
                              title_x = "") {

    df <- read_csv("data/各類別人數.csv", 
                   show_col_types = FALSE
            ) |> 
          filter(region == reg) |> 
          mutate(
            prop = round(number / sum(number) * 100, 1),
            prop_label = if_else(prop > marked_threshold,  str_c(sprintf("%.1f",prop), "%"), ""),
            color_marked = if_else(prop > marked_threshold,  "1", "0")
          )

    p <- df |> 
        ggplot(
            aes(x = number, y = fct_reorder(class, number), fill = color_marked)
        ) +
        geom_bar(stat = "identity", width = 0.7) +
        scale_x_continuous(
            breaks = breaks_x,
            labels = labels_x,
            limits = limits_x,
            expand = expansion(0),
            position = "top",
        ) +
        scale_y_discrete(expand = expansion(mult = 0.1) ) +
        scale_fill_manual(values = c("grey", mark_color)) +
        labs(
            title = title ,
            subtitle = subtitle,
            x = title_x,
            caption = "資料時間：2023年底・資料來源：衛生福利部統計處"
        ) +
        theme(
            text = element_text(family = "Noto Sans TC"),
            plot.title = element_text(size = 27, face = "bold"),
            plot.subtitle = element_text(size = 18, margin = margin(b = 10)),
            plot.caption = element_text(size = 12, 
                                        margin = margin(t = 15),
                                        hjust = 0),
            plot.caption.position = "plot",
            plot.title.position = "plot",
            plot.margin = unit(c(1, 2, 1, 1), "cm"),
            axis.text = element_text(size = 14),
            axis.line.x = element_line(color = "grey50"),
            axis.ticks.length.y = unit(0, "mm"),
            axis.ticks.length.x = unit(-2, "mm"),
            axis.title.y = element_blank(),
            axis.title.x = element_text(size = 14),
            axis.text.x = element_text(size = 14),  
            panel.background = element_blank(),
            legend.position = "None"
        )
    
    p <- p + geom_text(
        aes(y = class, 
            x = number + mark_shift, 
            label = prop_label),
        size = 4
        )
    return(p)
}

reg <- "高雄市"
title = "高雄市各類別身心障礙者人數"
subtitle = str_c(
    "前三種類別之身心障礙者人數，佔比超過總人數之70%"
)
title_x = "人數 （萬人）"

p <- generate_bar_plot(reg = reg, 
                       marked_threshold = 10,
                       mark_shift = 2400,
                       mark_color = "#7695FF",
                       limits_x = c(0, 50000),
                       labels_x = as.character(seq(0, 5, by = 1)),
                       title = title,
                       subtitle = subtitle,
                       title_x = title_x)
ggsave("./img/高雄_各類別.png", p, 
       width = 12, height = 8)
p
