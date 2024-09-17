library(tidyverse)

df <- read_csv("data/各類別人數.csv", 
               show_col_types = FALSE
               ) |> 
      filter(region == "全國") |> 
      mutate(
          prop = round(number / sum(number) * 100, 1),
          prop_label = if_else(prop > 20,  str_c(prop, "%"), "")
      )
df
p <- df |> 
    mutate(
        mark = if_else(number > 200000, "1", "0")
    ) |> 
    ggplot(
    aes(x = number, y = fct_reorder(class, number), fill = mark)
    ) +
    geom_bar(stat = "identity", width = 0.7) +
    scale_x_continuous(
        breaks = c(0, 100000, 200000, 300000, 400000),
        limits = c(0, 400000),
        labels = c("","10","20", "30", "40"),
        expand = expansion(0),
        position = "top",
    ) +
    scale_y_discrete(
       expand = expansion(mult = 0.1) 
    ) +
    scale_fill_manual(values = c("grey", "#00B8A9")) +
    labs(
       title = "各類別身心障礙者人數" ,
       subtitle = "以「神經系統構造及精神、心智功能」與「神經、肌肉、骨骼之移動相關構造及其功能」\n這兩種類別之身心障礙者人數最多，佔比超過總人數之一半",
       x = "人數(萬人）",
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
        #panel.grid.major.x = element_line(color = "grey50",linetype = "dashed"),
        legend.position = "None"
    )
p <- p + geom_text(aes(y = class, x = number + 16500, label = prop_label))

ggsave("img/各類別身心障礙者人數.png", p, 
       width = 12, height = 8)
p
