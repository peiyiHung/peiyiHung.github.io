library(tidyverse)

df <- read_csv("data/佔總人數比率2.csv", show_col_types = FALSE) |> 
    filter(region %in% c("全國", "總計"))

year_labels <- str_c(seq(13, 22, by = 1), sep = "'")
year_labels <- sapply(year_labels, function(x){paste("'", x, sep = "")})


p <- df |> ggplot(aes(x = year, y = ratio)) +
    geom_line(aes(color = gender), 
              linewidth = 1.5, 
              show.legend = FALSE) +
    geom_point(aes(fill = gender), 
               color = "white", 
               size = 4,
               stroke = 1.2,
               pch = 21) +
    scale_x_continuous(
        limits = c(2011, 2023.5),
        breaks = c(2012:2023),
        labels = c("2012", year_labels, "2023"),
        expand = expansion(0)
    ) +
    scale_y_continuous(
        limits = c(4, 6.2),
        expand = expansion(0)
    ) +
    scale_fill_discrete(labels = c("女性", "男性", "整體"),
                        type = c("#E76F51", "#00B8A9", "#364F6B")) +
    scale_color_discrete(type = c("#E76F51", "#00B8A9", "#364F6B")) +
    labs(
        fill = "性別",
        x = "年度",
        y = "比\n率\n(%)",
        title = "近10年全台身心障礙者人數佔總人口比率",
        subtitle = "整體身心障礙者人數佔總人口比率逐漸上升，男性與女性呈現相似的趨勢",
        caption = "資料來源：衛生福利部統計處"
    ) +
    theme(
        text = element_text(family = "Noto Sans TC"),
        plot.margin = unit(c(1, 2, 1, 1), "cm"),
        plot.title = element_text(size = 27, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.caption = element_text(size = 12, 
                                    margin = margin(t = 15),
                                    hjust = 0),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "#A8BAC4", linetype = "dotted"),
        panel.background = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks.length = unit(0, "mm"),
        axis.line.y = element_blank(),
        axis.text = element_text(size = 14),
        axis.text.y = element_blank(),
        axis.title.y = element_text(angle = 0, vjust = 0.45),
        legend.text = element_text(size = 12)
    )

mark_df <- df |> 
    filter(year %in% c(2012, 2023)) |> 
    select(year, ratio) |> 
    mutate(label = sprintf("%.2f", ratio))

p <- p + geom_text(data = mark_df, 
                   aes(x = year, y = ratio + 0.08, label = label),
                   size = 12, size.unit = "pt")

d <- tibble(x = 2012-0.81, y = c(4, 4.5, 5, 5.5, 6)) |> 
    mutate(label = sprintf("%.1f", y),
           y = y + 0.04)

p <- p + geom_text(data = d, 
                   aes(x = x, y = y, label = label),
                   size = 12, size.unit = "pt")

ggsave("img/近10年全台身心障礙者人數佔總人口比率.png", p,
       width = 12, height = 7.5)
