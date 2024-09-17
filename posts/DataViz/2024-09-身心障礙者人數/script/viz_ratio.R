library(tidyverse)

#read data from a csv file and rename the columns
df <- read_csv("data/佔總人數比率.csv",
               show_col_types = FALSE) |> 
        filter(year == "2023",
               gender %in% c("male", "female"))
   
region_level <- df |> 
    select(region) |> 
    distinct(region) |> 
    pull() |> 
    rev()


p <- ggplot(df, aes(x = ratio, y = factor(region, region_level))) +
    geom_line(aes(group = region), size = 1.5, color = "#758694") +
    geom_point(aes(color = gender), size = 5) +
    labs(
        title = "各縣市身心障礙者佔人口數比率",
        subtitle = "各縣市男性比率皆大於女性，其中台東縣男女間差距最大，澎湖縣則最小",
        x = "身心障礙者佔人口數比率（％）",
        caption = c("註：男性比率為男性身心障礙人口數除以男性總人口數，女性比率算法以此類推",
                    "資料時間：2023年底．資料來源：衛生福利部統計處"),
    ) +
    scale_x_continuous(labels = scales::label_number(accuracy = 0.1),
                       breaks = seq(0, 10, 1)) +
    scale_color_discrete(labels = c("女性", "男性"), 
                         type = c("#E76F51", "#00B8A9")) +
    theme(
        text = element_text(family = "Noto Sans TC", color = "#364F6B"),
        # axis
        axis.title.y = element_blank(),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        axis.text.y = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        #plot
        plot.title = element_text(size = 27, 
                                  color = "#071952", 
                                  face = "bold",
                                  margin = margin(t = 5, b = 5)),
        plot.subtitle = element_text(size = 22, margin = margin(b = 15)),
        plot.caption = element_text(size = 12, hjust = c(0,1)),
        plot.margin = unit(c(1.2,1.2,1.2,1.2), "cm"),
        #legend
        legend.text = element_text(size = 14),
        legend.position = c(.9, .92),
        legend.background = element_rect(fill = "transparent", 
                                         linewidth = .4,
                                         color = "#071952"),
        legend.title = element_blank(),
        #panel
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(linetype = 2)
    )

loca <- df |> 
    filter(
        gender == "female"
    ) |> 
    mutate(
        ratio = ratio - .2
    )

p <- p + geom_text(data = loca, 
              aes(y = region, x = ratio, label = region, family = "Noto Sans TC"), 
              size=5)
p
# ggsave("身心障礙者佔總人口數比率.png",
#        path = "img/",
#        width = 16,
#        height = 10,
#        dpi = 400)

