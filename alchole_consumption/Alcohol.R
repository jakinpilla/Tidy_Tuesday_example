#'---
#'title: "Alcohol Consumption"
#'author: "jakinpilla"
#'output: rmarkdown::github_document
#'---

#'Alcohol consumption around the world
library("tidyverse")
library("ggplot2")
library("reshape2")
library("dplyr")
library("extrafont")
library("maps")

#'Read data
alcohol = read_csv("data/drinks.csv")
alcohol$total_servings = alcohol$beer_servings + alcohol$wine_servings + alcohol$spirit_servings
alcohol %>%
  tbl_df() %>%
  mutate(total_servings = beer_servings + wine_servings + spirit_servings)

#'Calculate servings of each type relative to other types of alcohol
alcohol %>%
  mutate_at(vars(-country), funs(round(./total_servings, 2))) %>%
  mutate_all(funs(ifelse(is.nan(.), 0, .))) %>%
  select(-total_servings) %>%
  rename_at(vars(-country), ~ paste0("ratio_", .)) -> alcohol_ratio

alcohol %>%
  left_join(alcohol_ratio, by = "country") -> alcohol_combined

alcohol_combined  %>%
  select(country, contains("ratio_")) %>%
  gather(main_type, max, -country) %>%
  group_by(country) %>%
  slice(which.max(max)) %>%
  mutate(main_type = gsub("ratio_|_servings", "", main_type)) -> alcohol_main

alcohol_combined %>%
  left_join(alcohol_main, by = "country") -> alcohol_total

#'Sums over types
alcohol_total[, 2:4] %>% colSums()

#'Get world map
map.world <- map_data(map="world")

#' Which countries in data cannot be found in the list of map.world countries?
countries = as.character(alcohol_total$country)[!is.na(alcohol_total$main_type)]
countries[!countries %in% map.world$region]

#' Alter names in data to match those in map.world 
m = as.character(alcohol_total$country)
names(m) = m
m[c("Antigua & Barbuda", "Bosnia-Herzegovina", "Cote d'Ivoire", 
    "Cabo Verde", "Congo", "DR Congo", "Russian Federation", 
    "United Kingdom")] = c("Antigua", "Bosnia and Herzegovina", "Ivory Coast", 
                           "Cape Verde", "Republic of Congo", 
                           "Democratic Republic of the Congo", "Russia", "UK")

alcohol_total$country = unname(m)

#' Join alcohol data with map data
map.world = left_join(map.world, alcohol_total, by = c('region' = 'country'))
map.world$main_type = factor(map.world$main_type)

#' Plot - World map with preferred alcoholic beverage per country
windowsFonts(arial=windowsFont("TT Arial"))
preferred_plot <- map.world %>% ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = main_type), 
               color="white", size=1) +
  scale_fill_manual(values = c("#fecc5c", "#3690c0", "#d7191c"), na.value="#eff3ff") +
  labs(title="\nPreferred alcoholic beverages around the world", 
       subtitle="For countries with at least 10 total servings per capita per year\n") +
  guides(fill = guide_legend(keywidth = 6, keyheight = 1.5, title=NULL, label.position = "top", 
                             label.hjust = 0.5)) +
  theme(text=element_text(family="arial"),
        plot.title = element_text(hjust = 0.5, size=25), 
        plot.subtitle = element_text(hjust = 0.5, size = 16),
        legend.position = "top",
        legend.text=element_text(size=16),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank()
        ) 

print(preferred_plot)  

#' Select only European countries
europe = c("Albania", "Finland", "Andorra", "Austria", "Belgium",  "Bulgaria", "Bosnia and Herzegovina", "Belarus", "Switzerland", "Czech Republic", "Cyprus", "Germany", "Denmark", "Canary Islands", "Spain", "Estonia", "France", "UK", "Greece", "Croatia", "Hungary", "Ireland", "Iceland", "Italy", "San Marino", "Kosovo", "Liechtenstein", "Lithuania", "Luxembourg", "Latvia", "Monaco", "Moldova", "Macedonia", "Malta", "Montenegro", "Netherlands", "Norway", "Poland", "Portugal", "Romania", "Russia", "Serbia", "Slovakia", "Slovenia", "Sweden",   "Turkey", "Ukraine", "Vatican")
map.europe = map.world[map.world$region %in% europe,]

#' Plot - Map of Europe with preferred alcoholic beverage per country
preferred_europe_plot <- map.europe %>%
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = main_type), color="white", size=1) +
  xlim(c(-26,42)) +
  ylim(c(34,75)) +
  scale_fill_manual(values = c("#fecc5c", "#3690c0", "#d7191c"), na.value="#eff3ff") +
  labs(title="\nPreferred alcoholic beverages - Europe", 
       subtitle="For countries with at least 10 total servings of alcohol per capita per year\n") +
  guides(fill = guide_legend(keywidth = 6, keyheight = 1.5, title=NULL, 
                             label.position = "top", label.hjust = 0.5)) +
  theme(text=element_text(family="arial"),
        plot.title = element_text(hjust = 0.5, size=25), 
        plot.subtitle = element_text(hjust = 0.5, size = 16),
        legend.position = "top",
        legend.text=element_text(size=16),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank()
  ) 


print(preferred_europe_plot)

#'Select top 15 by liters of pure alcohol
alcohol_total %>% colnames()
top30 = alcohol_total[order(alcohol$total_litres_of_pure_alcohol, decreasing = T),][1:30,]

top30 %>%
  filter(country == "South Korea")

top30 %>% View()

top30_plot <- top30 %>%
  ggplot() + 
  geom_bar(aes(x=reorder(country, total_litres_of_pure_alcohol, sum), 
               y=total_litres_of_pure_alcohol, fill=main_type), stat="identity", width=0.8) +
  coord_flip() +
  guides(fill = guide_legend(keywidth = 4, keyheight = 1, title=NULL, label.position = "top", 
                             label.hjust = 0.5)) +
  scale_fill_manual(values = c("#fecc5c", "#3690c0", "#d7191c"), na.value="#eff3ff") +
  labs(title="\nTop 30 alcohol consuming countries", 
       subtitle="Color shows the preferred alcoholic beverage for each country", 
       y="Liters of pure alcohol consumed per capita", x="") +
  theme_minimal(16) +
  theme(legend.position = "top",
        text=element_text(family="arial"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)
  )

#+ Fig1, echo=TRUE, fig.height=20, fig.width=15
print(top30_plot)
