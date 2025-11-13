library(tidyverse)
library(readxl)
library(ggdist)

# Datensatz laden
coffee <- read_csv("./Daten/simplified_coffee.csv")

# Zeilen mit NA-Werten bei "roast" löschen
coffee <- na.omit(coffee)

# Name von price ändern
names(coffee)[6] <- "price"

# Werte in der Spalte "loc_country" ändern
coffee <- coffee |> 
  mutate(loc_country = ifelse(loc_country == "New Taiwan", "Taiwan", loc_country)) |>
  mutate(loc_country = ifelse(loc_country == "Hawai'I", "Hawai'i", loc_country)) |>
  mutate(origin = ifelse(origin == "Hawai'I", "Hawai'i", origin))

# LC mit den meisten Kaffees
Top_5_L = coffee |>
  group_by(loc_country) |>
  summarise(n = n()) |>
  arrange(desc(n)) |>
  select(loc_country) |>
  head(5) |>
  pull(loc_country)

# Top whisker bei den Top 5 LC
top_whisker = coffee |>
  filter(loc_country %in% Top_5_L) |>
  group_by(loc_country) |>
  summarise(top_whisker = quantile(price, 0.75) + 1.5 * IQR(price))

# Outlier für jedes land entfernen
Guatemala_outlier_rm = coffee |>
  filter(loc_country %in% top_whisker$loc_country[1]) |>
  filter(price <= top_whisker$top_whisker[1])

Hawaii_outlier_rm = coffee |>
  filter(loc_country %in% top_whisker$loc_country[2]) |>
  filter(price <= top_whisker$top_whisker[2])

HK_outlier_rm = coffee |>
  filter(loc_country %in% top_whisker$loc_country[3]) |>
  filter(price <= top_whisker$top_whisker[3])

Taiwan_outlier_rm = coffee |>
  filter(loc_country %in% top_whisker$loc_country[4]) |>
  filter(price <= top_whisker$top_whisker[4])

US_outlier_rm = coffee |>
  filter(loc_country %in% top_whisker$loc_country[5]) |>
  filter(price <= top_whisker$top_whisker[5])

# Wieder zu einem dataframe zusammen fügen
coffee_outlier_rm = rbind(Guatemala_outlier_rm, Hawaii_outlier_rm, HK_outlier_rm,
                          Taiwan_outlier_rm, US_outlier_rm)

c_scale <- c("United States" ="#C1B095", "Taiwan" = "#978772", "Hawai'i" = "#5F5043" ,
             "Hong Kong" = "#7B6C5B", "Guatemala" = "#352720")

# Plot
coffee_outlier_rm |>
  ggplot(aes(x = loc_country , y = price, color = loc_country)) +
  ggdist::stat_halfeye(
    aes(fill = loc_country),
    adjust = 0.6,
    justification = -0.15,
    scale = 0.7,
    .width = 0,
    point_colour = NA) +
  geom_boxplot(
    width = 0.1,
    outlier.color = NA) +
  ggdist::stat_dots(
    aes(fill = loc_country),
    alpha = 0.3,
    binwidth = 0.3,
    scale = 0.5,
    side = "left",
    justification = 1.15,
    position = position_dodge(),
    overflow = "compress") +
  labs(
    title = "Vergleich der Kaffeepreise der Top 5 Röststandorte",
    x = "Standort der Rösterei",
    y = "Preis \n($/100g)",
    caption = "Daten von coffeereview.com [11.2017-11.2022]") +
  coord_flip() +
  scale_fill_manual(values = c_scale) +
  scale_colour_manual(values = c_scale) +
  scale_y_continuous(breaks = seq(0, 40, 2)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    aspect.ratio = 1,
    plot.background = element_rect(colour = "black", linewidth = 1, fill = "grey90"),
    plot.title = element_text(face = "bold", size = 15, hjust = 0.34),
    axis.title = element_text(face = "bold", size =10),
    axis.title.x = element_text(hjust = 0.43),
    plot.caption = element_text(face = "italic", size = 8))
