library(tidyverse)
library(readxl)
library(ggdist)

#Datensatz laden
coffee_reviews <- read_csv("./Daten/simplified_coffee.csv")

# Zeilen mit NA-Werten bei "roast" löschen 
coffee_reviews <- na.omit(coffee_reviews)

#name von price ändern
names(coffee_reviews)[6] <- "USD_100g"
# Werte in der Spalte "loc_country" ändern
coffee_reviews <- coffee_reviews |> 
  mutate(loc_country = ifelse(loc_country == "New Taiwan", "Taiwan", loc_country)) |> 
  mutate(loc_country = ifelse(loc_country == "Hawai'I", "Hawai'i", loc_country)) |> 
  mutate(origin = ifelse(origin == "Hawai'I", "Hawai'i", origin))

# LC mit den meisten Kaffees
Top_5_L = coffee_reviews |>  
  group_by(loc_country) |> 
  summarise(n = n()) |> 
  arrange(desc(n)) |> 
  select(loc_country) |> 
  head(5) |> 
  pull(loc_country)
# -> "United States" "Taiwan"  "Hawai'i"   "Guatemala"   "Hong Kong"

#Origin mit den meisten Kaffee
Top_5 = coffee_reviews |>  
  group_by(origin) |> 
  summarise(n = n()) |> 
  arrange(desc(n)) |> 
  select(origin) |> 
  head(5) |> 
  pull(origin)
# -> Top 5: "Ethiopia"  "Colombia"  "Kenya"   "Hawai'i"   "Guatemala"

# top whisker bei den Top 5 LC 
top_whisker_LC = coffee_reviews |> 
  filter(origin %in% Top_5) |>
  filter(loc_country %in% Top_5_L) |> 
  group_by(loc_country) |> 
  summarise(top_whisker = quantile(USD_100g, 0.75) + 1.5 * IQR(USD_100g))


#outlier für jedes land entfernen
Guatemala_outlier_rm = coffee_reviews |> 
  filter(loc_country %in% top_whisker_LC$loc_country[1]) |> 
  filter(USD_100g <= top_whisker_LC$top_whisker[1])

Hawaii_outlier_rm = coffee_reviews |> 
  filter(loc_country %in% top_whisker_LC$loc_country[2]) |> 
  filter(USD_100g <= top_whisker_LC$top_whisker[2])

HK_outlier_rm = coffee_reviews |> 
  filter(loc_country %in% top_whisker_LC$loc_country[3]) |> 
  filter(USD_100g <= top_whisker_LC$top_whisker[3])

Taiwan_outlier_rm = coffee_reviews |> 
  filter(loc_country %in% top_whisker_LC$loc_country[4]) |> 
  filter(USD_100g <= top_whisker_LC$top_whisker[4])

US_outlier_rm = coffee_reviews |> 
  filter(loc_country %in% top_whisker_LC$loc_country[5]) |> 
  filter(USD_100g <= top_whisker_LC$top_whisker[5])

#wieder zu einem dataframe zusammen fügen
coffee_outlier_rm_LC = rbind(Guatemala_outlier_rm, Hawaii_outlier_rm, HK_outlier_rm, Taiwan_outlier_rm, US_outlier_rm)

scale_LC <- c("United States" ="#352720", "Taiwan" = "#5F5043", "Hawai'i" = "#7B6C5B", "Hong Kong" = "#978772", "Guatemala" = "#C1B095") 

coffee_outlier_rm_LC |> 
  filter(loc_country %in% Top_5_L) |>
  group_by(loc_country) |> 
  ggplot(aes(x = loc_country , y = USD_100g, color = loc_country)) +
  # add half-violin from {ggdist} package
  ggdist::stat_halfeye(
    aes(fill = loc_country),
    # adjust bandwidth
    adjust = 0.6,
    # move to the right
    justification = -0.2,
    scale = 0.7,
    # remove the slub interval
    .width = 0,
    point_colour = NA
  ) +
  geom_boxplot(
    width = 0.1,
    # removing outliers
    outlier.color = NA
  ) +
  ggdist::stat_dots(
    aes(fill = loc_country),
    # ploting on left side
    side = "left",
    # adjusting position
    justification = 1.15,
    # adjust grouping (binning) of observations
    binwidth = 0.3, 
    position = position_dodge(), 
    overflow = "compress", 
    scale = 0.5, 
    alpha = 0.25
  ) +
  # Themes and Labels
  labs(
    title = "RainCloud Plot of Coffee Prices by Location Country",
    x = "Location Country",
    y = "Price \n($/100g)", 
    fill = "Country"
  ) +
  coord_flip() + 
  theme_minimal() + 
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0.25), 
    legend.position = "none",
    aspect.ratio = 1,
    plot.background = element_rect(colour = "black", linewidth = 1, fill = "grey90"), 
    axis.title = element_text(face = "bold", size = 10)
  ) + 
  scale_fill_manual(values = scale_LC) +
  scale_colour_manual(values = scale_LC)
