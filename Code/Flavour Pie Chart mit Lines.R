library(tidyverse)
library(readr)
library(ggrepel)

coffee <- read_csv("./Daten/simplified_coffee.csv")


# vektor mit allen flavourn des outer layer -------------------------------



flavours <- c(
  "black tea","chamomile", "rose", "jasmine", "blackberry", "raspberry", "blueberry", "strawberry", "raisin", "prune", "coconut", "cherry", "pomegranate", "pineapple", "grape", "apple", "peach", "pear", "grapefruit", "orange", "lemon", "lime", "sour aromatics", "acetic acid", "butyric acid", "isovaleric acid", "citric acid", "malic acid", "winey", "whiskey", "fermented", "overripe","olive oil", "raw", "underripe", "peapod", "fresh", "dark green", "vegetative", "hay-like", "herb-like","beany", "stale", "cardboard", "papery", "woody", "damp", "dusty", "earthy", "animalic", "meaty brothy", "phenolic", "bitter", "salty", "medicinal", "petroleum", "skunky", "rubber","pipe tobacco", "tobacco", "acrid", "ashy", "smoky", "brown", "grain", "malt", "pungent", "pepper", "anise", "nutmeg", "cinnamon", "clove", "peanuts", "hazelnut", "almond", "chocolate", "dark chocolate", "molasses", "maple syrup", "caramelized", "honey", "vanilla", "vanillin", "overall sweet", "sweet aromatics"
)


# dataframe welcher die gruppierungen enthält -----------------------------


übergruppen <- data.frame(
  flavour =  flavours,
  mid_group = c(
    "black tea", rep("floral", 3), rep("berry", 4), rep("dried_fruit", 2), rep("other_fruit", 8), rep("citrus_fruit", 4), rep("sour", 6), rep("alcohol/fermented", 4), "olive oil", "raw", rep("green", 7), "beany", rep("papery/musty",10), rep("chemical", 6), "pipe tobacco", "tobacco", rep("burnt", 4), rep("cereal", 2), "pungent", "pepper", rep("brown spice",4), rep("nutty", 3), rep("cocoa", 2), rep("brown sugar", 4), "vanilla", "vanillin", "overall sweet", "sweet aromatics" 
  ),
  inner_group = c(
    rep("floral", 4), rep("fruity", 18), rep("sour/fermented", 10), rep("green", 10), rep("other", 16), rep("roasted", 8), rep("spices", 6), rep("nutty/cocoa", 5), rep("sweet", 8)
  )
)



flavours_H = coffee |> 
  select(review) |> 
  mutate(review = tolower(review)) |> 
  mutate(review = str_replace_all(review, "[[:punct:]]", "")) |> 
  summarise(review = paste(review, collapse = " ")) |> 
  mutate(flavours = str_extract_all(review, paste(flavours, collapse = "|"))) |> 
  pull(flavours) |> 
  table() |> 
  as.data.frame() |> 
  left_join(übergruppen, by = join_by(Var1 == flavour))

mid_group_H = flavours_H |> 
  group_by(mid_group) |> 
  summarise(Freq = sum(Freq))

inner_group_H = flavours_H |> 
  group_by(inner_group) |> 
  summarise(Freq = sum(Freq)) 



# aus der spalte mit flavour oder flavourgruppe einen faktor --------
#levels enthält dabei die gewünschte reihenfolge
#wird dieser schritt nicht gemacht werden die flavour sowie übergruppen alphabetisch im kreis geplottet wodurch die untergruppierungen nicht erkennbar sind



flavours_H$Var1 = factor(flavours_H$Var1, levels = flavours)
flavours_H = flavours_H |> 
  arrange(Var1)


mid_group_H$mid_group = factor(mid_group_H$mid_group, levels = c(unique(übergruppen$mid_group)))
mid_group_H = mid_group_H |> 
  arrange(mid_group)

inner_group_H$inner_group = factor(inner_group_H$inner_group, levels = c(unique(übergruppen$inner_group)))
inner_group_H = inner_group_H |> 
  arrange(inner_group)




# label outer -------------------------------------------------------------



label_data_outer = data.frame(
  Var1 = flavours_H$Var1,
  Freq = flavours_H$Freq
)

label_data_outer = label_data_outer |> 
  arrange(desc(Var1)) #da der pie chart clockwise(durch direction -1, normal anticlockwise) geplottet ist und die labels anticlockwise (durch direction -1; normalerweise clockwise) geplottet würden drehen wir die reihenfolge um, um sie clockwise zu plotten




label_data_outer$cum_Freq = cumsum(label_data_outer$Freq)
Freq_half_outer = flavours_H |>
  arrange(desc(Var1)) |> 
  mutate(Freq = Freq/2) |> 
  select(Freq)
label_data_outer$Freq_position = label_data_outer$cum_Freq - Freq_half_outer[,1]


angle_outer <- -(90 - 360 * (label_data_outer$Freq_position/sum(label_data_outer$Freq)))

# Die Label auf der rechten seite werden herumgedreht um sie lesbar zu machen
label_data_outer = label_data_outer |> 
  mutate(angle = ifelse(angle_outer > 90, angle_outer-180, angle_outer))



# label mid ---------------------------------------------------------------


label_data_mid = data.frame(
  mid_group = mid_group_H$mid_group,
  Freq = mid_group_H$Freq
)

label_data_mid = label_data_mid |> 
  arrange(desc(mid_group))

label_data_mid$cum_Freq = cumsum(label_data_mid$Freq)

Freq_half_mid = mid_group_H |> 
  arrange(desc(mid_group)) |> 
  mutate(Freq = Freq/2) |> 
  select(Freq)

label_data_mid$Freq_position = label_data_mid$cum_Freq - Freq_half_mid$Freq # $Freq da Freq_half_mid ein dataframe ist und wir einen vektor benötigen

angle_mid <- -(90 - 360 * (label_data_mid$Freq_position/sum(label_data_mid$Freq)))

label_data_mid = label_data_mid |> 
  mutate(angle = ifelse(angle_mid > 90, angle_mid-180, angle_mid))


# label inner -------------------------------------------------------------

label_data_inner = data.frame(
  inner_group = inner_group_H$inner_group,
  Freq = inner_group_H$Freq
)

label_data_inner = label_data_inner |> 
  arrange(desc(inner_group))

label_data_inner$cum_Freq = cumsum(label_data_inner$Freq)

Freq_half_inner = inner_group_H |> 
  arrange(desc(inner_group)) |> 
  mutate(Freq = Freq/2) |> 
  select(Freq)

label_data_inner$Freq_position = label_data_inner$cum_Freq - Freq_half_inner$Freq # $Freq da Freq_half_inner ein dataframe ist und wir einen vektor benötigen

angle_inner <- -(90 - 360 * (label_data_inner$Freq_position/sum(label_data_inner$Freq)))

label_data_inner = label_data_inner |> 
  mutate(angle = ifelse(angle_inner > 90, angle_inner-180, angle_inner))

# Entfernt wurden im outer layer: 

label_data_removed = label_data_outer |> 
  filter(grepl("^vanillin$|^prune$|^winey$|^fermented$|^tobacco$|^nutmeg$|^whiskey$", Var1))

label_data_outer <- label_data_outer |> 
  anti_join(label_data_removed)

# plot --------------------------------------------------------------------

#Erstellung einer Color Scale 
scale_color_SCA = c(
  "floral" = "#d90e6a", "fruity" = "#da1d23", "sour/fermented" = "#ebb40f", "green" = "#187a2f", "other" = "#0aa3b5", "roasted" = "#c94930", "spices" = "#ad203e","nutty/cocoa" = "#a87b64", "sweet" = "#e65832","chamomile" = "#f89e1c", "rose" = "#ef5a78", "jasmine" = "#f7f1bd", "blackberry" = "#3e0317", "raspberry" = "#e71a60", "blueberry" = "#6569af", "strawberry" = "#f02c38", "raisin" = "#b53b54", "prune" = "#a5446e", "coconut" = "#cf7c36", "cherry" = "#e73451", "pomegranate" = "#e65656", "pineapple" = "#f89a1c", "grape" = "#aeb92c", "apple" = "#4eb947", "peach" = "#f78553", "pear" = "#baa635", "grapefruit" = "#f16356", "orange" = "#e2631e", "lemon" = "#fde404", "lime" = "#7db138", "sour aromatics" = "#9ea718", "acetic acid" = "#94a76f", "butyric acid" = "#cfb24e", "isovaleric acid" = "#8eb646", "citric acid" = "#faef07", "malic acid" = "#c1ba07", "winey" = "#8f1c53", "whiskey" = "#b34039", "fermented" = "#ba9232", "overripe" = "#8b6439","olive oil" = "#a2b029", "raw" = "#718933", "underripe" = "#a2bb2a", "peapod" = "#62aa3c", "fresh" = "#03a653", "dark green" = "#04854a", "vegetative" = "#29b44b", "hay-like" = "#a3a830", "herb-like" = "#7ac141","beany" = "#5d9b80", "stale" = "#8b8c8f", "cardboard" = "#beb275", "papery" = "#fefef4", "woody" = "#744e03", "damp" = "#a3a36f", "dusty" = "#c9b583", "earthy" = "#978847", "animalic" = "#9d977f", "meaty brothy" = "#cd7b6a", "phenolic" = "#db646a", "bitter" = "#80a89d", "salty" = "#def2fd", "medicinal" = "#7b9bae", "petroleum" = "#039fb8", "skunky" = "#5e777b", "rubber" = "#130d0d","pipe tobacco" = "#c8a564", "tobacco" = "#debd7e", "acrid" = "#b9a449", "ashy" = "#899893", "smoky" = "#a1743b", "brown" = "#894711", "grain" = "#b7906f", "malt" = "#eb9d5f", "pungent" = "#794752", "pepper" = "#cc3e42", "anise" = "#c78936", "nutmeg" = "#8c292c", "cinnamon" = "#e6752f", "clove" = "#a16c5a", "peanuts" = "#d4ad12", "hazelnut" = "#9d5433", "almond" = "#c89f83", "chocolate" = "#692a19", "dark chocolate" = "#470604", "molasses"= "#310c0f", "maple syrup" = "#ae341f", "caramelized" = "#d78823", "honey" = "#d95d1e", "vanilla" = "#f99980", "vanillin" = "#f27674", "overall sweet" = "#e75b67", "sweet aromatics" = "#d05460", "black tea" = "#975e6d",            "berry" = "#dd4c51", "dried_fruit" = "#c94a44", "other_fruit" = "#f1684b", "citrus_fruit" = "#f7a129", "sour" = "#e1c315", "alcohol/fermented" = "#b09733", "papery/musty" = "#9db2b7", "chemical" = "#76c0cb", "burnt" = "#be8663", "cereal" = "#ddaf61", "brown spice" = "#b14d57", "nutty" = "#c78869", "cocoa" = "#bb7449", "brown sugar" = "#d35a59"
)

white_circle <- data.frame(
  x = 0,
  y = sum(flavours_H$Freq)
)



ggplot() +
  geom_bar(
    data = flavours_H, mapping = aes(x = 5, y = Freq, fill = Var1), 
    position = "stack", stat = "identity", width = 1
  ) +
  geom_bar(
    data = mid_group_H,
    mapping = aes(x = 3.48, y = Freq, fill = mid_group),
    position = "stack", stat = "identity", width = 2
  ) +
  geom_bar(
    data = inner_group_H,
    mapping = aes(x = 1, y = Freq, fill = inner_group),
    position = "stack", stat = "identity", width = 2.9
  ) +
  geom_col(
    data = white_circle,
    mapping = aes(x = x, y = y),
    fill = "gray90", width = 0.84
  ) +
  scale_fill_manual(values = scale_color_SCA) +
  geom_text(
    data=label_data_outer, 
    aes(x = 5.55, y = Freq_position, label= Var1, angle = angle),
    color = "black", fontface = "bold",alpha = 0.6, size = 3, 
    position = position_identity(), hjust = "outward"
  ) + 
  geom_text(
    data = label_data_mid,
    mapping = aes(x = 4.35, y = Freq_position , label = mid_group, angle = angle), 
    color = "black", fontface = "bold",alpha = 0.6, size = 3,
    position = position_identity(),
    hjust = "inward",check_overlap = T
  ) +
  geom_text(
    data = label_data_inner,
    mapping = aes(x=2.35, y = Freq_position , label = inner_group, angle = angle),
    color = "black", fontface = "bold",alpha = 0.6, size = 3,
    position = position_identity(), hjust = "inward"
  ) +
  geom_text_repel(
    data=label_data_removed,
    mapping = aes(x=5.5, y= Freq_position , label= Var1),
    color="black", fontface="bold",alpha=0.6, size=3, min.segment.length = 0,
    nudge_x = 2, segment.alpha = 0.2
  ) +
  coord_polar(theta = "y", direction = -1) + 
  theme_void() +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "gray90", color = "black"),
        plot.title = element_text(hjust = 0.5, vjust = -6, size = 18,
                                  color = "black", face = "bold")
  ) +
  ggtitle("Häufigkeitsverteilung der Kaffee-Flavour")









