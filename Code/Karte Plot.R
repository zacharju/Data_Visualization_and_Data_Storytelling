library(rnaturalearth) # für ne_countries
library(tidyverse)
library(rnaturalearthdata)
library(sf)

# Die Kartendaten werden in world gespeichert
world <- ne_countries(scale = 110, returnclass = "sf")

#Laden der Daten und entfernen von na werten
coffee <- read_csv("./Daten/simplified_coffee.csv")



#Wir brauchen eine Tabelle welche die Mittelpunkte des Landes in form von lat und lon enthält sowie die Anzahl der Kaffees aus diesem Land

#1. Schritt: Ziehen der Geodaten für alle Herkunftsländer des Kaffees:

# Liste der Länder

Herkunftsländer <- unique(coffee$origin)
Herkunftsländer = Herkunftsländer[-which(Herkunftsländer == "Hawai'I")] 
Herkunftsländer[grep("Democratic Republic Of The Congo", Herkunftsländer)] = "Republic of the Congo"



# Länder filtern
Herkunftsländer_sf <- world[world$name_long %in% Herkunftsländer, ]

# Mittelpunkte der Länder berechnen
mittelpunkte <- data.frame(
  name = Herkunftsländer_sf$name,
  lon = st_coordinates(st_centroid(Herkunftsländer_sf))[ ,1],
  lat = st_coordinates(st_centroid(Herkunftsländer_sf))[ ,2]
)



bundesstaaten <- ne_states(country = "United States of America", returnclass = "sf")

# Hawaii ist kein Land sondern ein State von den USA -> Laden der Bundesstaten der USA und Auswahl von Hawaii
hawaii_sf <- bundesstaaten[bundesstaaten$name == "Hawaii", ]

# Mittelpunkt von Hawaii berechnen
mittelpunkt_hawaii <- data.frame(
  name = "Hawaii",
  lon = st_coordinates(st_centroid(hawaii_sf))[ ,1],
  lat = st_coordinates(st_centroid(hawaii_sf))[ ,2]
)

#anfügen der Zeile Hawaii an den Dataframe mit den Kartendaten
mittelpunkte <- rbind(mittelpunkte, mittelpunkt_hawaii)

#Häufigkeitstabelle mit den Anz. der Kaffees pro Land
origin_H = coffee |> 
  group_by(origin) |> 
  summarise(Freq = n())
#Für den späteren left_join müssen die Ländernamen der Länder in beiden Key Spalten für alle Länder übereinstimmen
setdiff(origin_H$origin, mittelpunkte$name)
#Angleichen der Namen
origin_H <- origin_H |> 
  #Democratic Republic Of The Congo -> Congo
  mutate(origin = ifelse(origin == "Democratic Republic Of The Congo", "Congo", origin)) |> 
  #Dominican Republic -> Dominican Rep.
  mutate(origin = ifelse(origin == "Dominican Republic", "Dominican Rep.", origin)) |> 
  #Hawai'I -> Hawaii
  mutate(origin = ifelse(origin == "Hawai'I", "Hawaii", origin))
#left join
origin_H_sf = origin_H |>  
  left_join(mittelpunkte, by = join_by(origin == name))
 
 
# Plotten


ggplot() + 
  geom_sf(data = world, colour = "grey30", fill = "antiquewhite") + 
  geom_point(data = origin_H_sf, mapping = aes(x = lon, y = lat, size = Freq), colour = "red") + 
  coord_sf() +
  labs(x = "Längengrad", y = "Breitengrad", size = "Anzahl der Kaffees", title = "Herkunftsländer des Kaffees")+
  scale_x_continuous(expand = c(0, 0), breaks = seq(-180,180, 40)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(-90, 90, 20)) +
  theme_bw() +
  theme(
    panel.grid.major = element_line(color = gray(.5), linetype = "dashed", linewidth = 0.5),
    plot.title = element_text(face = "bold", size = 15)
    ) 



