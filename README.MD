## Data Visualization und Data Storytelling – Kaffee-Datensatz

Dieses Projekt entstand im Rahmen des Seminars **Data Visualization & Storytelling** an der Universität Augsburg, Augsburg (02/2024 – 05/2024).  
Ziel war die Erstellung von Visualisierungen zu einem **Kaffee-Datensatz** (Quelle: [Kaggle](https://www.kaggle.com/datasets/schmoyote/coffee-reviews-dataset/data)) unter Anwendung von ggplot2.

---

### Projektbeschreibung

Im Projekt wurden verschiedene Visualisierungsmethoden umgesetzt, um den Datensatz und seine Struktur anschaulich zu analysieren.  
Erstellt wurden:

- Ein **SCA-Flavour-Wheel** zur Darstellung sensorischer Merkmale
- Eine **Weltkarte** der Anbauländer
- Ein **Raincloud-Plot** zur Preisverteilung und -analyse

Das begleitende Dokument `Ausarbeitung.qmd` beschreibt die Entwicklung der Plots Schritt für Schritt und erläutert das methodische Vorgehen.  

---

### Ordnerstruktur

```
Data_Visualization_and_Data_Storytelling/
├── Code/                         # Skripte zur Datenvorbereitung und Visualisierung
├── Daten/                        # Datensätze: (Coffee Simplified) und (Capitals)
├── Ausarbeitung.qmd              # Quarto-Datei mit ausführlicher Dokumentation
├── Data_Visualization_and_Data_Storytelling.Rproj
├── Data_Visualization_und_Data_Storytelling.pdf  # Folien zum Vortrag
├── .gitignore
```

---

### Ausführung

1. Repository klonen oder herunterladen:
   ```bash
   git clone https://github.com/zacherju/Data_Visualization_und_Data_Storytelling.git
   ```
2. Das R-Projekt `Data_Visualization_and_Data_Storytelling.Rproj` öffnen.
3. Die Quarto-Datei **`Ausarbeitung.qmd`** rendern:
   ```r
   quarto::render("Ausarbeitung.qmd")
   ```
   → Dadurch wird die PDF-Datei `Ausarbeitung.pdf` erzeugt.

---

### Inhalt der Folien

- Tidy Data
- The Grammar of Graphics
- Das Paket ggplot2
- Vorstellung eigener Visualisierungen

Begleitend dazu wurde eine 60-minütige Präsentation gehalten.

---

### Literatur

- Anscombe, F. J. (1973). *Graphs in Statistical Analysis*. *The American Statistician*, 27(1), 17–21.
- Spencer, M. et al. (2016). *Using single free sorting and multivariate exploratory methods to design a new coffee taster’s flavor wheel.* *Journal of Food Science*, 81(12), S2997–S3005. 
- Wickham, H. (2010). *A Layered Grammar of Graphics.* *Journal of Computational and Graphical Statistics*, 19(1), 3–28.
- Wickham, H. (2016). *ggplot2: Elegant Graphics for Data Analysis.* Springer.
- Wickham, H., Çetinkaya-Rundel, M., & Grolemund, G. (2023). *R for Data Science.* O’Reilly Media.
- Wilkinson, L. (2012). *The Grammar of Graphics.* Springer Berlin Heidelberg.

---

### Autoren

Hanan Loulou und Julian Zacharias

---

### Lizenz

Dieses Projekt ist ausschließlich zu akademischen Zwecken erstellt worden (Seminararbeit)  
Alle Datensätze stammen aus öffentlichen Quellen (Kaggle)
