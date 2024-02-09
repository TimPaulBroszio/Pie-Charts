# Visualization of the Installed Capacity Composition
# Central Electricity Authority India (CEA) 
# Source: https://cea.nic.in/dashboard/?lang=en
# 
# Auhtor: Tim Paul Broszio, M.A., M.A. 
# FernUniversität in Hagen
# Department of International Politics

# Setup----

Sys.Info()

sessionInfo()

Sys.setenv(language = "en")

rm(list=ls())

pac <- "pacman" %in% rownames(installed.packages())
if(!pac) install.packages("pacman")
library(pacman)
rm(pac)
p_load("tidyverse", "readxl", "RColorBrewer", "plotrix")



# Import Data-----

file.choose()

df <- read_xlsx("C:\\Sonstige Unterlagen\\R Projekte\\Datenvisualisierung\\Installed Power Composition\\Installed Power Composition CEA India.xlsx", 
                col_names = TRUE)


df$MW <- as.numeric(df$MW)

df <- as.data.frame(df)

str(df)

# round values to 2 digits

df$`Installed Capacity` <- round(df$`Installed Capacity`, digits = 2)

# remove values below 1% to facilitate visualization

df <- df[-c(3),]

# Visualization----

### BASIC PIE CHART ###

# customize colors


cols <- c("#0ab8ae", "#FFA54F", "#36648B", 
          "#c2d3e3", "#e2c5aa", "#e7f8f7")

#

p1 <- ggplot(df, aes(x = "", y = `Installed Capacity`, fill = Category)) +
  geom_col(color = "black") +
  geom_text(aes(label = `Installed Capacity`),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = cols) + 
  theme_piechart() +
labs(title = "Installed Capacity Category wise", 
     subtitle = "Total Installed Capacity: 416591.379 MW | (Apr-2023)", 
     x = "", 
     y = "", 
     caption = "Data: Central Elecricity Authority of India, 2023 | Not including diesel with a share of 0.14% | 
     Creation: Tim Paul Broszio")

p1

### 3D PIE CHART ###

lab <- paste0(df$`Installed Capacity`, "%") 

lab2 <- paste0(lab," ", df$Category)

p2 <- pie3D(df$`Installed Capacity`, 
            col = cols,
            border = "black", 
            shade = 0.75, 
            labels = lab2, 
            labelcex = 0.75,
            main = "Installed Capacity Category wise")




# exploding 3D pie chart

p3 <- pie3D(df$`Installed Capacity`, mar = rep(1.75, 4),
            col = cols,
            labels = lab2,
            labelcex = 1.5,
            labelcol = "black",
            border = "black", 
            shade = 0.75,
            explode = 0.175,
            main = "Installed Capacity Category wise")





# customize the theme (function)----

theme_piechart <- function() {
  theme_void() +
    theme(
      text = element_text(family = "Halis", color = "#1e1450"),
      plot.title = element_text(size= 19, hjust=0.01, color = "#1e1450", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
      plot.subtitle = element_text(size= 15, hjust=0.01, color = "#1e1450", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
      plot.caption = element_text(size= 9, color = "#1e1450", margin = margin(b = 0.3, r=-99, unit = "cm")),
      plot.background = element_rect(fill = "#f5f5f2", color = NA),
      panel.background = element_rect(fill = "#f5f5f2", color = NA),
      legend.background = element_rect(fill = "#f5f5f2", color = NA)
    )
}






