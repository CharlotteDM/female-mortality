library(ggplot2)
library(tidyverse)
library("forcats")
library(plotly)
library("rworldmap")?
  library("rworldxtra")?
  library("ggmap")?
  library("grid")?
  library("gridExtra")?
  library(sp)
library(gifski)

install.packages("eurostat")?
  library("eurostat")?
  library("leaflet")?
  library(sf)
library(scales)
library(cowplot)
library(ggthemes)
library("raster")?
  install.packages("spDataLarge")??
  library("spDataLarge")?
  library(tmap)
library(tmaptools)
library(rnaturalearth)
library("rnaturatlearthdata")
library(rgeos)
library(RColorBrewer)
library(rstudioapi)
install.packages("conflicted")
install.packages("gganimate")
install.packages("gapminder")

path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(path)
#Data source: https://data.worldbank.org/indicator/SH.DYN.NCOM.FE.ZS 

mortality_fem <- read.csv("data/mortality_data.csv",  
                          stringsAsFactors = F)
head(mortality_fem)
str(mortality_fem)

#mortality in 2019
mort2019 <- dplyr::select(mortality_fem, Country.Name, Country.Code, X2019) 

#finding country with the highest mortality rate in 2019
max(mort2019$X2019, na.rm = TRUE) 
#---max(mort2019$X2019)
the_highest_MR2019 <- filter(mort2019, X2019 == max(X2019, na.rm = TRUE)) #Kiribati - 43.7

#finding country with the lowest mortality rate in 2019
min(mort2019$X2019, na.rm = TRUE)
the_lowest_MR2019 <- filter(mort2019, X2019 == min(X2019, na.rm = TRUE)) #Korea Płd - 4.4

#mortality in 2000
mort2000 <- select(mortality_fem, Country.Name, Country.Code, X2000)

#finds country with the highest mortality rate in 2000
max(mort2000$X2000, na.rm = TRUE)
the_highest_MR2000 <- filter(mort2000, X2000 == max(X2000, na.rm = TRUE)) #Kiribati - 47.8

#finds country with the lowest mortality rate in 2000
min(mort2000$X2000, na.rm = TRUE)
the_lowest_MR2000 <- filter(mort2000, X2000 == min(X2000, na.rm = TRUE)) #Japan 7.5


#selects data from 2000 to 2019
mortality_fem_2000_2019 <- dplyr::select(
  mortality_fem, Country.Name, Country.Code, X2000:X2019)

#adds column with names of continents
mortality_fem_2000_2019 <- left_join(mortality_fem_2000_2019, countries_world, by = "Country.Code")

head(mortality_fem_2000_2019)
str(mortality_fem_2000_2019)

#ggplot: "Mortality fem 2019 - world"
gg_world <- ggplot(data = mortality_fem_2000_2019) + 
  geom_col(aes(x = reorder(Country.Name, X2019), y = X2019, fill = X2019)) + 
  scale_fill_gradient(low="blue", high="red") +
  coord_flip() +
  facet_wrap(~ continent) +
  theme_light() +
  labs(
    title = "Mortality from CVD, cancer, diabetes, CRD in 2019 (%)",
    subtitle = "Females, ages between 30 and 70",
    caption = "(based on data from: https://data.worldbank.org/indicator/SH.DYN.NCOM.FE.ZS)",
    x = "Country",
    y = "Mortality, fem (%)") +
  theme(
    plot.title = element_text(color="royalblue4", size=14, face="bold"),
    axis.title.x = element_text(color="steelblue2", size=14, face="bold"),
    axis.title.y = element_text(color="steelblue2", size=14, face="bold"),
    legend.position = "none") 

gg_world

cont <- ggplot(data = mortality_fem_2000_2019) +
  geom_point(mapping = aes(x = Country.Name, y = X2019), color = "blue") +
  facet_wrap(~ continent,nrow = 1, scales = "free_x") +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  labs(
    title = "Mortality from CVD, cancer, diabetes, CRD in 2019 (%) in Europe",
    subtitle = "Females, ages between 30 and 70",
    caption = "(based on data from: https://data.worldbank.org/indicator/SH.DYN.NCOM.FE.ZS)",
    x = "Country",
    y = "Mortality, fem (%)") +
  theme(
    plot.title = element_text(color="royalblue4", size=14, face="bold"),
    axis.title.x = element_text(color="steelblue2", size=14, face="bold"),
    axis.title.y = element_text(color="steelblue2", size=14, face="bold"))

cont

#interactive: "Mortality (fem) in 2019 in whole world"
ggplotly(gg_world)
ggplotly(cont)

#ggplot: "Mortality(fem) in Europe Countries"
mortalityInEurope <- mortality_fem_2000_2019%>%
  filter(continent %in% c("Europe"))

ggplot(data = mortalityInEurope, aes(y=reorder(Country.Name, X2019), x=X2019, fill=X2019)) + 
  geom_tile() +
  scale_fill_gradient(low="blue", high="red") +
  theme_light() +
  labs(
    title = "Mortality from CVD, cancer, diabetes, CRD in 2019 (%) in Europe",
    subtitle = "Females, ages between 30 and 70",
    caption = "(based on data from: https://data.worldbank.org/indicator/SH.DYN.NCOM.FE.ZS)",
    y = "Country",
    x = "Mortality, fem (%)",
    fill = "Mortality (%)") +
  theme(
    plot.title = element_text(color="royalblue4", size=14, face="bold"),
    axis.title.x = element_text(color="steelblue2", size=14, face="bold"),
    axis.title.y = element_text(color="steelblue2", size=14, face="bold"),
    legend.position = "right")  

#boxplot - comparison of mortality across continents
ggplot (data = mortality_fem_2000_2019, (aes(continent,X2019, color=continent))) +
  geom_boxplot() +
  geom_jitter(width=0.15, alpha=0.3) +
  labs(
    title = "Mortality from CVD, cancer, diabetes, CRD in 2019 (%)",
    subtitle = "Females, ages between 30 and 70",
    caption = "(based on data from: https://data.worldbank.org/indicator/SH.DYN.NCOM.FE.ZS)",
    x = "Continent",
    y = "Mortality, fem (%)") +
  theme(
    plot.title = element_text(color="royalblue4", size=14, face="bold"),
    axis.title.x = element_text(color="steelblue2", size=14, face="bold"),
    axis.title.y = element_text(color="steelblue2", size=14, face="bold"),
    legend.position = "none")

#prepars map "Mortality from CVD, cancer, diabetes, CRD in 2019 (%)"
world <- ne_countries(scale = "medium", returnclass = "sf")
combined_data_world <- left_join(mortality_fem_2000_2019, world, by = c("Country.Code" = "brk_a3"))

#map: "Mortality from CVD, cancer, diabetes, CRD in 2019 (%)"
ggplot(combined_data_world) +
  geom_sf(aes(geometry = geometry, fill=X2019)) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(
    title = "Mortality from CVD, cancer, diabetes, CRD in 2019 (%)",
    subtitle = "Females, ages between 30 and 70",
    caption = "(based on data from: https://data.worldbank.org/indicator/SH.DYN.NCOM.FE.ZS)",
    fill = "Mortality (%)") +
  theme(
    plot.title = element_text(color="royalblue4", size=14, face="bold"),
    plot.subtitle = element_text(color="slateblue", size=12, face="italic"))



#filter data from UE
EU <- filter (mortality_fem_2000_2019, Country.Code == "POL" | Country.Code == "AUT" |
                Country.Code == "BEL" | Country.Code == "BGR" | Country.Code == "HRV" |
                Country.Code == "CYP" |
                Country.Code == "CZE" | Country.Code == "DNK" | Country.Code == "EST" |
                Country.Code == "FIN" | Country.Code == "FRA" | Country.Code == "GRC" |
                Country.Code == "ESP" | Country.Code == "NLD" | Country.Code == "IRL" |
                Country.Code == "LTU" | Country.Code == "LUX" | Country.Code == "LVA" |
                Country.Code == "MLT" | Country.Code == "DEU" | Country.Code == "PRT" |
                Country.Code == "ROU" | Country.Code == "SVK" | Country.Code == "SVN" |
                Country.Code == "SWE" | Country.Code == "HUN" | 
                Country.Code == "ITA")

#finds EU country with the highest mortality rate in 2019
max(EU$X2019, na.rm = TRUE) 
EUthe_highest_MR2019 <- filter(EU, X2019 == max(X2019, na.rm = TRUE)) #Bulgarria = 16.4

#finds EU country with the lowest mortality rate in 2019
min(EU$X2019, na.rm = TRUE)
EUthe_lowest_MR2019 <- filter(EU, X2019 == min(X2019, na.rm = TRUE)) #Cyprus - 5.7

#ggplot: "Mortality in UE in 2019, fem" (first attempt)
ggplot(data = EU) +
  geom_col(mapping = aes(x = Country.Code, y = X2019, fill = Country.Code, binwidth =  .5)) +
  coord_flip() +
  theme_light() +
  labs(
    title = "Mortality from CVD, cancer, diabetes, CRD in 2019 (%)",
    subtitle = "Females, ages between 30 and 70",
    caption = "(based on data from: https://data.worldbank.org/indicator/SH.DYN.NCOM.FE.ZS)",
    x = "UE Country",
    y = "Mortality, fem (%)",
    fill = "Country") +
  theme(
    plot.title = element_text(color="royalblue4", size=14, face="bold"),
    axis.title.x = element_text(color="steelblue2", size=14, face="bold"),
    axis.title.y = element_text(color="steelblue2", size=14, face="bold")
  )

#ggplot: "Mortality in UE in 2019, fem"  (second attempt)
gg <- ggplot(data = EU) + geom_col(aes(x = reorder(Country.Name, X2019), y = X2019, fill = X2019)) + 
  scale_fill_gradient(low="blue", high="red") +
  coord_flip() +
  theme_light() +
  labs(
    title = "Mortality from CVD, cancer, diabetes, CRD in 2019 (%) in EU",
    subtitle = "Females, ages between 30 and 70",
    caption = "(based on data from: https://data.worldbank.org/indicator/SH.DYN.NCOM.FE.ZS)",
    x = "EU Country",
    y = "Mortality, fem (%)") +
  theme(
    plot.title = element_text(color="royalblue4", size=14, face="bold"),
    axis.title.x = element_text(color="steelblue2", size=14, face="bold"),
    axis.title.y = element_text(color="steelblue2", size=14, face="bold"),
    legend.position = "none") 

ggplotly(gg)



#Europe map
world <- ne_countries(scale = "medium", returnclass = "sf")
Europe <- world[which(world$continent == "Europe"),]
ggplot(Europe) +
  geom_sf() +
  coord_sf(xlim = c(-25,45), ylim = c(35,72), expand = FALSE)


#creates new data frame
combined_data <- left_join(EU, Europe, by = c("Country.Code" = "brk_a3"))

#adding data: GDP 2019
#data source: https://ec.europa.eu/eurostat/statistics-explained/index.php?title=File:Current_healthcare_expenditure,_2019.png
CHE_mill_eu <- c(41483, 50759, 4364, 1562, 17546, 403444, 31137, 
                 113674, 1892, 21992, 269541, 14376, 3785, 9277, 23782, 155249,
                 3420, 3411, 2001, 1110, 82365, 34400, 20392, 12810, 6534, 4125, 51824)
GDP_perc <- c(10.4, 10.7, 7.1, 7, 7.8, 11.7, 10, 9.1, 6.7, 9.2, 11.1, 7.8, 7,
              6.4, 6.7, 8.7, 7, 5.4, 6.6, 9, 10.2, 6.5, 9.5, 5.7, 7, 8.5, 10.9)

#combains vector to data frame
combined_data <- cbind(combined_data, CHE_mill_eu, GDP_perc)


#map: "Mortality from CVD, cancer, diabetes, CRD in 2019 (%) in EU"
ggplot(combined_data) +
  geom_sf(aes(geometry = geometry, fill=X2019)) +
  coord_sf(xlim = c(-15,35), ylim = c(35,72), expand = FALSE) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(
    title = "Mortality from CVD, cancer, diabetes, CRD in 2019 (%) in EU",
    subtitle = "Females, ages between 30 and 70",
    caption = "(based on data from: https://data.worldbank.org/indicator/SH.DYN.NCOM.FE.ZS)",
    fill = "Mortality (%)") +
  theme(
    plot.title = element_text(color="royalblue4", size=14, face="bold"),
    plot.subtitle = element_text(color="slateblue", size=12, face="italic"))


#scatterplott: "Mortality and GDP(%) in 2019, fem"
ggGDP <- ggplot(data = combined_data) +
  geom_point(mapping = aes(x = X2019, y = GDP_perc, color = Country.Name)) +
  theme_light() +
  labs(
    title = "Mortality in 2019 (%) in EU and Current Healthcare Expenditure (% of GDP)",
    subtitle = "Mortality from CVD, cancer, diabetes, CRD in 2019 (%) females, ages between 30 and 70",
    caption = "(based on data from: https://data.worldbank.org/indicator/SH.DYN.NCOM.FE.ZS
    https://ec.europa.eu/eurostat/statistics-explained/index.php?title=File:Current_healthcare_expenditure,_2019.png)",
    x = "Mortality, fem (%), 2019",
    y = "Current Healthcare Expenditure (% of GDP)", 
    col = "EU Country") +
  theme(
    plot.title = element_text(color="royalblue4", size=12, face="bold"),
    plot.subtitle = element_text(color="slateblue", size=8, face="italic"))

#interactive plot: "Mortality and GDP(%) in 2019, fem"
ggplotly(ggGDP)


#how to add subtitle and caption to ggplotly
ggplotly(ggGDP) %>%
  layout(title = list(text = paste0("Mortality in 2019 (%) in EU and Current Healthcare Expenditure (% of GDP)",
                                    '<br>',
                                    '<sup>',
                                    "Mortality from CVD, cancer, diabetes, CRD in 2019 (%) females, ages between 30 and 70",
                                    '<br>',
                                    '<sub>',
                                    
                                    "<i>(based on data from: https://data.worldbank.org/indicator/SH.DYN.NCOM.FE. ; < br /> https://ec.europa.eu/eurostat/statistics-explained/index.php?title=File:Current_healthcare_expenditure,_2019.png)</i>")))
ggplotly(ggGDP) %>%
  labs()


#correlation between Mortality(fem) and Current Healthcare Expenditure (% of GDP)
cor(combined_data$X2019, combined_data$GDP_perc)


#adding data physicians2018
#data source: https://www.oecd-ilibrary.org/sites/1d767767-en/index.html?itemId=/content/component/1d767767-en
######"Note: The EU average is unweighted. 1. Data refer to all doctors licensed to practice, resulting in a large over-estimation of the number of practising doctors (e.g. of around 30% in Portugal). 2. Data include not only doctors providing direct care to patients, but also those working in the health sector as managers, educators, researchers, etc. (adding another 5-10% of doctors).
physicians2018 <- c(5.2, 3.1, 4.2, 4.1, 4, 4.3, 4.2, 4, 3.5, 3.2, 3.2, 
                    6.1, 3.4, 3.4, 3.3, 4, 4.6, 3, 3.3, 4, 3.7, 
                    2.4, 5.2, 3.1, 3.5, 3.2, 4.3) #physicians/1000 patients

#combaining vector to data frame
combined_data <- cbind(combined_data, physicians2018)

####People at risk of poverty or social exclusion in 2019 (%)
#data source: https://ec.europa.eu/eurostat/databrowser/view/t2020_50/default/table?lang=en
risk_poverty <- c(16.9, 19.5, 32.8, 22.3, 12.5, 17.4, 16.3, 25.3, 24.3, 
                  15.6, 17.9, 30, 23.3, 18.9, 20.6, 25.6, 26.3, 20.6, 27.3, 
                  20.1, 16.5, 18.2, 21.6, 31.2, 16.4, 14.4, 18.8)
combined_data <- cbind(combined_data, risk_poverty)


#max and min Risk of Poverty or Social Exclusion in EU in 2019
min(combined_data$risk_poverty)
min_risk_pov <- filter(combined_data, risk_poverty == min(risk_poverty))
max(combined_data$risk_poverty)
max_risk_pov <- filter(combined_data, risk_poverty == max(risk_poverty))

#Removing columns from data frame
#combined_data <- combined_data[, -c(90:92)]
#combined_data <- combined_data[, - c(101:103)] #
#combined_data <- combined_data[, -89] #

#Renaming columns in data frame

#creating new df's - the biggest Mortality & and the biggest Current Healthcare Expenditure (% of GDP)
big_mortality <- combined_data %>%
  group_by(Country.Name) %>%
  filter(X2019 > 12)
big_GDP <- combined_data %>%
  group_by(Country.Name) %>%
  filter(GDP_perc > 10) 

#simple plots with labels: "The Biggest Mortality (fem) & The Biggest Current Healthcare Expenditure (% of GDP) in EU"
ggplot(data = big_mortality) +
  geom_text_repel(mapping = aes(x = Country.Name, y = X2019, 
                                color = Country.Name, label = Country.Code))


ggplot(data = big_GDP) +
  geom_text_repel(mapping = aes(x = Country.Name, y = X2019, 
                                color = Country.Name, label = Country.Code))

#ggplot with labels: "Mortality (fem) and Number of Physicians in EU" 
ggPHYS <- ggplot(data = combined_data) +
  geom_point(mapping = aes(x = X2019, y = physicians2018, 
                           color = Country.Name, size = GDP_perc)) +
  geom_label_repel(
    aes(x = X2019, y = physicians2018, label = Country.Name, size = 6), 
    data = combined_data
  ) +
  theme_light() +
  labs(
    title = "Mortality in 2019 (%) in EU and Practising Physicians (per 1 000 population)",
    subtitle = "Mortality from CVD, cancer, diabetes, CRD in 2019 (%) females, ages between 30 and 70",
    caption = "(based on data from: https://data.worldbank.org/indicator/SH.DYN.NCOM.FE.ZS
    https://www.oecd-ilibrary.org/sites/1d767767-en/index.html?itemId=/content/component/1d767767-en",
    x = "Mortality, fem (%), 2019",
    y = "Practising Physicians", 
    col = "EU Country") +
  theme(
    plot.title = element_text(color="royalblue4", size=14, face="bold"),
    plot.subtitle = element_text(color="slateblue", size=8, face="italic"),
    plot.caption = element_text(color="deeppink", size=7),
    axis.title.x = element_text(color="darkmagenta", size=10),
    axis.title.y = element_text(color="darkmagenta", size=10)
  ) +
  geom_smooth(mapping = aes(x = X2019, y = physicians2018)) 

#ggplot: "Mortality (fem) and Number of Physicians in EU"
ggPHYS <- ggplot(data = combined_data) +
  geom_point(mapping = aes(x = X2019, y = physicians2018, 
                           color = Country.Name, size = GDP_perc)) +
  theme_light() +
  labs(
    title = "Mortality in 2019 (%) in EU and Practising Physicians (per 1 000 population)",
    subtitle = "Mortality from CVD, cancer, diabetes, CRD in 2019 (%) females, ages between 30 and 70",
    caption = "(based on data from: https://data.worldbank.org/indicator/SH.DYN.NCOM.FE.ZS
    https://www.oecd-ilibrary.org/sites/1d767767-en/index.html?itemId=/content/component/1d767767-en",
    x = "Mortality, fem (%), 2019",
    y = "Practising Physicians", 
    col = "EU Country") +
  theme(
    plot.title = element_text(color="royalblue4", size=14, face="bold"),
    plot.subtitle = element_text(color="slateblue", size=8, face="italic"),
    plot.caption = element_text(color="deeppink", size=7),
    axis.title.x = element_text(color="darkmagenta", size=10),
    axis.title.y = element_text(color="darkmagenta", size=10)
  ) +
  geom_smooth(mapping = aes(x = X2019, y = physicians2018)) 


#correlation between Mortality and Number of Physicians in EU
cor(combined_data$X2019, combined_data$physicians2018)

#correlation between Mortality and Current Healthcare Expenditure (% of GDP) in EU
cor(combined_data$X2019, combined_data$GDP_perc)

#ggplot: "Risk Poverty and Mortality (fem) in UE in 2019"
ggRP <- ggplot(data = combined_data) +
  geom_point(mapping = aes(x = X2019, y = risk_poverty, color = Country.Name)) +
  theme_light() +
  labs(
    title = "Mortality (%) & Risk of Poverty or Social Exclusion in EU in 2019",
    subtitle = "Mortality from CVD, cancer, diabetes, CRD in 2019 (%) females, ages between 30 and 70 & risk poverty",
    caption = "(based on data from: https://data.worldbank.org/indicator/SH.DYN.NCOM.FE.ZS
    https://ec.europa.eu/eurostat/databrowser/view/t2020_50/default/table?lang=en",
    x = "Mortality, fem (%), 2019",
    y = "Risk of Poverty or Social Exclusion", 
    col = "EU Country") +
  theme(
    plot.title = element_text(color="royalblue4", size=14, face="bold"),
    plot.subtitle = element_text(color="slateblue", size=8, face="italic"),
    plot.caption = element_text(color="deeppink", size=7),
    axis.title.x = element_text(color="darkmagenta", size=10),
    axis.title.y = element_text(color="darkmagenta", size=10)
  )

#interactive plot: "Risk Poverty & Mortality (fem) in EU in 2019"
ggplotly(ggRP)

#correlation between Mortality(fem) and Risk Poverty in EU in 2019
cor(combined_data$X2019, combined_data$risk_poverty)
