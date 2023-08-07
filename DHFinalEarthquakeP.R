library(readxl)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(leaflet)
library(tidyverse)
library(dplyr)
library(viridis)
library(ggthemes)
library(htmltools)
library(plotly)
library(ggforce)
library(ggplot2)
library(ggplotify)
library(jpeg)
library(grid)
library(ggimage)
library(dbplyr)
library(tidyr)
library(janitor)
library(sp)
library(rworldmap)
library(RCurl)
library(devtools)
library(leaflet)
library(htmlwidgets)
library(hrbrthemes)
require(scales)
library(stringr)




# Importing data



earthquakes_dataset <- read_excel("earthquakes_dataset.xlsx")
countryincomeinfo <- read_excel("Countryincomeinfo.xlsx")
landslides <- read_excel("landslides.xlsx")
countriesSP <- getMap(resolution='low')

# Joining data sets

# The first data set is for all earthquakes between 2000-2021
earthquake_income <- left_join(earthquakes_dataset,countryincomeinfo,
                               by="Country")

earthquake_allrecords = earthquake_income %>% select( c("Year", "Country", 
                                                        "Location", 
                                                        "Latitude", "Longitude", 
                                                        "Focal Depth (km)", 
                                                        "Mag", "Deaths", "Missing",
                                                        "Injuries", "Total Damages", 
                                                        "Houses Destroyed", 
                                                        "Houses Damaged", 
                                                        "Total Deaths", 
                                                        "IncomeGroup"))
# 1. Earthquake Income Group total Damages and mean magnitudes over 2000-2020
all_countries = earthquake_allrecords %>%
  group_by(IncomeGroup, Year) %>% 
  summarise(damages=sum(`Total Damages`, na.rm =TRUE), avgMag=mean(Mag, na.rm = TRUE)) %>% drop_na()

# Removing all zeros
all_countries2 <- filter(all_countries, damages != 0.000)

#Creating Scatterplot
p <- ggplot(all_countries2, aes(x = Year, y = damages, color = IncomeGroup, Mag=avgMag)) +
  geom_jitter(width = .2) + labs(title = "Earthquake Damages Over Time",
                                 subtitle = "(Damages over 2000-2020 in different income level Countries)",
                                 y = "Sum of Damages in Millions", x = "Years") + facet_wrap(~ IncomeGroup) + 
  scale_y_continuous(breaks = scales::breaks_width(10000)) + theme(axis.text.y = element_text(size = 7)) + theme(axis.text.x = element_text(size = 7)) 


#Adjusting the label titles to make them fit
gp <- ggplotly(p, tooltip= c("Mag","y","x"))
gp %>% layout(margin = list(l = 75))
str(gp[['x']][['layout']][['annotations']]) 
gp[['x']][['layout']][['annotations']][[2]][['x']]
gp[['x']][['layout']][['annotations']][[2]][['x']] <- -0.1

#Scatter plot with Tool tip
gp %>% layout(margin = list(l = 75))


#2.Top 16 Countries that have had the most earthquakes over 2000-2020
#looking at number of deaths as well as average magnitudes,
#even though a country may have the highest number of earthquakes the
#ratio of the number or earthquakes with deaths vary greatly
top16country_deaths  = earthquake_allrecords %>% group_by(Country) %>% 
  summarise(numberofearthquakes = n(), death=sum(`Total Deaths`,na.rm = TRUE), avgMag=mean(Mag, na.rm = TRUE)) %>% 
  filter(rank(desc(numberofearthquakes))<=16) %>% arrange(desc(numberofearthquakes))
# Removing all zeros
top16country_deaths2 <- filter(top16country_deaths, death != 0.000)

# Top 10 Countries deaths over the years 
#looking at the most active countries and how many deaths per year over period 
#of 2000-2020
target <- c("China","Indonesia", "Iran", "Japan", "United States", "India", 
            "Turkey", "Philippines", "Peru", "Chile")
country_deaths  = earthquake_allrecords %>% group_by(Country,Year) %>% 
  summarise(numberofearthquakes = n(), death=sum(`Total Deaths`,na.rm = TRUE), 
            avgMag=mean(Mag, na.rm = TRUE)) %>% filter(Country %in% target)

#Creating bar plot 
c <- ggplot(country_deaths, aes(x = Year, y = death, fill = Country, 
                                numofearthquakes=numberofearthquakes,
                                deaths=death)) + 
  geom_bar(position = "dodge", stat  = "identity") +
  ggtitle("Earthquake Deaths over Years by Countries ") + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) + 
  theme(axis.text.y = element_text(size = 7)) + 
  theme(axis.text.x = element_text(size = 7)) 



# Making barplot interactive
ggplotly(c, tooltip= c("numofearthquakes","deaths","x"))

#3.Creating an interactive Map

#Here i am creating a world map with all earthquakes from 2000-2020 the markers 
# colors are based on the magnitude of the earthquakes strength light is 4-5 mag
# which is yellow, moderate is 5-6 mag which is red and strong is 6-7 mag which
#is black, from the map you can tell that certain areas of China and the Middle 
#East are very active. 
      
earthquake_map <- earthquake_allrecords %>% select(Country, Location, Latitude, Longitude, Year, Mag)
str(earthquake_map)
earthquake_map1 <- na.omit(earthquake_map)

earthquake_map1$Mag = cut(earthquake_map1$Mag,
                       breaks = c(4, 5, 6, 7), right = FALSE,
                       labels = c("Light", "Moderate", "Strong"))

pal = colorFactor(palette = c("yellow", "red", "black"), domain=earthquake_map1$Mag) 

m <- leaflet(earthquake_map1) %>% addProviderTiles(providers$OpenStreetMap.Mapnik) %>% 
  setView(-73.931180, 4.5385453, zoom = 7) %>% 
  addCircleMarkers( lng = ~Longitude, lat = ~Latitude, popup=~Location, weight = 3, radius=4, 
                   color= ~ pal(Mag), stroke = F, fillOpacity = 0.5,) 
m

#4.Looking at the type of fault by earthquakes that produced a landslide 
#What Country is prone to faults and how many deaths resulted by them. 

#What are the most prominent fault type from 1772-2020, how many deaths
# did they cause as well as the average movement magnitude (Mw)
faulttypes_deaths = landslides %>% group_by(`Fault Type`) %>% 
  summarise(faultcounts=n(),deaths=sum(`Total Fatalities`,na.rm = TRUE), 
            avgMag=mean(Mw, na.rm = TRUE))

#Creating a Pie Chart 

slices <- c(42,99,93)
lbls <- c("Normal", "Strike Slip", "Thrust/Reverse")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls,pct)
lbls <- paste(lbls,"%",sep="")

pie(slices,labels = lbls, col = rainbow(length(lbls)),
    main = "What is the most common fault type in Earthquakes between 1772-2020")


# 5.Grouping by Country as well to determine which Country is most susceptible
# to that fault type
faulttypes_deaths2 = landslides %>% group_by(`Fault Type`, Country) %>% 
  summarise(faultcounts=n(),deaths=sum(`Total Fatalities`,na.rm = TRUE), 
            avgMag=mean(Mw, na.rm = TRUE))

#Creating bar plot 
Ft <- ggplot(faulttypes_deaths2, aes(x = `Fault Type`, y = deaths, fill = Country, 
                                avgMag=avgMag, death=deaths, country=Country)) + 
  geom_bar(position = "dodge", stat  = "identity") +
  ggtitle("Earthquake Fault Types Vs Deaths over Years by Countries ") + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) + 
  theme(axis.text.y = element_text(size = 7)) + 
  theme(axis.text.x = element_text(size = 7)) 

ggplotly(Ft, tooltip= c("avgMag","death","country"))
