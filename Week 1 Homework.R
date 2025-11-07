library(sf)
shape <- st_read("C:\\Users\\choic\\OneDrive\\Desktop\\CASA0005GIS\\Week 1\\Week 1 GIS Homework\\statsnz-territorial-authority-2018-generalised-SHP (1)")

summary(shape)

plot(shape)

library(sf)
shape %>% 
  st_geometry() %>%
  plot()

library(tidyverse)
mycsv <-  read_csv("C:\\Users\\choic\\OneDrive\\Desktop\\CASA0005GIS\\Week 1\\Week 1 GIS Homework\\paid_employees.csv")

mycsv

shape <- shape%>%
  merge(.,
        mycsv,
        by.x="TA2018_V1_", 
        by.y="Area_Code")

shape%>%
  head(., n=10)

library(tmap)
tmap_mode("plot")

shape %>%
  qtm(.,fill = "Paid_employee")

my_map
