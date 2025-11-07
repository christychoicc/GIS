1+5
4*5^2

A <- 1
B <- 2
C <- A+B
C

  # to type this <-, press type alt - 
ls()
  # list

rm(A)
  # rm() means remove, in this case removed a, b, A 

function(object, arguement1, arguement2, argument3)
  # function can be thought of as single or multiple calculations that you apply to objects

X <- function(data, argument1, arguement2, arguement3)

  # create some datasets, first a vector of 1-100 and 101-200
Data1 <- c(1:100)
Data2 <- c(101:200)
  # plot the data
plot(Data1, Data2, col="red")

#for normally distributed
#vectors of 100 numbers
Data3 <- rnorm(100, mean = 53, sd=34)
Data4 <- rnorm(100, mean = 64, sd=14)
#plot
plot(Data3, Data4, col="blue")

df <- data.frame(Data1, Data2)
plot(df, col="green")

library(tidyverse)
#show the first 10 and last 10 rows of data in df:
df %>%
  head()
#head used the first numerical rows present in the input data frame

df %>%
  tail()

#data.frame[row, column]
#when programming refer to different elements in a data frame and refer to specific ranges or elements of rows and columns using [] operator

df[1:10, 1]
df[5:15,]
df[c(2,3,6),2]
df[,1]

#use the rename() function from dplyr that is loaded as a part of tidyverse package
library(dplyr)
df <- df %>%
  dplyr::rename(column1 = Data1, column2=Data2)
#dplyr is a grammar of data manipulation, has multiple verbs that allow you to change your data into a suitable format,
#this includes select(), filter(), summarise(), also applied to groups in dataset using group_by(), dplyr
#Majority times dplyr followed by %>% and another %>% if more verbs of functions needed
#you can also assign the output to an object with <- 

df %>%
  dplyr::select(column1)
#select() is used to pick selected variables or features in a df or tibble
df$column1
#access a specific data in a column
df[["column1"]]
#can also use double square brackets [[]] when there's a lot of columns and wish to efficiently extract one of them

#check your work directory first use the operator getwd()
LondonDataOSK<- read.csv("ward-profiles-excel-version.csv", 
                          header = TRUE,
                          sep = ",",
                          encoding = "latin1")

LondonDataOSK<- read.csv("ward-profiles-excel-version.csv",
                         sep=",") 
#sep= is a named arguement that usually means "separator"

LondonDataOSK<- read.csv("ward-profiles-excel-version.csv",
                         header = TRUE, sep = ",", encoding = "latin1")
install.packages("here")
#package used for more intuitive to find and load files
library(here)
here::here()


#now have removed any need for ever using a / or \\ in file path

LondonDataOSK <- read_csv("https://data.london.gov.uk/download/f33fb38c-cb37-48e3-8298-84c0d3cc5a6c/772d2d64-e8c6-46cb-86f9-e52b4c7851bc/ward-profiles-excel-version.csv",
                          locale=locale(encoding= "latin1"), 
                          na = 'n/a')
#locale = locale(encoding = "latin1") is basically the encoding of the data (how it is stored)
#locale is your language setting, controls things like: number formating, date/time names, sorting and case rules, message language, character encoding...

class(LondonDataOSK)
#this is just like type() in python

Datatypelist <- LondonDataOSK %>%
  summarise_all(class) %>%
  pivot_longer(everything(),
               names_to="All_variables",
               values_to="Variable_class")
Datatypelist


#attempt without excluding the 'n./a' values in the file
LondonDataOSK <- read_csv("https://data.london.gov.uk/download/f33fb38c-cb37-48e3-8298-84c0d3cc5a6c/772d2d64-e8c6-46cb-86f9-e52b4c7851bc/ward-profiles-excel-version.csv",
                          locale = locale(encoding = "latin1"))
class(LondonDataOSK)
#this is just like type() in python


Datatypelist2 <- LondonDataOSK %>%
  summarise_all(class) %>%
  pivot_longer(everything(),
               names_to="All_variables",
               values_to="Variable_class")

Datatypelist2

summary(df)

LondonDataOSK%>%
  colnames()%>%
  head()  #only look at top 5

#we want to select borough data which is between row 626 and 658
#the comma (,) + leaving the after empty means include all columns
LondonBoroughs <- LondonDataOSK[626:658,]
#can also use deplyr or slice() to do same thing
#LondonBoroughs <- LondonDataOSK%>%
 # dplyr::slice(626:658) #the format is package::function; slice() always work on rows by design!! It just knows rows automatically

Femalelifeexp <- LondonDataOSK%>%
  filter(`Female life expectancy -2009-13`> 90.0)

#New code column cannot be filtered as its in str format
#therefore used str_detect() and combine with filter()
install.packages("stringr")
library(stringr)

LondonBoroughs <- LondonDataOSK %>%
  filter(str_detect(`New code`, "^E09")) #E09 refers to the things that start with E09 not the cell of E9 huhhh

#check if it work
LondonBoroughs$`Ward name`

LondonBoroughs %>%
  dplyr::select(`Ward name`) %>%
  print()

LondonBoroughs <- LondonBoroughs%>%
  distinct()

#select columns if we know which index we want
LondonBoroughs_manualcols <- LondonBoroughs[,c(1, 19, 20,21)]

#select columns 1, 19, 20 and 21
LondonBoroughs_dplyrcols <- LondonBoroughs%>%
  dplyr::select(c(1,19,20,21))

LondonBoroughs_contains <- LondonBoroughs%>%
  dplyr::select(contains("expectancy"),
                contains("obese - 2011/12 to 2013/14"),
                contains("Ward name"))


### 2.5.5.3. RENAMING COLUMNS
install.packages("janitor")

library(janitor)

#changed the column called Ward name to Borough
LondonBoroughs <- LondonBoroughs%>%
  dplyr::rename(Borough= `Ward name`)%>%
  clean_names()   #this comes from the janitor package
# clean_names will change e.g. Total Population --> total_population





### 2.5.5.4. MORE dplyr VERBS
#either load in library(dplyr) or type dplyr:: in front of the below codes (did that here this time)
Life_expectancy <- LondonBoroughs%>%
  #new column with average of male and female life expectancy
  dplyr::mutate(averagelifeexpectancy= (female_life_expectancy_2009_13 + 
                                   male_life_expectancy_2009_13)/2)%>%
  #new column with normalised life expectancy
  dplyr::mutate(normalisedlifeexpectancy= averagelifeexpectancy /
         mean(averagelifeexpectancy))%>%
  #select only columns we want
  dplyr::select(new_code,
                borough,
                averagelifeexpectancy,
                normalisedlifeexpectancy)%>%
  #arrange in descending order
  #ascending is the default and would be
  #arrange (normalisedlifexpectancy)
  dplyr::arrange(desc(normalisedlifeexpectancy))

#top 5 of data
dplyr::slice_head(Life_expectancy, n=5)

#find the last 5 too here, can also find the max and min same format!!
dplyr::slice_tail(Life_expectancy, n=5)





###2.5.5.5. Levelling up with dplyr
#now we compare life expectancy of London Borough with the UK avcerage of 81.16
Life_expectancy2 <- Life_expectancy%>%
  dplyr::mutate(UKcompare = dplyr::case_when(averagelifeexpectancy>81.16 ~ "abovce UK average",
                                      TRUE ~ "below UK average"))
Life_expectancy2

#now find out the range of life expectancies for London Boroughs that are above national average
library(dplyr)
Life_expectancy2_group <- Life_expectancy2%>%
  dplyr::mutate(UKdiff=averagelifeexpectancy-81.16)%>%
  dplyr::group_by(UKcompare)%>%
  dplyr::summarise(range=max(UKdiff)-min(UKdiff), count=n(), Average=mean(UKdiff))

Life_expectancy2_group

#now find the difference between the life expectancy of the Boroughs compared to national average
library(stringr)
Life_expectancy3 <- Life_expectancy%>%
  dplyr::mutate(UKdiff=averagelifeexpectancy-81.16)%>%
  dplyr::mutate(dplyr::across(where(is.numeric), round, 3))%>%
  dplyr::mutate(dplyr::across(UKdiff, round, 0))%>%
  dplyr::mutate(UKcompare=dplyr::case_when(averagelifeexpectancy >=81~
                               str_c("equal or above UK average by",
                                     UKdiff,
                                     "years",
                                     sep=" "),
                             TRUE~str_c("below UK average by",
                                        UKdiff,
                                        "years",
                                        sep=" ")))%>%
  group_by(UKcompare)%>%
  summarise(count=n())

Life_expectancy3

#use the info we made up there and make into a plot or map
#for example reusing the code above:
library(dplyr)

Life_expectancy4 <- Life_expectancy%>%
  dplyr::mutate(UKdiff=averagelifeexpectancy - 81.16)%>%
  dplyr::mutate(across(is.numeric, round, 3))%>%
  #across->apply a function across multiple columns
  #when(is.numeric)->selects only numeric columns
  #round all numeric cokumns into, 3 decimal places
  dplyr::mutate(across(UKdiff, round, 0))




###2.5.6. Plotting the graph
plot(LondonBoroughs$male_life_expectancy_2009_13,
     LondonBoroughs$percent_children_in_reception_year_who_are_obese_2011_12_to_2013_14)





###Pimp my graph!
install.packages("plotly")

library(plotly) #create interactive charts in R
plot_ly(LondonBoroughs,
        #data for x axis
        x = ~male_life_expectancy_2009_13,
        #data for y axis
        y = ~percent_children_in_reception_year_who_are_obese_2011_12_to_2013_14,
        #attribute to display when hovering
        text = ~borough, #when you move mouse over a point, will show borough name!!
        type = "scatter",
        mode = "markers") #specify that each data is marked with a dot





###Spatial Data in R
install.packages(c("tmap"))

install.packages(c("tmaptools","sf"))


