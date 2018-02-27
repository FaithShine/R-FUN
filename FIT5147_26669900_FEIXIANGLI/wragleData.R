library(tidyr)
library(dplyr)
require(readxl)
library(plotly)

raw_data <- read_excel("../data/suburb_population_statics.xlsx", 1)
raw_cleanData <- raw_data[!is.na(raw_data$Suburb), ]
write.csv(raw_cleanData, file = "../data/data1.csv")
# raw_data <- read.csv('data/data2.csv')
raw_data1 <- plyr::rename(raw_cleanData, c("Postcode"="POSTCODE"))
raw_data1 <- raw_data1 %>% select(-Children, everything())
raw_data1 <- raw_data1 %>% select(-Adult, everything())
raw_data1 <- raw_data1 %>% select(-Elderly, everything())
raw_data1 <- plyr::rename(raw_data1, c("age 0-4, %"="rang0n4","age 5-9, %"="rang5n9", 
                                     "age 10-14, %"="rang10n14", "age 15-19, %"="rang15n19", 
                                     "age 20-24, %"="rang20n24", "age 25-44, %"="rang25n44", 
                                     "age 45-64, %"="rang45n64", "age 65-69, %"="rang65n69", 
                                     "age 70-74, %"="rang70n74", "age 75-79, %"="rang75n79", 
                                     "age 80-84, %"="rang80n84", "age 85+, %"="above85"))
#pivot columns age rang into rows
raw_data1 <- gather(raw_data1, AgeRang, percent, `rang0n4`:`above85`)
# set suburb to uppercase
raw_data2 <- raw_data1 %>% mutate_each(funs(toupper), Suburb)
raw_data3 <- gather(raw_data2, AgeGroup, AG_percent, `Children`:`Elderly`)
# sampling data
# sample_data <- raw_data2 %>% group_by(AgeRang) %>% sample_n(500)
# names(raw_data2)
# raw_data

# load raw property data 
raw_propertyData <- read.csv('../data/property_data.csv')
# class(raw_propertyData$X2005)
# raw_propertyData
raw_propertyData <- plyr::rename(raw_propertyData, c("X2005"="2005","X2006"="2006", "X2007"="2007", "X2008"="2008", "X2009"="2009",
                                                     "X2010"="2010", "X2011"="2011", "X2012"="2012", "X2013"="2013","X2014"="2014",
                                                     "X2015"="2015"))
property.data <- gather(raw_propertyData, year, price, `2005`:`2015`)
# property.data

#load population density data
raw_density_data <- read_xlsx("../data/popupation_density.xlsx", 1)
# uppercase of the suburb
raw_density_data <- raw_density_data %>% mutate_each(funs(toupper), Suburb)
# reshape data
density_data1 <- gather(raw_density_data, year, population, `2005`:`2015`)
# clear the suburb with 0 population
density_data2 <- density_data1[!(density_data1$population==0), ]
density_data2 <- density_data2[!is.na(density_data2$Suburb),]
# nrow(density_data1)
# nrow(density_data2)
names(density_data)

#load crime data
raw_crimeData <- read.csv("../data/crime_data.csv")
raw_crimeData <- plyr::rename(raw_crimeData, c("postcode"="POSTCODE", "X2011"="2011", "X2012"="2012", 
                                               "X2013"="2013","X2014"="2014",
                                               "X2015"="2015"))
crime_data <- gather(raw_crimeData, year, occurence, `2011`:`2015`)

#save data to csv file
write.csv(raw_data3, file="../data/data4.csv")
write.csv(property.data, file ="../data/property_data2.csv" )
write.csv(density_data2, file="../data/pop_density.csv")
crime_data <- plyr::rename(crime_data, c("postcode"="POSTCODE"))
write.csv(crime_data, file = "../data/crime_data.csv")

ppd_df <- read.csv("../data/pop_density.csv")
d4_df <- read.csv("../data/data4.csv")
d4_df <- d4_df[, c("POSTCODE", "Suburb")]
ddf <- merge(ppd_df, d4_df, by="Suburb")
ddf <- distinct(ddf)
write.csv(ddf, file="../data/population_density.csv")

head(ddf)
head(ppd_df)
head(d4_df)


# filter data by age group
df <- read.csv("../data/data4.csv")
df_property <- read.csv("../data/property_data2.csv" )
df_property
df_property2 <- df_property %>%
  filter(
    year==2015
    ) %>%
  arrange(Suburb)
df_property2  

names(df)
df <- df[, c("POSTCODE", "Suburb", "AgeGroup", "AG_percent")]
df_child <- df %>%
  filter(
    AgeGroup=="Children",
    AG_percent >= 28.5
  ) %>%
  arrange(POSTCODE)
df_child <- distinct(df_child)
df_child #max29.4

df_adult <- df %>%
  filter(
    AgeGroup=="Adult",
    AG_percent >= 88.5
  ) %>%
  arrange(POSTCODE)
df_adult <- distinct(df_adult)
df_adult #max 99.9

df_elderly <- df %>%
  filter(
    AgeGroup=="Elderly",
    AG_percent >= 52.0
  ) %>%
  arrange(POSTCODE)
df_elderly <- distinct(df_elderly)
df_elderly #max 52.4

df
df_agegroup <- df %>%
  filter(
    AgeGroup=="Children" && AG_percent >= 0,
    AgeGroup=="Adult" && AG_percent >= 0,
    AgeGroup=="Elderly" && AG_percent >= 0
  ) %>%
  arrange(POSTCODE)
df_agegroup <- rbind(df_child, df_adult, df_elderly)
df_agegroup

dfc <- merge(df_agegroup, df_property2, by = "Suburb")
dfc


# slice data frame group by a string column value
df <- read.csv("../data/property_data2.csv")
df1 <- subset(df, df$Suburb==toupper("vermont"))
df2 <- subset(df, df$Suburb==toupper("vermont south"))
df3 <- rbind(df1, df2)
df3 # data frames used to manulipate
df4 <- df3 %>%
  distinct(Suburb) %>%
  select(Suburb)
df5 <- df3 %>% 
  filter(Suburb %in% df4[1,])
p <- plot_ly(df5) %>%
  add_lines(x = ~year, y = ~price, yaxis="y2",mode='lines+markers') 
df5 <- df3 %>% 
  filter(Suburb %in% df4[2,])
df5

for(i in nrow(df4)){
  df5 <- df3 %>% 
    filter(Suburb %in% df4[i,])
    p <- plot_ly(df5) %>%
      add_trace(x = ~year, y = ~price, yaxis="y2",mode='lines+markers')
}

nrow(df4)
nrow(df3)
df4 <- df3 %>% arrange(Suburb, year)
# df4 <- df3 %>% group_by(Suburb)
df4
slice(df4, 1:(nrow(df4)/2))

library(plotly)

dens <- with(diamonds, tapply(price, INDEX = cut, density))
df <- data.frame(
  x = unlist(lapply(dens, "[[", "x")),
  y = unlist(lapply(dens, "[[", "y")),
  cut = rep(names(dens), each = length(dens[[1]]$x))
)

p <- plot_ly(df, x = ~x, y = ~y, color = ~cut) %>%
  add_lines()
p <- plot_ly(df3, x = ~year, y = ~price, yaxis="y2", color = ~Suburb) %>%
  add_lines()
p

# plotly crime data
df_crime <- read.csv("../data/crime_data.csv")
crimedf <- df_crime %>%
        group_by(POSTCODE) %>%
        filter(POSTCODE=="3148")

class(df3$price)
names(df3)
ppy <- as.numeric(as.character(df3$price))
p <- plot_ly() %>%
  add_lines(data=df3, x = ~year, y = ~as.numeric(as.character(df3$price)), color=~Suburb) %>%
  add_lines(data=crimedf, x=~year, y= ~as.numeric(gsub('\\,', '', as.character(crimedf$occurence))), yaxis ="y2", name="crime") %>%
  layout(
    yaxis=list(
      title="property price"
    ),
    yaxis2 = list(
      side="right",
      overlaying="y",
      title="crime occurence"
    )
  )
p

p <- plot_ly(crimedf, x = ~year, y = ~as.integer(crimedf$occurence), mode="lines", rangemode="tozero", type="scatter" ,name="crime") %>%
  layout(
    yaxis=list(
      rangemode="tozero"
    )
  )
p
