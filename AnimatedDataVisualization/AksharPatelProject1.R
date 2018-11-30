library(tidyr)
library(magick)
library(ggplot2)
library(xlsx)
library(readr)
setwd("C:/Users/Akshar/Desktop/Stat 128/DataVizProj")
countries <- read_csv("country_population.csv")
life.exp <- read_csv("life_expectancy.csv")
fertility <- read_csv("fertility_rate.csv")
dfs <- list(countries = countries, life.exp = life.exp, fertility = fertility)
key <- "Year"
values <- c("Population", "LifeExp", "FertRate")
wideToLong <- function(data, value, key){
  data <- gather(data, "Key", "Value", - `Country Name`, - `Country Code`, - `Indicator Name`, - `Indicator Code`)
  fix <- colnames(data)
  colnames(data)[fix == "Key"] <- key
  colnames(data)[fix == "Value"] <- value
  return(data)
}
longed.data <- mapply(wideToLong, data = dfs, value = values, MoreArgs = list(key = key), SIMPLIFY = FALSE)
countries <- longed.data$countries
life.exp <- longed.data$life.exp
fertility <- longed.data$fertility
df <- cbind(countries, life.exp, fertility)
df <- df[, c("Country Name", "Year", "Population","LifeExp", "FertRate")]
#Source For Continents: World Bank - https://data.worldbank.org/country 
continents <- read.xlsx("continents.xlsx", 1)
conts <- rep(continents$Continents.Region, times = 57)
df$Continents.Region <- conts
df$Year <- factor(df$Year)
df <- df[complete.cases(df), ]
df <- subset(df, Continents.Region != "Multiple")
df$Continents.Region <- droplevels(df$Continents.Region)
graph <- function(year, df){
  subdata <- subset(df, Year == year)
  plot <- ggplot(data = subdata, aes(x = LifeExp, y = FertRate, group = Continents.Region, size = Population, color = Continents.Region)) + geom_point()
  plot <- plot + theme_bw() + ggtitle(paste("Life Expectancy Vs Fertility Rate", year)) + ylim(1, 10) + xlim(10, 80)
  return(plot)
}

years <- levels(df$Year)

for(year in years){
  file <- paste(year, "png", sep = ".")
  png(file)
  plot <- graph(year, df)
  print(plot)
  dev.off()
}
pic.bundle <- NULL
for(year in years){
  file <- paste(year, "png", sep = ".")
  pic <- image_read(file)
  pic <- image_resize(pic, '110%')
  if(is.null(pic.bundle)){
    pic.bundle <- c(pic)
  }else{
    pic.bundle <- c(pic.bundle, pic)
  }
}

animation <- image_animate(pic.bundle, fps = 1.25)
print(animation)



