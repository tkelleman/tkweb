#Import data
countries<- read.csv("C:/Users/Kelle/OneDrive/Desktop/Week5/countries_total.csv")
income<-read.csv("C:/Users/Kelle/OneDrive/Desktop/Week5/income_per_person.csv")
lifeexp<-read.csv("C:/Users/Kelle/OneDrive/Desktop/Week5/life_expectancy_years.csv")
population<-read.csv("C:/Users/Kelle/OneDrive/Desktop/Week5/population_total.csv")

#Reshape Data
countriesEdit = subset(countries, select = c(country, year, income))