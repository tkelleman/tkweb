library(tidyverse)
library(knitr)
library(png)
library(RCurl)
library(magick)
library(grDevices)
library(jpeg)

#Import data set from Github and omit blank observations
urlfile="https://tkelleman.github.io/tkweb/week3/w03-penguins.csv"
pendata<-read.csv(url(urlfile))
#A
pendata_no_na <- na.omit(pendata)

#Filter Data based on HW Requirements 
#B
PenData.Filter<- filter(pendata_no_na, ((species=="Adelie" | species=="Gentoo") & (island=="Biscoe" | island=="Torgersen")))
#C
PenData.Filter<- filter(PenData.Filter, (body_mass_g <5000 & body_mass_g >3500))
#D
PenData.Filter<- PenData.Filter %>%
  mutate(BMI=body_mass_g/4000)
#E
drop<- c("X", "sex", "year", "body_mass_g")
PenData.Filter=PenData.Filter[,!(names(PenData.Filter) %in% drop)]

#Variables
billLength = PenData.Filter$bill_length_mm
flipperLength = PenData.Filter$flipper_length_mm
species = PenData.Filter$species
pointSize = PenData.Filter$BMI

#ID of species
adelie.id = which(species=="Adelie")
gentoo.id = which(species=="Gentoo")


## Create Scatter plot (No points - Type "n")
plot(
  billLength, 
  flipperLength, 
  main = "Bill Length vs Flipper Length \n of Adelie and Gentoo Penguins", 
  type = "n",   
  xlab="Bill Length (mm)",  
  ylab="Flipper Length (mm)",
)

## Plot Points
points(billLength[adelie.id], flipperLength[adelie.id], pch = 19, col = "#EE6C4D", cex = pointSize[adelie.id])
points(billLength[gentoo.id], flipperLength[gentoo.id], pch = 19, col = "#98C1D9", cex = pointSize[gentoo.id])

## Regression Line
adelie.model<-lm(flipperLength[adelie.id] ~ billLength[adelie.id])
abline(adelie.model, lwd =2, col="#EE6C4D")

gentoo.model<-lm(flipperLength[gentoo.id] ~ billLength[gentoo.id])
abline(gentoo.model, lwd =2, col="#98C1D9")


## Create Legend
legend("topleft", c("Adelie", "Gentoo"), 
       col=c("#EE6C4D", "#98C1D9"),
       pch=c(19, 19))
