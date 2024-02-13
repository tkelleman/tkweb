library(jpeg)
library(png)
library(curl)

#Import data set from Github and omit blank observations
urlfile="https://tkelleman.github.io/tkweb/week3/w03-penguins.csv"
pendata<-read.csv(url(urlfile))
pendata_no_na <- na.omit(pendata)

#Import image
#penguin.img <-"https://tkelleman.github.io/tkweb/week3/penguin.jpg"
#my.penguin <- readJPEG(getURLContent(penguin.img))
#raster.penguin <- as.raster(my.penguin)

#Exploratory Analysis
head(pendata_no_na)
summary(pendata_no_na)

#Variables
billLength = pendata_no_na$bill_length_mm
flipperLength = pendata_no_na$flipper_length_mm
species = pendata_no_na$species
pointSize = (pendata_no_na$body_mass_g)/3000
  
#ID of species
adelie.id = which(species=="Adelie")
gentoo.id = which(species=="Gentoo")
chinstrap.id = which(species=="Chinstrap")


## Create Scatter plot (No points - Type "n")
plot(
  billLength, 
  flipperLength, 
  main = "Bill Length vs Flipper Length of Penguins", 
  type = "n",   
  xlab="Bill Length (mm)",  
  ylab="Flipper Length (mm)",
)

## Plot Points
points(billLength[adelie.id], flipperLength[adelie.id], pch = 19, col = "red", cex = pointSize[adelie.id])
points(billLength[gentoo.id], flipperLength[gentoo.id], pch = 19, col = "blue", cex = pointSize[gentoo.id])
points(billLength[chinstrap.id], flipperLength[chinstrap.id], pch = 19, col = "darkgreen", cex = pointSize[chinstrap.id])


## Create Legend
legend("topleft", c("Adelie", "Gentoo", "Chinstrap"), 
       col=c("red", "blue", "darkgreen"),
       pch=c(19, 19, 19))

## Write Text
text(52, 172, "**Point size is proportional to body mass in grams", col = "black", cex = .8)


#Create Reg Line
model<-lm(flipperLength ~ billLength)
abline(model, lwd =2, col="black")

#Display image
#rasterImage(raster.penguin,37,220,42,232)

