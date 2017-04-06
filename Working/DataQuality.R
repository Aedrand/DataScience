#Andrew Riggs
#Data quality report script for CS376B

#LOADING

#Load the necessary libraries.
library(corrplot)
library(aplpack)
library(modes)
library(googleVis)

#Load the data from an Rda file.
#All data modification should be done prior to running this script.
#Storing in seperate frame because I'm not entirely sure how R works and don't want to overwrite the original data
load('modgamedata.Rda')
load('modgamedata100.Rda')
load('modgamedata1000.Rda')
moddata <- gamedata
moddata100 <- gamedata100
moddata1000 <- gamedata1000
rm(gamedata)
rm(gamedata100)
rm(gamedata1000)

#SETUP

#Define a function to calculate Pearson's r for inclusion in a scatter plot matrix- from Lab4.R
panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.6, txt)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.4, txt2)
}

#REPORTING

#Print some of the data using googlevis, cannot do all as it will not render.
gvt <- gvisTable(moddata[1:30,1:40])
plot(gvt)

#Provides a summary of all non-tag attributes and some tag attributes. Currently too many tag attributes to display them all.-----------TODO
writeLines("Summary of first 100 attributes.")
print(summary(moddata[,1:100]))

#Provides a five number summary of the first 100 numeric attributes.
writeLines("Five number summary of first 100 numeric attributes.")
for (i in 3:106) {
  writeLines(paste("Five number summary of", colnames(moddata)[i]),sep = " ")
  print(fivenum(moddata[[i]]))
}

#Provides the IQR of the numeric non-tag values.
#Does not yet include price and score, as those are broken right now. ------------------------------------------------------------------TODO
writeLines("IQR of numeric non-tag values")
for (i in 3:16) 
{
  writeLines(paste("IQR of", colnames(moddata)[i]),sep = " ")
  print(IQR(moddata[[i]]))
}

corModdata <- cor(moddata[,3:15])
corrplot(corModdata, main = "Correlation Plot")

#Tests correlation between the numerical columns and average playtime. Still have to make price and score comparable.-------------------TODO
writeLines("Correlation data, comparing to average playtime.")
for(i in 3:17)
{
  writeLines(paste("Correlation between average playtime (in minutes) and",colnames(moddata)[i],sep = " "))
  print(cor(moddata[[i]], moddata$average_forever))
}

#Histogram of the average playtime (in minutes) attribute
png("avgHist.png", width=900, height=500)
hist(moddata$average_forever, main = "Average Playtime Forever", xlab = "Average Playtime (minutes)")

#Histogram of the average playtime (in minutes) attribute, above 100 minutes
png("avgHist100.png", width=900, height=500)
hist(moddata100$average_forever, main = "Average Playtime Forever", xlab = "Average Playtime (minutes)")
dev.off()

#Histogram of the average playtime (in minutes) attribute, above 1000 minutes
png("avgHist1000.png", width=900, height=500)
hist(moddata1000$average_forever, main = "Average Playtime Forever", xlab = "Average Playtime (minutes)")
dev.off()

#Histogram of the average playtime (in minutes) attribute, above 10000 minutes
png("avgHist10000.png", width=900, height=500)
hist(moddata$average_forever[moddata$average_forever > 10000], main = "Average Playtime Forever", xlab = "Average Playtime (minutes)")
dev.off()

#Scatter plot matrix for the dataset
png("plotmatrix.png", width=1920, height=1080)
plot(moddata[,3:17], main = "Scatter Plot Matrix for Steam Data")
dev.off()

#Scatter plot for individual tags
#Only some for now. Already get enough emails about my MCS quota being full.
for(i in 16:28)
{
  nam <- paste(colnames(moddata)[12], colnames(moddata)[i], sep = " vs ")
  png(paste(nam,".png"), width=900, height=500)
  plot(moddata[,12],moddata[,i], main = nam, xlab = "Average Playtime (minutes)", ylab = paste(colnames(moddata)[i],"(0 = no, 1 = yes)",sep = " "))
  dev.off()
}

#Scatter plot for individual tags over 100
#Only some for now. Already get enough emails about my MCS quota being full.
for(i in 16:28)
{
  nam <- paste(colnames(moddata100)[12], colnames(moddata100)[i], sep = " vs ")
  png(paste(nam,"100.png"), width=900, height=500)
  plot(moddata100[,12],moddata100[,i], main = paste(nam, "(over100)", sep = " "), xlab = "Average Playtime (minutes)", ylab = paste(colnames(moddata100)[i],"(0 = no, 1 = yes)",sep = " "))
  dev.off()
}

#Scatter plot for individual tags over 1000
#Only some for now. Already get enough emails about my MCS quota being full.
for(i in 16:28)
{
  nam <- paste(colnames(moddata1000)[12], colnames(moddata1000)[i], sep = " vs ")
  png(paste(nam,"1000.png"), width=900, height=500)
  plot(moddata1000[,12],moddata1000[,i], main = paste(nam, "(over1000)", sep = " "), xlab = "Average Playtime (minutes)", ylab = paste(colnames(moddata1000)[i],"(0 = no, 1 = yes)",sep = " "))
  dev.off()
}

#Prints the number of games tagged with each tag.
for(i in 6:ncol(moddata))
{
  writeLines(paste("Number of items tagged with",colnames(moddata)[i],sep = " "))
  print(nrow(moddata[moddata[,i] == 1,]))
}




