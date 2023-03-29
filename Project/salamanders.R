setwd("C:/Users/lexik/evolution/tasks/project")
getwd()
#install.packages('readxl')
library("readxl")
df = read_xlsx("C:/Users/lexik/evolution/tasks/project/AllSpecimens.xlsx")
allSpecimens <- df
head(allSpecimens)
colnames(allSpecimens)
footMeans <- read.csv("FootLandmarks.SpeciesSummary.Csize.csv")
head(footMeans)
colnames(footMeans)
bodyMeasurements <- read.csv("LinMeas.SpeciesSummaries.csv")
head(bodyMeasurements)
colnames(bodyMeasurements)
microHabitats <- read.csv("Microhabitats.csv")
head(microHabitats)
colnames(microHabitats)
neoteny <- read.csv("neoteny.csv")
head(neoteny)
colnames(neoteny)
str(neoteny)

#install.packages('dplyr')
library('dplyr')
#install.packages('ggplot2')
library('ggplot2')
#install.packages("reshape2")
library("reshape2")

#alwaysNeotenic <- neoteny %>% group_by(genus, species) %>% reframe(which(neotenic=='2'))
#normalDevelopment <- neoteny %>% group_by(genus, species) %>% reframe(which(neotenic=='0'))
#directDevelopment <- neoteny %>% group_by(genus, species) %>% reframe(which(neotenic=='-1'))
#flexDevelop <- neoteny %>% group_by(genus, species) %>% reframe(which(neotenic=='1'))

neoSp <- apply(neoteny, 1, function(x) paste(x[1], x[2], sep="_"))
rownames(neoteny) <- neoSp
rownames(microHabitats) <- microHabitats[,1]
Keeps <- intersect(rownames(neoteny), rownames(microHabitats))

neo2 <- neoteny[Keeps,]
micro2 <- microHabitats[Keeps,]

ohabitat <- tapply(micro2$M7, micro2$M7, length)
oneo <- tapply(neo2$neotenic, neo2$neotenic, length)

outMat <- matrix(0, nrow=7, ncol=4)
rownames(outMat) <- unique(micro2$M7)
colnames(outMat) <- unique(neo2$neotenic)
for (count in 1:4){
  Neo <- unique(neo2$neotenic)[count]
  NeoRows <- which(neo2$neotenic == Neo)
  Habs <- tapply(micro2[NeoRows,"M7"], micro2[NeoRows,"M7"], length)
  outMat[names(Habs),count] <- Habs
}
outMat <- outMat[,c("-1","0","1","2")]
Cols <- c('#d73027','#fc8d59','#fee08b','#ffffbf','#d9ef8b','#91cf60','#1a9850')
names(Cols) <- unique(micro2$M7)

par(las=1, mar=c(4,5,1,1))
barplot(outMat, col=Cols, legend.text=T)


####################################################################################
####################################################################################

Counts <- sapply(1:nrow(neo2), function(x) tapply(neo2[x,3], micro2$M7[x], length))


Guide <- sapply(1:nrow(neo2), function(x) paste(micro2[x,"M7"], neo2[x,3], sep="_"))










########################################################################################


microHabitats2 <- microHabitats[, -c(2, 3, 5:8)]
nrow(microHabitats2)

norm_Develop <- paste(normalDevelopment$genus, normalDevelopment$species, sep="_")
flex_Develop <- paste(flexDevelop$genus, flexDevelop$species, sep="_")
direct_Develop <- paste(directDevelopment$genus, directDevelopment$species, sep="_")
always_Neo <- paste(alwaysNeotenic$genus, alwaysNeotenic$species, sep="_")

Habitats <- tapply(microHabitats2$M7, microHabitats2$M7, length)

normDevHabitat <- filter(microHabitats2, Species %in% norm_Develop)
flexDevHabitat <- filter(microHabitats2, Species %in% flex_Develop)
directDevHabitat <- filter(microHabitats2, Species %in% direct_Develop)
alwaysNeoHabitat <- filter(microHabitats2, Species %in% always_Neo)

str(alwaysNeoHabitat)

count1 <- normDevHabitat %>% count(M7)
count2 <- flexDevHabitat %>% count(M7)
count3 <- directDevHabitat %>% count(M7)
count4 <- alwaysNeoHabitat %>% count(M7)

colnames(count1) <- c('habitat','number')
colnames(count2) <- c('habitat','number')
colnames(count3) <- c('habitat','number')
colnames(count4) <- c('habitat','number')


count1[nrow(count1) +1,] <-list("A",0)
count2[nrow(count2) +1,] <-list("A",0)
count2[nrow(count2) +1,] <-list("C",0)
count2[nrow(count2) +1,] <-list("S",0)
count2[nrow(count2) +1,] <-list("A",0)
count2[nrow(count2) +1,] <-list("C",0)
count3[nrow(count3) +1,] <-list("SA",0)
count3[nrow(count3) +1,] <-list("W",0)
count4[nrow(count4) +1,] <-list("T",0)
count4[nrow(count4) +1,] <-list("A",0)
count4[nrow(count4) +1,] <-list("F",0)
count4[nrow(count4) +1,] <-list("S",0)

test <- data.frame(count1,count2,count3,count4)

lifestyle <- c(rep("-1",3), rep("0",3), rep("1",3), rep("2",3))
condition <- rep(c("Arboreal A)","Saxicolous S)","Terrestrial (T)","Aquatic (W)", "Fossorial (F)", "Semi-Aquatic (SA"), 4)
value <- abs(rnorm(12, 0, 15))

barplot(number ~ cbind(count1, countn2, count3, count4), data=count1)

barplot(cbind(number, number.1, number.2, number.3) ~ habitat, data=test, beside=TRUE)

barplot(number ~ habitat, data=count1)

barplot(number ~ count1$habitat + count2$habitat , data=test, beside=TRUE)

ggplot(test, aes(fill=condition, y=value, x=lifestyle)) +
  geom_bar(position="dodge",  stat="identity")

count <- matrix(NA, nrow=3, ncol=4)
count[1,] <- tapply(count1, count2, count4, length)

cd ~/Evolution/Tasks/Project
git add -A
git commit -m "Alexa Head Project"
git push -u origin master