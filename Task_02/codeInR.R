setwd("C:/Users/lexik/Evolution/Tasks/Task_02")
#
Data1 <- read.csv('http://jonsmitchell.com/data/beren.csv', stringsAsFactors=F)
Data2 <- read.csv('http://jonsmitchell.com/data/cyrus.csv', stringsAsFactors=F)
#
write.csv(Data1, 'rawdata.csv', quote=F)

