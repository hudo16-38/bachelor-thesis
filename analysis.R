library(foreign)
FILE.NAME <- "C:/Users/roman/Desktop/school/BS/bp/SAAVSspokoj.sav"

#load data
data <- read.spss(FILE.NAME, to.data.frame=TRUE, reencode="utf-8")

#column names
col.names <- colnames(data)


#flexibility
flex <- subset(data, select=7:11)

#pred
pred <- subset(data, select = 12:18)

#ucitVZD
ucitVZD <- subset(data, select=19:25)

#ucitPRSIST
ucitPRIST <- subset(data, select = 26:32)

#hodnot
hodnot <- subset(data, select = 33:37)

#zruc
zruc <- subset(data, select=38:43)

#podpor
podpor <- subset(data, select=44:48)

