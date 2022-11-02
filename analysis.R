library(foreign)
FILE.NAME <- "C:/Users/roman/Desktop/school/BS/bp/SAAVSspokoj.sav"

#load data
data <- read.spss(FILE.NAME, to.data.frame=TRUE, reencode="utf-8")

#column names
col.names <- colnames(data)

#programmes
programmes <- unique(data$odbor)

converter <- function(x){
  if(any(x == c("Rozhodne súhlasím","Takmer všetci učitelia", "Takmer všetky predmety" ))){
    return(4)
  }
  
  if(any(x == c("Skôr súhlasím", "Väčšina učiteľov", "Väčšina predmetov"))){
    return(3)
  }
  
  if(any(x == c("Skôr nesúhlasím", "Menšina učiteľov", "Menšina predmetov"))){
    return(2)
  }
  
  if(any(x == c("Rozhodne nesúhlasím", "Takmer žiadni učitelia", "Takmer žiadne predmety"))){
    return(1)
  }
  return(x)
}


#TAKING DIMENSIONS

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
#END OF DIMENSIONS

#conversion of data
new.data <- data.frame(apply(data, c(1,2), converter))
