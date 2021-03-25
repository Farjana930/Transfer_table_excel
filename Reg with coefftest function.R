# This is an function named 'regcoffn'
# After running a regression and coeftest in R
# you can use this function to transfer the results
# to an excel file in standard regression table format
#
#How to use this function:
#
# Run a regression and a coeftest like the following:
# regression = plm(y~x,data = dataset,index = c("ID","time"), model = "within", effect = "individual")
# regcoftest = coeftest(regression, vcovHC(regression,type ="HC1",cluster="group"))
# Use coeftest accoring to your choice
#
#Then use the regcoffn function. Here dataset is the data used in regression and regression is the regression name.
#regcoftest is the coeftest name
#
#regcoffn(dataset,regression,regcoftest)


#Declaimer
#I have only tried this function with plm. But it can work with other functions as well.

regcoffn<- function(dataset,regression,regcoftest){
  z<- dataset
  x<- regression
  y<- regcoftest
  library(tidyverse)
  library(openxlsx)
  ####summary of regression
  res<- summary(x)
  # name of rows with gap for SE
  t1<- as.data.frame(rownames(res$coefficients))
  na.df <- data.frame(var = NA)
  col1 <- do.call(rbind, apply(t1, 1, function(x) {rbind(x, na.df)}))

  # Estimates
  #Beta
  k<- y[,4]
  star<- ifelse(k<=0.001,"***",ifelse(k<=0.01,"**",ifelse(k<=0.05,"*","")))
  coeff<- c(format(round(res$coefficients[,1],3),nsmall = 3))
  coeff1<- paste0(coeff,star)

  #SE
  SE<- c(paste0("(",format(round(y[,2],3),nsmall =3),")"))

  # Result row
  id<- seq(1:2)
  c<- as.data.frame(rbind(coeff1,SE))
  c<- cbind(id,c)
  col2<- c%>% gather(ID,Coeff,-id)%>%select(Coeff)

  # main result table
  restab1<- cbind(col1,col2)
  colnames(restab1)<- c("Var_name","Beta")


  # below part of the table
  r1<- as.data.frame(c("F-state(1st stage)","p-value","R^2","Adj_R^2","Observation"))
  f<- res$fstatistic
  f1<- f[2]
  f2<- f[4]
  rsqur<- res$r.squared
  obs<- nrow(z)

  r2<- round(t(as.data.frame(c(f1,f2,rsqur,obs))),3)
  restab2<- cbind(r1,r2)
  colnames(restab2)<- c("Var_name","Beta")

  result<- rbind(restab1,restab2)
  result
  write.xlsx(result,"result.xlsx") #Can change the excel file name here
}


