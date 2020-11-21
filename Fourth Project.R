#Author; Maulid Bwabo
#Fourth Project
#Pathmox PLS
#Setting Working Directory
setwd("C:/Users/bwabo/OneDrive/Desktop/Review Paper T")
#Install Package
require(genpathmox)
require(pathmox)
library(genpathmox)
help(genpathmox )
require(plspm)
require(testthat)
#Governance Data Set
Governance1= read.csv("C:/Users/bwabo/OneDrive/Desktop/Public Money/Governace 3.csv")
Governance1
Governance1$Gender = as.factor(Governance1$Gender)
Governance1$Age = as.factor(Governance1$Age)
str(Governance1)
str(Governance.fib)
col(Governance.fib)
#Manifest variables
Gov.fib =Governance1[,1:41]
# Elucidate inner model matrix
Transparency =rep(0,4)
Accountability=rep(0,4)
Legal =c(1,1,0,0)
Value =c(1,1,1,0)
inner.Gov = rbind(Transparency,Accountability,Legal, Value)
# blocks of indicators (outer model)
outer.gov =list(1:10, 11:17, 27:34, 35:41)
modes.Gov= rep("A", 4)
pls.Gov=plspm(Gov.fib,inner.Gov, outer.gov,modes.Gov)
summary(pls.Gov)
# re-ordering those segmentation variables with ordinal scale
seg.Gov= Gov.fib[,1:2]
seg.Gov$Age = factor(seg.Gov$Age, ordered=T)
seg.Gov$Gender = factor(seg.Gov$Gender, ordered=T)

#Pathmox
# Pathmox Analysis
#Issue for noting
Gov.pathmox1=pls.pathmox(pls.Gov,inner.Gov,outer.gov,modes.Gov,SVAR=seg.Gov,signif=0.05,
                         deep=2,size=0.2,n.node=20)
fib.comp=pls.treemodel(Gov.pathmox1)
summary(Gov.pathmox1)
Gov.pathmox1

