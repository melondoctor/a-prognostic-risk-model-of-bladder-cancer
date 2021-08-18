#install.packages("survival")
#install.packages("survminer")
#install.packages("timeROC")


library(survival)
library(survminer)
library(timeROC)
setwd("C:\\Users\\melon doctor\\Desktop\\immune therapy\\log\\18.ROC")     


bioROC=function(inputFile=null, rocFile=null){
	predictTime=1
	rt=read.table(inputFile, header=T, sep="\t", check.names=F)
	ROC_rt=timeROC(T=rt$futime, delta=rt$fustat,
	               marker=rt$riskScore, cause=1,
	               times=c(predictTime), ROC=TRUE)
	pdf(file=rocFile, width=5, height=5)
	plot(ROC_rt, time=predictTime, col='red', title=FALSE, lwd=2)
	legend('bottomright', cex=1.3,
           paste0('AUC=',sprintf("%.03f",ROC_rt$AUC[2])),
	       col="white", lwd=1, bty = 'n')
	dev.off()
}

bioROC(inputFile="risk.train.txt", rocFile="ROC.train.pdf")
bioROC(inputFile="risk.test.txt", rocFile="ROC.test.pdf")
bioROC(inputFile="risk.all.txt", rocFile="ROC.all.pdf")