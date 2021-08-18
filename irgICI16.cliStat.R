trainFile="data.train.txt"    
testFile="data.test.txt"      
cliFile="clinical.txt"        
setwd("C:\\Users\\melon doctor\\Desktop\\immune therapy\\log\\16.cliStat")      

train=read.table(trainFile, header=T, sep="\t", check.names=F, row.names=1)
test=read.table(testFile, header=T, sep="\t", check.names=F, row.names=1)

cli=read.table(cliFile, header=T, sep="\t", check.names=F, row.names=1)

trainCli=cli[row.names(train),]
trainCli=cbind(trainCli, Type="Train")
testCli=cli[row.names(test),]
testCli=cbind(testCli, Type="Test")
rt=rbind(trainCli, testCli)

cliStatOut=data.frame()
for(i in 1:(ncol(rt)-1)){
	nameStat=colnames(rt)[i]
	tableStat=table(rt[,c(nameStat,"Type")])
	tableStatSum=cbind(Total=rowSums(tableStat), tableStat)
	tableStatRatio=prop.table(tableStatSum,2)
	tableStatRatio=round(tableStatRatio*100,2)
	tableStatPaste=paste(tableStatSum,"(",tableStatRatio,"%)",sep="")
	tableStatOut=matrix(tableStatPaste,ncol=3,dimnames=dimnames(tableStatSum))
	pStat=chisq.test(tableStat[row.names(tableStat)!="unknow",])
	pValueStat=round(pStat$p.value,4)
	pValueCol=c(pValueStat,rep(" ",(nrow(tableStatOut)-1)) )
	tableStatOut=cbind(Covariates=nameStat,Type=row.names(tableStatOut),tableStatOut,Pvalue=pValueCol)
	cliStatOut=rbind(cliStatOut,tableStatOut)
}
write.table(cliStatOut,file="cliStat.result.xls",sep="\t",quote=F,row.names=F)