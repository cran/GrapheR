ajouter.droite2 <-
function(nb.niv,tra,drt,horiz,col.symb,ep.lig,vertic,coefb,coefa,x.log,y.log,reg,var1,fact,niv,var2) {
  for (i in 1:nb.niv) {
    trait=if (tra[i]==Env$vocab[75,1]) {1} else if (tra[i]==Env$vocab[76,1]) {2} else {3}
    if (drt[i]=="hor") {abline(h=horiz[i],col=col.symb[i],lty=trait,lwd=ep.lig[i])}
    if (drt[i]=="ver") {abline(v=vertic[i],col=col.symb[i],lty=trait,lwd=ep.lig[i])}
    if (drt[i]=="affine") {abline(coefb[i],coefa[i],col=col.symb[i],lty=trait,lwd=ep.lig[i],untf=if (x.log==1 | y.log==1) {TRUE} else {FALSE})}
    if (drt[i]=="reg" & reg[i]=="mc") {
	x=if (x.log==1) {log10(Env$datas.GrapheR[,var1][Env$datas.GrapheR[,fact]==levels(Env$datas.GrapheR[,fact])[as.numeric(strsplit(niv,split=" ")[[1]][i])+1]])} else {Env$datas.GrapheR[,var1][Env$datas.GrapheR[,fact]==levels(Env$datas.GrapheR[,fact])[as.numeric(strsplit(niv,split=" ")[[1]][i])+1]]}
	y=if (y.log==1) {log10(Env$datas.GrapheR[,var2][Env$datas.GrapheR[,fact]==levels(Env$datas.GrapheR[,fact])[as.numeric(strsplit(niv,split=" ")[[1]][i])+1]])} else {Env$datas.GrapheR[,var2][Env$datas.GrapheR[,fact]==levels(Env$datas.GrapheR[,fact])[as.numeric(strsplit(niv,split=" ")[[1]][i])+1]]}
	abline(lm(y~x),col=col.symb[i],lty=trait,lwd=ep.lig[i])
    }
    if (drt[i]=="reg" & reg[i]=="mr") {
	x=if (x.log==1) {log10(Env$datas.GrapheR[,var1][Env$datas.GrapheR[,fact]==levels(Env$datas.GrapheR[,fact])[as.numeric(strsplit(niv,split=" ")[[1]][i])+1]])} else {Env$datas.GrapheR[,var1][Env$datas.GrapheR[,fact]==levels(Env$datas.GrapheR[,fact])[as.numeric(strsplit(niv,split=" ")[[1]][i])+1]]}
	y=if (y.log==1) {log10(Env$datas.GrapheR[,var2][Env$datas.GrapheR[,fact]==levels(Env$datas.GrapheR[,fact])[as.numeric(strsplit(niv,split=" ")[[1]][i])+1]])} else {Env$datas.GrapheR[,var2][Env$datas.GrapheR[,fact]==levels(Env$datas.GrapheR[,fact])[as.numeric(strsplit(niv,split=" ")[[1]][i])+1]]}
	a=sqrt(var(y,na.rm=TRUE)/var(x,na.rm=TRUE))*sign(cov(x,y,use="complete.obs"))
	b=mean(y,na.rm=TRUE)-a*mean(x,na.rm=TRUE)
	abline(b,a,col=col.symb[i],lty=trait,lwd=ep.lig[i])
    }
    if (drt[i]=="quadra") {
	x=Env$datas.GrapheR[,var1][Env$datas.GrapheR[,fact]==levels(Env$datas.GrapheR[,fact])[as.numeric(strsplit(niv,split=" ")[[1]][i])+1]]
	y=Env$datas.GrapheR[,var2][Env$datas.GrapheR[,fact]==levels(Env$datas.GrapheR[,fact])[as.numeric(strsplit(niv,split=" ")[[1]][i])+1]]
	regress=lm(y~x+I(x^2))
	x2=seq(min(x,na.rm=TRUE),max(x,na.rm=TRUE),abs(max(x,na.rm=TRUE)-min(x,na.rm=TRUE))/1000)
	y2=predict(regress,list(x=x2))
	lines(x2,y2,col=col.symb[i],lty=trait,lwd=ep.lig[i])
    }
    if (drt[i]=="gam") {
	x=Env$datas.GrapheR[,var1][Env$datas.GrapheR[,fact]==levels(Env$datas.GrapheR[,fact])[as.numeric(strsplit(niv,split=" ")[[1]][i])+1]]
	y=Env$datas.GrapheR[,var2][Env$datas.GrapheR[,fact]==levels(Env$datas.GrapheR[,fact])[as.numeric(strsplit(niv,split=" ")[[1]][i])+1]]
	model=gam(y~s(x))
	x2=seq(min(x,na.rm=TRUE),max(x,na.rm=TRUE),abs(max(x,na.rm=TRUE)-min(x,na.rm=TRUE))/1000)
	y2=predict(model,list(x=x2))
	lines(x2,y2,col=col.symb[i],lty=trait,lwd=ep.lig[i])
    }
  }
}

