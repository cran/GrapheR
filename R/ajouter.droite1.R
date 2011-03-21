ajouter.droite1 <-
function(tra,drt,horiz,col.symb,ep.lig,vertic,coefb,coefa,x.log,y.log,reg,var1,var2) {
  trait=if (tra==Env$vocab[75,1]) {1} else if (tra==Env$vocab[76,1]) {2} else {3}
  if (drt=="hor") {abline(h=horiz,col=col.symb,lty=trait,lwd=ep.lig)}
  if (drt=="ver") {abline(v=vertic,col=col.symb,lty=trait,lwd=ep.lig)}
  if (drt=="affine") {
    abline(coefb,coefa,col=col.symb,lty=trait,lwd=ep.lig,untf=if (x.log==1 | y.log==1) {TRUE} else {FALSE})
  }
  if (drt=="reg" & reg=="mc") {
    x=if (x.log==1) {log10(Env$datas.GrapheR[,var1])} else {Env$datas.GrapheR[,var1]}
    y=if (y.log==1) {log10(Env$datas.GrapheR[,var2])} else {Env$datas.GrapheR[,var2]}
    abline(lm(y~x),col=col.symb,lty=trait,lwd=ep.lig)
  }
  if (drt=="reg" & reg=="mr") {
    x=if (x.log==1) {log10(Env$datas.GrapheR[,var1])} else {Env$datas.GrapheR[,var1]}
    y=if (y.log==1) {log10(Env$datas.GrapheR[,var2])} else {Env$datas.GrapheR[,var2]}
    a=sqrt(var(y,na.rm=TRUE)/var(x,na.rm=TRUE))*sign(cov(x,y,use="complete.obs"))
    b=mean(y,na.rm=TRUE)-a*mean(x,na.rm=TRUE)
    abline(b,a,col=col.symb,lty=trait,lwd=ep.lig)
  }
  if (drt=="quadra") {
    x=Env$datas.GrapheR[,var1]
    y=Env$datas.GrapheR[,var2]
    regress=lm(y~x+I(x^2))
    x2=seq(min(x,na.rm=TRUE),max(x,na.rm=TRUE),abs(max(x,na.rm=TRUE)-min(x,na.rm=TRUE))/1000)
    y2=predict(regress,list(x=x2))
    lines(x2,y2,col=col.symb,lty=trait,lwd=ep.lig)
  }
  if (drt=="gam") {
    x=Env$datas.GrapheR[,var1]
    y=Env$datas.GrapheR[,var2]
    model=gam(y~s(x))
    x2=seq(min(x,na.rm=TRUE),max(x,na.rm=TRUE),abs(max(x,na.rm=TRUE)-min(x,na.rm=TRUE))/1000)
    y2=predict(model,list(x=x2))
    lines(x2,y2,col=col.symb,lty=trait,lwd=ep.lig)
  }
}

