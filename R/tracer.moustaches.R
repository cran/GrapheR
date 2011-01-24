tracer.moustaches <-
function(var,fact,orient,log.ax,yinf,ysup,col.box,col.bor,col.wsk,col.out,lty.wsk,out,pch.out,
  size.mous,noms,ic,x.leg,y.leg,size.leg,col.leg,col.ax,size.ax,titre,col.titre,size.titre,fen) {
  if (nchar(var)>0 & nchar(fact)>0) {
    log.axes=if (orient=="ver" & log.ax==1) {"y"} else if (orient=="hor" & log.ax==1) {"x"} else {""}
    if (is.numeric(dev.list()[1])==FALSE) {par(bg="white",mar=c(5,6,4,2))}
    xsup=nlevels(Env$datas.GrapheR[,fact])+0.5
    if (yinf=="Auto") {yinf=min(na.omit(Env$datas.GrapheR[,var]))} else {yinf=as.numeric(yinf)}
    if (ysup=="Auto") {ysup=max(na.omit(Env$datas.GrapheR[,var]))} else {ysup=as.numeric(ysup)}
    type=if (lty.wsk==Env$vocab[75,1]) {1} else if (lty.wsk==Env$vocab[76,1]) {2} else {3}
    boxplot(Env$datas.GrapheR[,var]~Env$datas.GrapheR[,fact],main="",horizontal=if (orient=="hor") {TRUE} else {FALSE},col=col.box,
	boxcol=col.bor,medcol=col.bor,whiskcol=col.wsk,staplecol=col.wsk,outcol=col.out,whisklty=type,
	outline=if (out==0) {FALSE} else {TRUE},outpch=if (pch.out=="vide") {1} else {16},
	range=size.mous,names=noms,notch=if (ic==1) {TRUE} else {FALSE},
	xlab=if (orient=="ver") {x.leg} else {y.leg},ylab=if (orient=="ver") {y.leg} else {x.leg},
	cex.lab=size.leg,col.lab=col.leg,col.axis=col.ax,cex.axis=size.ax,
	log=log.axes,xlim=c(0.5,xsup),ylim=c(yinf,ysup))
    title(main=titre,col.main=col.titre,cex.main=size.titre)
    if (Env$toolbar.GrapheR==1) {tkdestroy(Env$Toolbar)}
    tkgrab.release(fen)
    toolbar(type="moust",type.hist="",sequence="",log.ax=log.axes,ht="",abs="")
  } else {tkmessageBox(title=Env$vocab[3,1],message=Env$vocab[157,1],type="ok",icon="error")}
}

