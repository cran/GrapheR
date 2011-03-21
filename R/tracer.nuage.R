tracer.nuage <-
function(var1,var2,fact,nb.niv,niv,encadre,titre,col.titre,taille.titre,col.ax,taille.ax,col.legende,taille.legende,x.legende,y.legende,x.inf,x.sup,
  y.inf,y.sup,x.log,y.log,symbol,col.symb,taille.symb,drt,reg,horiz,vertic,coefa,coefb,tra,ep.lig,leg,pos.leg,noms,leg.titre,fen) {
  if (nchar(var1)>0 & nchar(var2)>0) {
    log.axes=if (x.log==1) {if (y.log==1) {"xy"} else {"x"}} else {if (y.log==1) {"y"} else {""}}
    if (nchar(fact)==0 | fact==Env$vocab[48,1]) {
	nuage1(x.inf=x.inf,var1=var1,x.sup=x.sup,y.inf=y.inf,var2=var2,y.sup=y.sup,x.legende=x.legende,y.legende=y.legende,log.axes=log.axes,taille.legende=taille.legende,
	  col.legende=col.legende,col.symb=col.symb,symbol=symbol,taille.symb=taille.symb,drt=drt,tra=tra,horiz=horiz,vertic=vertic,ep.lig=ep.lig,coefb=coefb,coefa=coefa,
	  x.log=x.log,y.log=y.log,reg=reg)
    } else {
	nuage2(nb.niv=nb.niv,var1=var1,fact=fact,niv=niv,var2=var2,x.inf=x.inf,x.sup=x.sup,y.inf=y.inf,y.sup=y.sup,x.legende=x.legende,y.legende=y.legende,log.axes=log.axes,
	  taille.legende=taille.legende,col.legende=col.legende,col.symb=col.symb,symbol=symbol,taille.symb=taille.symb,leg=leg,pos.leg=pos.leg,leg.titre=leg.titre,noms=noms,
	  drt=drt,tra=tra,horiz=horiz,vertic=vertic,ep.lig=ep.lig,coefb=coefb,coefa=coefa,x.log=x.log,y.log=y.log,reg=reg)
    }
    title(main=titre,col.main=col.titre,cex.main=taille.titre)
    axis(1,col=col.ax,col.axis=col.ax,cex.axis=taille.ax)
    axis(2,col=col.ax,col.axis=col.ax,cex.axis=taille.ax)
    if (encadre==1) {box()}
    if (Env$toolbar.GrapheR==1) {tkdestroy(Env$Toolbar)}
    tkgrab.release(fen)
    toolbar(type="nuage",type.hist="",sequence="",log.ax=log.axes,ht="",abs="")
  } else {tkmessageBox(title=Env$vocab[3,1],message=Env$vocab[157,1],icon="error",type="ok")}
}

