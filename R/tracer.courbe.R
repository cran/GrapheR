tracer.courbe <-
function(var1,var2,niv1,fact,nb.niv,niv2,type,encadre,titre,col.titre,taille.titre,taille.ax,col.ax,x.legende,y.legende,taille.legende,col.legende,x.inf,x.sup,y.inf,y.sup,x.log,y.log,
  symbol,taille.symb,col.symb,lig,tra,ep.lig,err.type,err.seg,leg,pos.leg,leg.titre,noms,fen) {
  if (nchar(var1)>0 & nchar(var2)>0) {
    na.a=which(is.na(Env$datas.GrapheR[,var1])==TRUE)
    na.b=which(is.na(Env$datas.GrapheR[,var2])==TRUE)
    datas.GrapheR2=if (length(na.a)!=0) {Env$datas.GrapheR[-na.a,]} else {Env$datas.GrapheR}
    datas.GrapheR2=if (length(na.b)!=0) {datas.GrapheR2[-na.b,]} else {datas.GrapheR2}
    nivx=as.numeric(strsplit(niv2,split=" ")[[1]])+1
    log.axes=if (x.log==1) {
      if (y.log==1) {"xy"} else {"x"}
    } else {
      if (y.log==1) {"y"} else {""}
    }
    if (type=="som") {
	if (nchar(fact)==0 | fact==Env$vocab[48,1]) {
	  courbes.som1(datas.GrapheR2=datas.GrapheR2,var2=var2,var1=var1,x.inf=x.inf,x.sup=x.sup,y.inf=y.inf,y.sup=y.sup,lig=lig,tra=tra,x.legende=x.legende,y.legende=y.legende,log.axes=log.axes,taille.legende=taille.legende,
	    col.legende=col.legende,col.symb=col.symb,symbol=symbol,taille.symb=taille.symb,ep.lig=ep.lig)
	} else {
	  if (nb.niv==1) {
	    courbes.som2(datas.GrapheR2=datas.GrapheR2,var1=var1,fact=fact,nivx=nivx,var2=var2,x.inf=x.inf,x.sup=x.sup,y.inf=y.inf,y.sup=y.sup,lig=lig,tra=tra,x.legende=x.legende,y.legende=y.legende,log.axes=log.axes,
		taille.legende=taille.legende,col.legende=col.legende,col.symb=col.symb,symbol=symbol,taille.symb=taille.symb,ep.lig=ep.lig,nb.niv=nb.niv,pos.leg=pos.leg,leg.titre=leg.titre,noms=noms,leg=leg)
	  } else {
	    courbes.som3(datas.GrapheR2=datas.GrapheR2,nb.niv=nb.niv,var1=var1,fact=fact,nivx=nivx,var2=var2,lig=lig,tra=tra,x.inf=x.inf,x.sup=x.sup,y.inf=y.inf,y.sup=y.sup,x.legende=x.legende,y.legende=y.legende,log.axes=log.axes,
		taille.legende=taille.legende,col.legende=col.legende,col.symb=col.symb,symbol=symbol,taille.symb=taille.symb,ep.lig=ep.lig,pos.leg=pos.leg,leg.titre=leg.titre,noms=noms,leg=leg)
	  }
	}
    } else if (type=="moy") {
	if (nchar(fact)==0 | fact==Env$vocab[48,1]) {
	  courbes.moy1(datas.GrapheR2=datas.GrapheR2,var2=var2,var1=var1,err.type=err.type,x.inf=x.inf,x.sup=x.sup,y.inf=y.inf,y.sup=y.sup,lig=lig,tra=tra,x.legende=x.legende,y.legende=y.legende,log.axes=log.axes,
	    taille.legende=taille.legende,col.legende=col.legende,col.symb=col.symb,symbol=symbol,taille.symb=taille.symb,ep.lig=ep.lig,err.seg=err.seg)
	} else {
	  if (nb.niv==1) {
	    courbes.moy2(datas.GrapheR2=datas.GrapheR2,var1=var1,fact=fact,nivx=nivx,var2=var2,err.type=err.type,x.inf=x.inf,x.sup=x.sup,y.inf=y.inf,y.sup=y.sup,lig=lig,tra=tra,x.legende=x.legende,y.legende=y.legende,
		log.axes=log.axes,taille.legende=taille.legende,col.legend=col.legende,col.symb=col.symb,symbol=symbol,taille.symb=taille.symb,ep.lig=ep.lig,err.seg=err.seg,nb.niv=nb.niv,pos.leg=pos.leg,
		leg.titre=leg.titre,noms=noms,leg=leg)
	  } else {
	    courbes.moy3(datas.GrapheR2=datas.GrapheR2,nb.niv=nb.niv,var1=var1,fact=fact,nivx=nivx,var2=var2,err.type=err.type,lig=lig,tra=tra,x.inf=x.inf,x.sup=x.sup,y.inf=y.inf,y.sup=y.sup,x.legende=x.legende,y.legende=y.legende,
		log.axes=log.axes,taille.legende=taille.legende,col.legende=col.legende,col.symb=col.symb,symbol=symbol,taille.symb=taille.symb,ep.lig=ep.lig,err.seg=err.seg,pos.leg=pos.leg,leg.titre=leg.titre,noms=noms,leg=leg)
	  }
	}
    } else if (type=="prop"){
	if (nchar(fact)==0 | fact==Env$vocab[48,1]) {
	  courbes.prop1(datas.GrapheR2=datas.GrapheR2,var1=var1,var2=var2,niv1=niv1,err.type=err.type,x.inf=x.inf,x.sup=x.sup,y.inf=y.inf,y.sup=y.sup,lig=lig,tra=tra,x.legende=x.legende,y.legende=y.legende,log.axes=log.axes,
	    taille.legende=taille.legende,col.legende=col.legende,col.symb=col.symb,symbol=symbol,taille.symb=taille.symb,ep.lig=ep.lig,err.seg=err.seg)
	} else {
	  if (nb.niv==1) {
	    courbes.prop2(datas.GrapheR2=datas.GrapheR2,var1=var1,var2=var2,fact=fact,nivx=nivx,niv1=niv1,err.type=err.type,x.inf=x.inf,x.sup=x.sup,y.inf=y.inf,y.sup=y.sup,lig=lig,tra=tra,x.legende=x.legende,y.legende=y.legende,
		log.axes=log.axes,taille.legende=taille.legende,col.legende=col.legende,col.symb=col.symb,symbol=symbol,taille.symb=taille.symb,ep.lig=ep.lig,err.seg=err.seg,nb.niv=nb.niv,pos.leg=pos.leg,leg.titre=leg.titre,
		noms=noms,leg=leg)
	  } else {
	    courbes.prop3(datas.GrapheR2=datas.GrapheR2,nb.niv=nb.niv,var1=var1,fact=fact,nivx=nivx,var2=var2,niv1=niv1,err.type=err.type,lig=lig,tra=tra,x.inf=x.inf,x.sup=x.sup,y.inf=y.inf,y.sup=y.sup,log.axes=log.axes,
		x.legende=x.legende,y.legende=y.legende,taille.legende=taille.legende,col.legende=col.legende,col.symb=col.symb,symbol=symbol,taille.symb=taille.symb,ep.lig=ep.lig,err.seg=err.seg,pos.leg=pos.leg,
		leg.titre=leg.titre,noms=noms,leg=leg)
	  }
	}
    }
    title(main=titre,col.main=col.titre,cex.main=taille.titre)
    axis(1,col=col.ax,col.axis=col.ax,cex.axis=taille.ax)
    axis(2,col=col.ax,col.axis=col.ax,cex.axis=taille.ax)
    if (encadre==1) {box()}
    if (Env$toolbar.GrapheR==1) {tkdestroy(Env$Toolbar)}
    tkgrab.release(fen)
    toolbar(type="courbe",type.hist="",sequence="",log.ax=log.axes,ht="",abs="")
  } else {tkmessageBox(title=Env$vocab[3,1],message=Env$vocab[157,1],icon="error",type="ok")}
}

