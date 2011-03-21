tracer.barres <-
function(var,fact1,fact2,type,niv.prop,y.inf,y.sup,beside,x.legende,y.legende,col.ax,taille.ax,log.ax,col.legende,taille.legende,noms1,col.bar,col.bor,dens.motif,ang.motif,
  col.motif,titre,col.titre,taille.titre,leg,leg.titre,pos.leg,noms2,err.type,err.seg,err.col,fen) {
  if (nchar(var)>0 & nchar(fact1)>0) {
    log.axes=if (log.ax==1) {"y"} else {""}
    na=which(is.na(Env$datas.GrapheR[,var])==TRUE)
    datas.GrapheR2=if (length(na)!=0) {Env$datas.GrapheR[-na,]} else {Env$datas.GrapheR}
    if (type=="som") {
      if (nchar(fact2)>0 & fact2!=Env$vocab[48,1]) {
	  liste=barres.som1(datas.GrapheR2=datas.GrapheR2,var=var,fact2=fact2,fact1=fact1,y.inf=y.inf,beside=beside,log.ax=log.ax,y.sup=y.sup,x.legende=x.legende,y.legende=y.legende,log.axes=log.axes,
	    col.legende=col.legende,taille.legende=taille.legende,taille.ax=taille.ax,noms1=noms1,col.bar=col.bar,col.bor=col.bor,dens.motif=dens.motif,ang.motif=ang.motif,
	    col.motif=col.motif,pos.leg=pos.leg,leg=leg,leg.titre=leg.titre,noms2=noms2)
      } else {
	  liste=barres.som2(datas.GrapheR2=datas.GrapheR2,var=var,fact1=fact1,y.inf=y.inf,log.ax=log.ax,y.sup=y.sup,x.legende=x.legende,y.legende=y.legende,log.axes=log.axes,col.ax=col.ax,taille.ax=taille.ax,
	    col.legende=col.legende,taille.legende=taille.legende,noms1=noms1,col.bar=col.bar,col.bor=col.bor,dens.motif=dens.motif,ang.motif=ang.motif,col.motif=col.motif)
	}
    } else if (type=="moy") {
      if (nchar(fact2)>0 & fact2!=Env$vocab[48,1]) {
	  liste=barres.moy1(datas.GrapheR2=datas.GrapheR2,var=var,fact2=fact2,fact1=fact1,err.type=err.type,y.inf=y.inf,beside=beside,log.ax=log.ax,y.sup=y.sup,x.legende=x.legende,y.legende=y.legende,log.axes=log.axes,
	    col.ax=col.ax,taille.ax=taille.ax,col.legende=col.legende,taille.legende=taille.legende,noms1=noms1,col.bar=col.bar,col.bor=col.bor,dens.motif=dens.motif,
	    ang.motif=ang.motif,col.motif=col.motif,pos.leg=pos.leg,leg=leg,leg.titre=leg.titre,noms2=noms2,err.col=err.col,err.seg=err.seg)
	} else {
	  liste=barres.moy2(datas.GrapheR2=datas.GrapheR2,var=var,fact1=fact1,err.type=err.type,y.inf=y.inf,y.sup=y.sup,beside=beside,log.ax=log.ax,x.legende=x.legende,y.legende=y.legende,log.axes=log.axes,col.ax=col.ax,taille.ax=taille.ax,
	    col.legende=col.legende,taille.legende=taille.legende,noms1=noms1,col.bar=col.bar,col.bor=col.bor,dens.motif=dens.motif,ang.motif=ang.motif,col.motif=col.motif,
	    err.col=err.col,err.seg=err.seg)
	}
    } else if (type=="prop") {
	liste=barres.prop1(datas.GrapheR2=datas.GrapheR2,niv.prop=niv.prop,fact1=fact1,var=var,err.type=err.type,y.inf=y.inf,log.ax=log.ax,y.sup=y.sup,x.legende=x.legende,y.legende=y.legende,
	  log.axes=log.axes,col.ax=col.ax,taille.ax=taille.ax,col.legende=col.legende,taille.legende=taille.legende,noms1=noms1,col.bar=col.bar,col.bor=col.bor,err.col=err.col,
	  err.seg=err.seg,dens.motif=dens.motif,ang.motif=ang.motif,col.motif=col.motif,beside=beside,pos.leg=pos.leg,leg=leg,leg.titre=leg.titre,noms2=noms2)
    }
    title(main=titre,col.main=col.titre,cex.main=taille.titre)
    if (Env$toolbar.GrapheR==1) {tkdestroy(Env$Toolbar)}
    tkgrab.release(fen)
    toolbar(type="barres",type.hist="",sequence="",log.ax=log.axes,ht=liste$ht,abs=liste$graphe)
  } else {tkmessageBox(title=Env$vocab[3,1],message=Env$vocab[235,1],icon="error",type="ok")}
}

