tracer.camembert <-
function(var,nb.niv,prop,noms,sens,debut,autour,col.par,col.bor,dens.motif,ang.motif,col.motif,titre,col.titre,taille.titre,leg,pos.leg,leg.titre,fen) {
  if (nchar(var)>0) {
    if (is.numeric(dev.list()[1])==FALSE) {par(bg="white",mar=c(5,6,4,2))}
    if (is.factor(Env$datas.GrapheR[,var])==TRUE) {
	cam.fact(nb.niv=nb.niv,var=var,prop=prop,autour=autour,noms=noms,col.par=col.par,col.bor=col.bor,sens=sens,debut=debut,dens.motif=dens.motif,
	  ang.motif=ang.motif,col.motif=col.motif)
    } else {
	cam.num(var=var,prop=prop,autour=autour,noms=noms,col.par=col.par,col.bor=col.bor,sens=sens,debut=debut,dens.motif=dens.motif,
	  ang.motif=ang.motif,col.motif=col.motif)
    }
    title(main=titre,col.main=col.titre,cex.main=taille.titre)
    tracer.legende.1(pos.leg=pos.leg,leg=leg,leg.titre=leg.titre,labels=noms,couleurs=col.par)
    if (Env$toolbar.GrapheR==1) {tkdestroy(Env$Toolbar)}
    tkgrab.release(fen)
    toolbar(type="cam",type.hist="",sequence="",log.ax="",ht="",abs="")
  } else {tkmessageBox(title=Env$vocab[3,1],message=Env$vocab[6,1],icon="error",type="ok")}
}

