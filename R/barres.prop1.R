barres.prop1 <-
function(datas.GrapheR2,niv.prop,fact1,var,err.type,y.inf,log.ax,y.sup,x.legende,y.legende,log.axes,col.ax,taille.ax,col.legende,taille.legende,
  noms1,col.bar,col.bor,err.col,err.seg,dens.motif,ang.motif,col.motif,beside,pos.leg,leg,leg.titre,noms2) {
  niv.prop2=as.numeric(strsplit(niv.prop,split=" ")[[1]])
  proportions=NULL
  for (i in 1:nlevels(datas.GrapheR2[,fact1])) {
    for (j in 1:length(niv.prop2)) {
	proportions=c(proportions,length(datas.GrapheR2[,var][datas.GrapheR2[,var]==levels(datas.GrapheR2[,var])[niv.prop2[j]+1] & 
	datas.GrapheR2[,fact1]==levels(datas.GrapheR2[,fact1])[i]])/length(datas.GrapheR2[,var][datas.GrapheR2[,fact1]==levels(datas.GrapheR2[,fact1])[i]]))}
  }
  proportions.mat=matrix(proportions,nrow=length(niv.prop2),dimnames=list(levels(datas.GrapheR2[,var])[niv.prop2+1],levels(datas.GrapheR2[,fact1])))
  list.err=calc.err3(datas.GrapheR2=datas.GrapheR2,err.type=err.type,fact1=fact1,niv.prop2=niv.prop2,proportions.mat=proportions.mat,var=var)
  barres.inf.mat=list.err$inf
  barres.sup.mat=list.err$sup
  ht.max=proportions.mat+barres.sup.mat
  if (is.numeric(dev.list()[1])==FALSE) {par(bg="white",mar=c(5,6,4,2))}
  if (nchar(niv.prop)==1) {
    graphe=barres.prop2A(datas.GrapheR2=datas.GrapheR2,y.inf=y.inf,log.ax=log.ax,y.sup=y.sup,proportions.mat=proportions.mat,barres.sup.mat=barres.sup.mat,x.legende=x.legende,y.legende=y.legende,
	log.axes=log.axes,col.ax=col.ax,taille.ax=taille.ax,col.legende=col.legende,taille.legende=taille.legende,noms1=noms1,col.bar=col.bar,col.bor=col.bor,
	barres.inf.mat=barres.inf.mat,err.col=err.col,err.seg=err.seg,fact1=fact1,dens.motif=dens.motif,ang.motif=ang.motif,col.motif=col.motif)
  } else {
    graphe=barres.prop2B(datas.GrapheR2=datas.GrapheR2,y.inf=y.inf,log.ax=log.ax,y.sup=y.sup,beside=beside,proportions.mat=proportions.mat,barres.sup.mat=barres.sup.mat,x.legende=x.legende,y.legende=y.legende,
	log.axes=log.axes,col.ax=col.ax,taille.ax=taille.ax,col.legende=col.legende,taille.legende=taille.legende,noms1=noms1,col.bar=col.bar,col.bor=col.bor,fact1=fact1,
	dens.motif=dens.motif,ang.motif=ang.motif,col.motif=col.motif,pos.leg=pos.leg,leg=leg,leg.titre=leg.titre,noms2=noms2,barres.inf.mat=barres.inf.mat,
	err.col=err.col,err.seg=err.seg)
  }
  return(list(graphe=graphe,ht=ht.max))
}

