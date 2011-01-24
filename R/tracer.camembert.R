tracer.camembert <-
function(var,nb.niv,prop,noms,sens,debut,autour,col.par,col.bor,dens.motif,ang.motif,col.motif,titre,col.titre,taille.titre,leg,pos.leg,leg.titre,fen) {
  if (nchar(var)>0) {
    position.legende=if (pos.leg==Env$vocab[126,1]) {"topleft"} else if (pos.leg==Env$vocab[127,1]) {"top"} else if (pos.leg==Env$vocab[128,1]) {"topright"} else
	if (pos.leg==Env$vocab[129,1]) {"left"} else if (pos.leg==Env$vocab[130,1]) {"center"} else if (pos.leg==Env$vocab[131,1]) {"right"} else
	if (pos.leg==Env$vocab[132,1]) {"bottomleft"} else if (pos.leg==Env$vocab[133,1]) {"bottom"} else if (pos.leg==Env$vocab[134,1]) {"bottomright"}
    if (is.numeric(dev.list()[1])==FALSE) {par(bg="white",mar=c(5,6,4,2))}
    if (is.factor(Env$datas.GrapheR[,var])==TRUE) {
	long=NULL
	for (i in 1:nb.niv) {long=c(long,length(Env$datas.GrapheR[,var][Env$datas.GrapheR[,var]==levels(Env$datas.GrapheR[,var])[as.numeric(strsplit(prop,split=" ")[[1]][i])+1]]))}
	pie(long,main="",labels=if (autour==1) {noms} else {NA},col=col.par,border=col.bor,clockwise=if (sens=="am") {TRUE} else {FALSE},
	  init.angle=if (sens=="am") {debut*-1+90} else {debut+90})
	par(new=TRUE)
	pie(long,main="",labels=NA,border=col.bor,clockwise=if (sens=="am") {TRUE} else {FALSE},init.angle=if (sens=="am") 
	  {debut*-1+90} else {debut+90},density=dens.motif,angle=ang.motif,col=col.motif)
	title(main=titre,col.main=col.titre,cex.main=taille.titre)
	if (leg==1) {
	  if (nchar(leg.titre)==0) {legend(position.legende,legend=noms,fill=col.par)} else {
	    legend(position.legende,legend=noms,fill=col.par,title=leg.titre)}
	}
    } else {
	pie(na.omit(Env$datas.GrapheR[,var][as.numeric(strsplit(prop,split=" ")[[1]])+1]),main="",labels=if (autour==1) {noms} else {NA},
	  col=col.par,border=col.bor,clockwise=if (sens=="am") {TRUE} else {FALSE},
	  init.angle=if (sens=="am") {debut*-1+90} else {debut+90})
	par(new=TRUE)
	pie(na.omit(Env$datas.GrapheR[,var][as.numeric(strsplit(prop,split=" ")[[1]])+1]),main="",labels=NA,border=col.bor,
	  clockwise=if (sens=="am") {TRUE} else {FALSE},init.angle=if (sens=="am") {debut*-1+90} else {debut+90},density=dens.motif,angle=ang.motif,col=col.motif)
	title(main=titre,col.main=col.titre,cex.main=taille.titre)
	if (leg==1) {
	  if (nchar(leg.titre)==0) {legend(position.legende,legend=noms,fill=col.par)} else {
	    legend(position.legende,legend=noms,fill=col.par,title=leg.titre)}
	}
    }
    if (Env$toolbar.GrapheR==1) {tkdestroy(Env$Toolbar)}
    tkgrab.release(fen)
    toolbar(type="cam",type.hist="",sequence="",log.ax="",ht="",abs="")
  } else {tkmessageBox(title=Env$vocab[3,1],message=Env$vocab[6,1],icon="error",type="ok")}
}

