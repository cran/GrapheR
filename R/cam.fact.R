cam.fact <-
function(nb.niv,var,prop,autour,noms,col.par,col.bor,sens,debut,dens.motif,ang.motif,col.motif) {
  long=integer(nb.niv)
  for (i in 1:nb.niv) {
    long[i]=length(Env$datas.GrapheR[,var][Env$datas.GrapheR[,var]==levels(Env$datas.GrapheR[,var])[as.numeric(strsplit(prop,split=" ")[[1]][i])+1]])
  }
  pie(long,main="",labels=if (autour==1) {noms} else {NA},col=col.par,border=col.bor,clockwise=if (sens=="am") {TRUE} else {FALSE},
    init.angle=if (sens=="am") {debut*-1+90} else {debut+90})
  par(new=TRUE)
  pie(long,main="",labels=NA,border=col.bor,clockwise=if (sens=="am") {TRUE} else {FALSE},init.angle=if (sens=="am") {debut*-1+90} else {debut+90},
    density=dens.motif,angle=ang.motif,col=col.motif)
}

