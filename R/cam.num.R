cam.num <-
function(var,prop,autour,noms,col.par,col.bor,sens,debut,dens.motif,ang.motif,col.motif) {
  pie(na.omit(Env$datas.GrapheR[,var][as.numeric(strsplit(prop,split=" ")[[1]])+1]),main="",labels=if (autour==1) {noms} else {NA},
    col=col.par,border=col.bor,clockwise=if (sens=="am") {TRUE} else {FALSE},init.angle=if (sens=="am") {debut*-1+90} else {debut+90})
  par(new=TRUE)
    pie(na.omit(Env$datas.GrapheR[,var][as.numeric(strsplit(prop,split=" ")[[1]])+1]),main="",labels=NA,border=col.bor,
    clockwise=if (sens=="am") {TRUE} else {FALSE},init.angle=if (sens=="am") {debut*-1+90} else {debut+90},density=dens.motif,angle=ang.motif,col=col.motif)
}

