tracer.cam <-
function() {
  niveaux<-as.numeric(strsplit(tclvalue(Env$l.var$parts.niveaux),split=" ")[[1]])+1
  prevariable1<-factor(Env$dataset[,tclvalue(Env$l.var$variable)])
  prevariable2<-prevariable1[prevariable1%in%levels(prevariable1)[niveaux]]
  variable<-summary(prevariable2)
  pie(variable,col=Env$l.var$couleur1B,border=tclvalue(Env$l.var$col.borduresA),
    labels=if (tclvalue(Env$l.var$cam.lien)==1) {Env$l.var$nomsparts} else {NA},
    clockwise=ifelse(tclvalue(Env$l.var$cam.orient)==Env$voc[120,1],TRUE,FALSE),
    init.angle=ifelse(tclvalue(Env$l.var$cam.orient)==Env$voc[120,1],as.numeric(tclvalue(Env$l.var$cam.start))+90,
    -1*as.numeric(tclvalue(Env$l.var$cam.start))+90))
  if (any(Env$l.var$hachuresB!=1)) {
    hachures<-graphe.hachures(num=Env$l.var$hachuresB)
    par(new=TRUE)
    pie(variable,col=tclvalue(Env$l.var$col.borduresA),labels=NA,density=hachures$densite,angle=hachures$angle,
	clockwise=ifelse(tclvalue(Env$l.var$cam.orient)==Env$voc[120,1],TRUE,FALSE),
	init.angle=ifelse(tclvalue(Env$l.var$cam.orient)==Env$voc[120,1],as.numeric(tclvalue(Env$l.var$cam.start))+90,
	-1*as.numeric(tclvalue(Env$l.var$cam.start))+90))
  }
  graphe.titre()
  if (tclvalue(Env$l.var$cam.lien)==0 & tclvalue(Env$l.var$legende)==1) {
    graphe.legende(type="cam")
  }
}

