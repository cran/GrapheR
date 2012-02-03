tracer.barres.moyplusieurs <-
function() {
  variable<-Env$dataset[,tclvalue(Env$l.var$variable)]
  facteur1<-Env$dataset[,tclvalue(Env$l.var$facteur1)]
  facteur2<-Env$dataset[,tclvalue(Env$l.var$facteur2)]
  valeurs<-tapply(variable,list(facteur2,facteur1),function(x) mean(x,na.rm=TRUE))
  erreurs<-graphe.erreurs.calculer(variable=variable,facteur1=facteur1,facteur2=facteur2)
  limites<-tracer.barres.limites(valeurs=valeurs,erreur.inf=erreurs$erreur.inf,erreur.sup=erreurs$erreur.sup)
  Env$l.var$add.hauteurs<-valeurs+erreurs$erreur.sup
  Env$l.code$y.inf<-y.inf<-limites$yinf
  Env$l.code$y.sup<-y.sup<-limites$ysup
  if (tclvalue(Env$l.var$nobar)==0) {
    Env$l.var$add.abscisses<-barplot(valeurs,axes=FALSE,ann=FALSE,col=Env$l.var$couleur1B,log=graphe.log(),
	border=Env$l.var$col.borduresB,ylim=c(y.inf,y.sup),names.arg=Env$l.var$noms1,
	beside=ifelse(tclvalue(Env$l.var$stack)==1,FALSE,TRUE))
  } else {
    Env$l.var$add.abscisses<-barplot(valeurs,log=graphe.log(),ylim=c(y.inf,y.sup),beside=ifelse(tclvalue(Env$l.var$stack)==1,FALSE,TRUE),plot=FALSE)
    plot(if(tclvalue(Env$l.var$stack)==1){rep(Env$l.var$add.abscisses,each=length(Env$l.var$add.abscisses))}else{Env$l.var$add.abscisses},valeurs,cex=1.7,pch=16,
	col=if(tclvalue(Env$l.var$stack)==1){Env$l.var$couleur1B}else{rep(Env$l.var$couleur1B,ncol(Env$l.var$add.abscisses))},xlim=c(min(Env$l.var$add.abscisses)-0.5,
	max(Env$l.var$add.abscisses)+0.5),ylim=c(y.inf,y.sup),axes=FALSE,ann=FALSE,log=graphe.log())
  }
  Env$l.var$add.matrice<-matrix(numeric(length(Env$l.var$add.abscisses)^2),nrow=length(Env$l.var$add.abscisses),
    dimnames=list(1:length(Env$l.var$add.abscisses),1:length(Env$l.var$add.abscisses)))
  for (i in 1:length(Env$l.var$add.abscisses)) {
    for (j in 1:length(Env$l.var$add.abscisses)) {
	Env$l.var$add.matrice[j,i]<-max(Env$l.var$add.hauteurs[i:j])
    }
  }
  if (tclvalue(Env$l.var$nobar)==0 & graphe.log()=="" & any(Env$l.var$hachuresB!=1)) {
    hachures<-graphe.hachures(num=Env$l.var$hachuresB)
    barplot(valeurs,axes=FALSE,ann=FALSE,col=Env$l.var$col.borduresB,border=Env$l.var$col.borduresB,
	log=graphe.log(),ylim=c(y.inf,y.sup),density=hachures$densite,angle=hachures$angle,
	beside=ifelse(tclvalue(Env$l.var$stack)==1,FALSE,TRUE),names.arg=rep("",nlevels(facteur1)),add=TRUE)
  }
  if (tclvalue(Env$l.var$stack)==0 & nchar(tclvalue(Env$l.var$erreur))>0 & tclvalue(Env$l.var$erreur)!=Env$voc[95,1]) {
    graphe.erreurs.tracer(abscisses=Env$l.var$add.abscisses,valeurs=valeurs,erreur.inf=erreurs$erreur.inf,
	erreur.sup=erreurs$erreur.sup,couleur=tclvalue(Env$l.var$couleur2A))
  }
  graphe.titre()
  graphe.axes(type="bar",ordonnee=limites$ordonnee)
  graphe.box()
  if (tclvalue(Env$l.var$legende)==1) {
    graphe.legende(type="bar")
  }
}
