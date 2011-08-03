tracer.barres.moyplusieurs <-
function() {
  variable<-Env$dataset[,tclvalue(Env$l.var$variable)]
  facteur1<-Env$dataset[,tclvalue(Env$l.var$facteur1)]
  facteur2<-Env$dataset[,tclvalue(Env$l.var$facteur2)]
  valeurs<-tapply(variable,list(facteur2,facteur1),function(x) mean(x,na.rm=TRUE))
  erreurs<-graphe.erreurs.calculer(variable=variable,facteur1=facteur1,facteur2=facteur2)
  limites<-tracer.barres.limites(valeurs=valeurs,erreur.inf=erreurs$erreur.inf,erreur.sup=erreurs$erreur.sup)
  Env$l.var$add.hauteurs<-valeurs+erreurs$erreur.sup
  y.inf<-limites$yinf
  y.sup<-limites$ysup
  Env$l.var$add.abscisses<-barplot(valeurs,axes=FALSE,ann=FALSE,col=Env$l.var$couleur1B,log=graphe.log(),
    border=Env$l.var$col.borduresB,ylim=c(y.inf,y.sup),names.arg=Env$l.var$noms1,
    beside=ifelse(tclvalue(Env$l.var$stack)==1,FALSE,TRUE))
  Env$l.var$add.matrice<-matrix(numeric(length(Env$l.var$add.abscisses)^2),nrow=length(Env$l.var$add.abscisses),
    dimnames=list(1:length(Env$l.var$add.abscisses),1:length(Env$l.var$add.abscisses)))
  for (i in 1:length(Env$l.var$add.abscisses)) {
    for (j in 1:length(Env$l.var$add.abscisses)) {
	Env$l.var$add.matrice[j,i]<-max(Env$l.var$add.hauteurs[i:j])
    }
  }
  if (graphe.log()=="" & any(Env$l.var$hachuresB!=1)) {
    hachures<-graphe.hachures(num=Env$l.var$hachuresB)
    barplot(valeurs,axes=FALSE,ann=FALSE,col=Env$l.var$col.borduresB,border=Env$l.var$col.borduresB,
	log=graphe.log(),ylim=c(y.inf,y.sup),density=hachures$densite,angle=hachures$angle,
	beside=ifelse(tclvalue(Env$l.var$stack)==1,FALSE,TRUE),names.arg=rep("",nlevels(facteur1)),add=TRUE)
  }
  if (tclvalue(Env$l.var$stack)==0 & nchar(tclvalue(Env$l.var$erreur))>0 & tclvalue(Env$l.var$erreur)!=Env$voc[95,1]) {
    graphe.erreurs.tracer(abscisses=Env$l.var$add.abscisses,valeurs=valeurs,erreur.inf=erreurs$erreur.inf,
	erreur.sup=erreurs$erreur.sup,alert=erreurs$alert,couleur=tclvalue(Env$l.var$couleur2A))
  }
  graphe.titre()
  graphe.axes(type="bar",ordonnee=limites$ordonnee)
  graphe.box()
  if (tclvalue(Env$l.var$legende)==1) {
    graphe.legende(type="bar")
  }
}

