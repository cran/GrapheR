tracer.barres.moyun <-
function() {
  variable<-Env$dataset[,tclvalue(Env$l.var$variable)]
  facteur<-Env$dataset[,tclvalue(Env$l.var$facteur1)]
  valeurs<-tapply(variable,facteur,function(x) mean(x,na.rm=TRUE))
  erreurs<-graphe.erreurs.calculer(variable=variable,facteur1=facteur)
  limites<-tracer.barres.limites(valeurs=valeurs,erreur.inf=erreurs$erreur.inf,erreur.sup=erreurs$erreur.sup)
  Env$l.var$add.hauteurs<-valeurs+erreurs$erreur.sup
  Env$l.code$y.inf<-y.inf<-limites$yinf
  Env$l.code$y.sup<-y.sup<-limites$ysup
  Env$l.var$add.abscisses<-barplot(valeurs,axes=FALSE,ann=FALSE,col=tclvalue(Env$l.var$couleur1A),log=graphe.log(),
    border=tclvalue(Env$l.var$col.borduresA),ylim=c(y.inf,y.sup),names.arg=Env$l.var$noms1)
  Env$l.var$add.matrice<-matrix(numeric(length(Env$l.var$add.abscisses)^2),nrow=length(Env$l.var$add.abscisses),
    dimnames=list(1:length(Env$l.var$add.abscisses),1:length(Env$l.var$add.abscisses)))
  for (i in 1:length(Env$l.var$add.abscisses)) {
    for (j in 1:length(Env$l.var$add.abscisses)) {
	Env$l.var$add.matrice[j,i]<-max(Env$l.var$add.hauteurs[i:j])
    }
  }
  if (graphe.log()=="" & tclvalue(Env$l.var$hachuresA)!="1") {
    hachures<-graphe.hachures(num=as.numeric(tclvalue(Env$l.var$hachuresA)))
    barplot(valeurs,axes=FALSE,ann=FALSE,col=tclvalue(Env$l.var$col.borduresA),border=tclvalue(Env$l.var$col.borduresA),
	log=graphe.log(),ylim=c(y.inf,y.sup),density=hachures$densite,angle=hachures$angle,names.arg="",add=TRUE)
  }
  if (nchar(tclvalue(Env$l.var$erreur))>0 & tclvalue(Env$l.var$erreur)!=Env$voc[95,1]) {
    graphe.erreurs.tracer(abscisses=Env$l.var$add.abscisses,valeurs=valeurs,erreur.inf=erreurs$erreur.inf,
	erreur.sup=erreurs$erreur.sup,couleur=tclvalue(Env$l.var$couleur2A))
  }
  graphe.titre()
  graphe.axes(type="bar",ordonnee=limites$ordonnee)
  graphe.box()
}

