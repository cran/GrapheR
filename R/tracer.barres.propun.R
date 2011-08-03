tracer.barres.propun <-
function() {
  niveau<-as.numeric(tclvalue(Env$l.var$prop.niveaux))+1
  variable<-Env$dataset[,tclvalue(Env$l.var$proportions)][Env$dataset[,tclvalue(Env$l.var$proportions)]==levels(Env$dataset[,tclvalue(Env$l.var$proportions)])[niveau]]
  facteur<-Env$dataset[,tclvalue(Env$l.var$facteurprop)][Env$dataset[,tclvalue(Env$l.var$proportions)]==levels(Env$dataset[,tclvalue(Env$l.var$proportions)])[niveau]]
  valeurs<-integer(nlevels(facteur))
  for (i in 1:nlevels(facteur)) {
    valeurs[i]<-length(variable[facteur==levels(facteur)[i]])/length(na.omit(variable))
  }
  erreurs<-graphe.erreurs.calculer(variable=variable,facteur1=facteur,valeurs=valeurs)
  limites<-tracer.barres.limites(valeurs=valeurs,erreur.inf=erreurs$erreur.inf,erreur.sup=erreurs$erreur.sup)
  Env$l.var$add.hauteurs<-valeurs+erreurs$erreur.sup
  y.inf<-limites$yinf
  y.sup<-limites$ysup
  Env$l.var$add.abscisses<-barplot(valeurs,axes=FALSE,ann=FALSE,col=tclvalue(Env$l.var$couleur1A),log=graphe.log(),
    border=tclvalue(Env$l.var$col.borduresA),ylim=c(y.inf,y.sup),names.arg=Env$l.var$nomsprop.fac)
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
	erreur.sup=erreurs$erreur.sup,alert=erreurs$alert,couleur=tclvalue(Env$l.var$couleur2A))
  }
  graphe.titre()
  graphe.axes(type="bar",ordonnee=limites$ordonnee)
  graphe.box()
}

