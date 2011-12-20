graphe.erreurs.calculer2 <-
function(varX,varY,niveau=NULL,valeurs=NULL,facteur=NULL) {
  erreur.inf<-NULL
  erreur.sup<-NULL
  if (tclvalue(Env$l.var$plusieurs)==0) {
    if (tclvalue(Env$l.var$erreur)==Env$voc[97,1]) {
	erreur<-NULL
	for (i in 1:nlevels(varX)) {
	  erreur<-c(erreur,sqrt((valeurs[i]*(1-valeurs[i]))/(length(varY[varX==levels(varX)[i]])-1)))
	}
	erreur.inf<-erreur.sup<-erreur
    } else if (tclvalue(Env$l.var$erreur)==Env$voc[98,1]) {
	for (i in 1:nlevels(varX)) {
	  erreur.inf<-c(erreur.inf,valeurs[i]-binom.test(length(varY[varY==niveau & varX==levels(varX)[i]]),length(na.omit(varY[varX==levels(varX)[i]])))$conf.int[1])
	  erreur.sup<-c(erreur.sup,binom.test(length(varY[varY==niveau & varX==levels(varX)[i]]),length(na.omit(varY[varX==levels(varX)[i]])))$conf.int[2]-valeurs[i])
	}
    } else {
	erreur.inf<-erreur.sup<-rep(0,nlevels(varX))
    }
  } else {
    if (tclvalue(Env$l.var$erreur)==Env$voc[97,1]) {
	erreur<-matrix(0,nrow=nlevels(facteur),ncol=nlevels(varX))
	for (i in 1:nlevels(varX)) {
	  for (j in 1:nlevels(facteur)) {
	    erreur[j,i]<-sqrt((valeurs[j,i]*(1-valeurs[j,i]))/(length(varY[facteur==levels(facteur)[j] & varX==levels(varX)[i]])-1))
	  }
	}
	erreur.inf<-erreur.sup<-erreur
    } else if (tclvalue(Env$l.var$erreur)==Env$voc[98,1]) {
	erreur.inf<-matrix(0,nrow=nlevels(facteur),ncol=nlevels(varX))
	erreur.sup<-matrix(0,nrow=nlevels(facteur),ncol=nlevels(varX))
	for (i in 1:nlevels(varX)) {
	  for (j in 1:nlevels(facteur)) {
	    erreur.inf[j,i]<-valeurs[j,i]-binom.test(length(varY[varY==niveau & varX==levels(varX)[i] & facteur==levels(facteur)[j]]),
		length(varY[varX==levels(varX)[i] & facteur==levels(facteur)[j]]))$conf.int[1]
	    erreur.sup[j,i]<-binom.test(length(varY[varY==niveau & varX==levels(varX)[i] & facteur==levels(facteur)[j]]),
		length(varY[varX==levels(varX)[i] & facteur==levels(facteur)[j]]))$conf.int[2]-valeurs[j,i]
	  }
	}
    } else {
	erreur.inf<-erreur.sup<-matrix(0,nrow=nlevels(facteur),ncol=nlevels(varX))
    }
  }
  return(list(erreur.inf=erreur.inf,erreur.sup=erreur.sup))
}

