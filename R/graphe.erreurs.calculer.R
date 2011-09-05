graphe.erreurs.calculer <-
function(variable,facteur1,facteur2=NULL,valeurs=NULL,prop.nvx=NULL) {
  alert=FALSE
  erreur.inf<-NULL
  erreur.sup<-NULL
  if (tclvalue(Env$l.var$moyprop)=="moy") {
    if (tclvalue(Env$l.var$plusieurs)==0) {
	if (tclvalue(Env$l.var$erreur)==Env$voc[96,1]) {
	  erreur<-tapply(variable,facteur1,function(x) sd(x,na.rm=TRUE))
	  erreur.inf<-erreur.sup<-erreur
	} else if (tclvalue(Env$l.var$erreur)==Env$voc[97,1]) {
	  erreur<-tapply(variable,facteur1,function(x) sd(x,na.rm=TRUE)/sqrt(length(na.omit(x))))
	  erreur.inf<-erreur.sup<-erreur
	} else if (tclvalue(Env$l.var$erreur)==Env$voc[98,1]) {
	  test<-tapply(variable,facteur1,function(x) length(na.omit(x)))
	  if (any(test<20)) {
	    medianes<-tapply(variable,facteur1,function(x) median(x,na.rm=TRUE))
	    erreur.inf<-medianes-tapply(variable,facteur1,function(x) wilcox.test(x,conf.int=TRUE)$conf.int[1])
	    erreur.sup<-tapply(variable,facteur1,function(x) wilcox.test(x,conf.int=TRUE)$conf.int[2])-medianes
	    alert<-TRUE
	  } else {
	    moyennes<-tapply(variable,facteur1,function(x) mean(x,na.rm=TRUE))
	    erreur.inf<-erreur.sup<-moyennes-tapply(variable,facteur1,function(x) t.test(x)$conf.int[1])
	  }
	} else {
	  erreur.inf<-erreur.sup<-rep(0,nlevels(facteur1))
	}
    } else {
	if (tclvalue(Env$l.var$erreur)==Env$voc[96,1]) {
	  erreur<-tapply(variable,list(facteur2,facteur1),function(x) sd(x,na.rm=TRUE))
	  erreur.inf<-erreur.sup<-erreur
	} else if (tclvalue(Env$l.var$erreur)==Env$voc[97,1]) {
	  erreur<-tapply(variable,list(facteur2,facteur1),function(x) sd(x,na.rm=TRUE)/sqrt(length(na.omit(x))))
	  erreur.inf<-erreur.sup<-erreur
	} else if (tclvalue(Env$l.var$erreur)==Env$voc[98,1]) {
	  test<-tapply(variable,list(facteur2,facteur1),function(x) length(na.omit(x)))
	  if (any(test<20)) {
	    medianes<-tapply(variable,list(facteur2,facteur1),function(x) median(x,na.rm=TRUE))
	    erreur.inf<-medianes-tapply(variable,list(facteur2,facteur1),function(x) wilcox.test(x,conf.int=TRUE)$conf.int[1])
	    erreur.sup<-tapply(variable,list(facteur2,facteur1),function(x) wilcox.test(x,conf.int=TRUE)$conf.int[2])-medianes
	    alert<-TRUE
	  } else {
	    moyennes<-tapply(variable,list(facteur2,facteur1),function(x) mean(x,na.rm=TRUE))
	    erreur.inf<-erreur.sup<-moyennes-tapply(variable,list(facteur2,facteur1),function(x) t.test(x)$conf.int[1])
	  }
	} else {
	  erreur.inf<-erreur.sup<-matrix(0,ncol=nlevels(facteur1),nrow=nlevels(facteur2))
	}
    }
  } else {
    if (tclvalue(Env$l.var$plusieurs)==0) {
	if (tclvalue(Env$l.var$erreur)==Env$voc[97,1]) {
	  erreur<-NULL
	  for (i in 1:nlevels(facteur1)) {
	    erreur<-c(erreur,sqrt((valeurs[i]*(1-valeurs[i]))/(length(na.omit(variable[facteur1==levels(facteur1)[i]]))-1)))
	  }
	  erreur.inf<-erreur.sup<-erreur
	} else if (tclvalue(Env$l.var$erreur)==Env$voc[98,1]) {
	  for (i in 1:nlevels(facteur1)) {
	    erreur.inf<-c(erreur.inf,valeurs[i]-binom.test(length(variable[variable==levels(variable)[prop.nvx] & facteur1==levels(facteur1)[i]]),length(na.omit(variable[facteur1==levels(facteur1)[i]])))$conf.int[1])
	    erreur.sup<-c(erreur.sup,binom.test(length(variable[variable==levels(variable)[prop.nvx] & facteur1==levels(facteur1)[i]]),length(na.omit(variable[facteur1==levels(facteur1)[i]])))$conf.int[2]-valeurs[i])
	  }
	} else {
	  erreur.inf<-erreur.sup<-rep(0,nlevels(facteur1))
	}
    } else {
	if (tclvalue(Env$l.var$erreur)==Env$voc[97,1]) {
	  erreur<-matrix(0,nrow=nlevels(variable),ncol=nlevels(facteur1))
	  for (i in 1:nlevels(facteur1)) {
	    for (j in 1:length(prop.nvx)) {
		erreur[j,i]<-sqrt((valeurs[j,i]*(1-valeurs[j,i]))/(length(na.omit(variable[facteur1==levels(facteur1)[i]]))-1))
	    }
	  }
	  erreur.inf<-erreur.sup<-erreur
	} else if (tclvalue(Env$l.var$erreur)==Env$voc[98,1]) {
	  erreur.inf<-matrix(0,nrow=length(prop.nvx),ncol=nlevels(facteur1))
	  erreur.sup<-matrix(0,nrow=length(prop.nvx),ncol=nlevels(facteur1))
	  for (i in 1:nlevels(facteur1)) {
	    for (j in 1:length(prop.nvx)) {
		erreur.inf[j,i]<-valeurs[j,i]-binom.test(length(variable[variable==levels(variable)[prop.nvx[j]] & facteur1==levels(facteur1)[i]]),
		  length(na.omit(variable[facteur1==levels(facteur1)[i]])))$conf.int[1]
		erreur.sup[j,i]<-binom.test(length(variable[variable==levels(variable)[prop.nvx[j]] & facteur1==levels(facteur1)[i]]),
		  length(na.omit(variable[facteur1==levels(facteur1)[i]])))$conf.int[2]-valeurs[j,i]
	    }
	  }
	} else {
	  erreur.inf<-erreur.sup<-matrix(0,nrow=nlevels(variable),ncol=nlevels(facteur1))
	}
    }
  }
  return(list(erreur.inf=erreur.inf,erreur.sup=erreur.sup,alert=alert))
}