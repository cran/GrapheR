code.data <-
function() {
  variable<-tclvalue(Env$l.var$variable)
  facteur1<-tclvalue(Env$l.var$facteur1)
  facteur2<-tclvalue(Env$l.var$facteur2)
  niveau<-tclvalue(Env$l.var$niveau)
  if (Env$l.var$ecran=="H") {
    if (nchar(facteur1)>0 & facteur1!=Env$voc[82,1]) {
	cat("# Preliminary data creation\n\n")
	cat(paste("variable <- ",variable,"[",facteur1,"==\"",niveau,"\"]\n\n",sep=""))
    }
    if (tclvalue(Env$l.var$hist.type)==Env$voc[40,1]) {
	if (nchar(facteur1)==0 | facteur1==Env$voc[82,1]) {
	  cat("# Preliminary data creation\n\n")
	}
	texte<-""
	if (nchar(facteur1)>0 & facteur1!=Env$voc[82,1]) {
	  texte<-"frequencies <- hist(variable"
	} else {
	  texte<-paste("frequencies <- hist(",variable,sep="")
	}
	if (tclvalue(Env$l.var$hist.barres)!="Auto") {texte<-paste(texte,", breaks=",as.numeric(tclvalue(Env$l.var$hist.barres))-1,sep="")}
	texte<-paste(texte,", plot=FALSE)$counts / length(na.omit(",sep="")
	if (nchar(facteur1)>0 & facteur1!=Env$voc[82,1]) {
	  texte<- paste(texte,"variable))\n\n",sep="")
	} else {
	  texte<-paste(texte,variable,"))\n\n",sep="")
	}
	if (nchar(facteur1)>0 & facteur1!=Env$voc[82,1]) {
	  texte<-paste(texte,"middles <- hist(variable",sep="")
	} else {
	  texte<-paste(texte,"middles <- hist(",variable,sep="")
	}
	if (tclvalue(Env$l.var$hist.barres)!="Auto") {texte<-paste(texte,", breaks=",as.numeric(tclvalue(Env$l.var$hist.barres))-1,sep="")}
	texte<-paste(texte,", plot=FALSE)$mids\n\n",sep="")
	cat(texte)
    }
  } else if (Env$l.var$ecran=="M") {
    if (nchar(facteur2)>0 & facteur2!=Env$voc[82,1]) {
	cat("# Preliminary data creation\n\n")
	cat(paste("interaction <- factor(paste(",facteur1,",",facteur2,",sep=\":\"))\n",sep=""))
	if (tclvalue(Env$l.var$boxmoy)==1) {
	  cat(paste("means <- tapply(",variable,", interaction, function(x) mean(x,na.rm=TRUE))\n",sep=""))
	}
	cat("\n")
    } else {
	if (tclvalue(Env$l.var$boxmoy)==1) {
	  cat("# Preliminary data creation\n\n")	
	  cat(paste("means <- tapply(",variable,", ",facteur1,", function(x) mean(x,na.rm=TRUE))\n\n",sep=""))
	}
    }
  } else if (Env$l.var$ecran=="B") {
    proportions<-tclvalue(Env$l.var$proportions)
    facteurprop<-tclvalue(Env$l.var$facteurprop)
    if (tclvalue(Env$l.var$moyprop)=="moy") {
	if (tclvalue(Env$l.var$plusieurs)==0) {
	  cat("# Preliminary data creation\n\n")
	  cat(paste("means <- tapply(",variable,",",facteur1,",function(x) mean(x,na.rm=TRUE))\n",sep=""))
	  if (tclvalue(Env$l.var$erreur)==Env$voc[96,1]) {
	    cat(paste("\nstd.dev <- tapply(",variable,",",facteur1,",function(x) sd(x,na.rm=TRUE))\n\n",sep=""))
	  } else if (tclvalue(Env$l.var$erreur)==Env$voc[97,1]) {
	    cat(paste("\nstd.err <- tapply(",variable,",",facteur1,",function(x) sd(x,na.rm=TRUE)/sqrt(length(na.omit(x))))\n\n",sep=""))
	  } else if (tclvalue(Env$l.var$erreur)==Env$voc[98,1]) {
	    cat(paste("\nci <- means - tapply(",variable,",",facteur1,",function(x) t.test(x)$conf.int[1])\n\n",sep=""))
	  } else {
	    cat("\n")
	  }
	} else {
	  cat("# Preliminary data creation\n\n")
	  cat(paste("means <- tapply(",variable,",list(",facteur2,",",facteur1,"),function(x) mean(x,na.rm=TRUE))\n",sep=""))
	  if (tclvalue(Env$l.var$erreur)==Env$voc[96,1]) {
	    cat(paste("\nstd.dev <- tapply(",variable,",list(",facteur2,",",facteur1,"),function(x) sd(x,na.rm=TRUE))\n\n",sep=""))
	  } else if (tclvalue(Env$l.var$erreur)==Env$voc[97,1]) {
	    cat(paste("\nstd.err <- tapply(",variable,",list(",facteur2,",",facteur1,"),function(x) sd(x,na.rm=TRUE)/sqrt(length(na.omit(x))))\n\n",sep=""))
	  } else if (tclvalue(Env$l.var$erreur)==Env$voc[98,1]) {
	    cat(paste("\nci <- means - tapply(",variable,",list(",facteur2,",",facteur1,"),function(x) t.test(x)$conf.int[1])\n\n",sep=""))
	  } else {
	    cat("\n")
	  }
	}
    } else {
	cat("# Preliminary data creation\n\n")
	cat(paste("prop.total <- matrix(0,nrow=nlevels(",proportions,"),ncol=nlevels(",facteurprop,"),dimnames=list(levels(",proportions,"),levels(",facteurprop,")))\n",sep=""))
	cat(paste("for (i in 1:nlevels(",facteurprop,")) {\n",sep=""))
	cat(paste("  for (j in 1:nlevels(",proportions,")) {\n",sep=""))
	cat(paste("    prop.total[j,i] <- length(",proportions,"[",proportions,"==levels(",proportions,")[j] & ",facteurprop,"==levels(",facteurprop,")[i]])/length(na.omit(",
	  proportions,"[",facteurprop,"==levels(",facteurprop,")[i]]))\n",sep=""))
	cat("  }\n}\n")
	if (tclvalue(Env$l.var$plusieurs)==0) {
	  niveaux<-as.numeric(tclvalue(Env$l.var$prop.niveaux))+1
	  cat(paste("proportions <- prop.total[",niveaux,",]\n",sep=""))
	  if (tclvalue(Env$l.var$erreur)==Env$voc[97,1]) {
	    cat("\nstd.err <- NULL\n")
	    cat(paste("for (i in 1:nlevels(",facteurprop,")) {\n",sep=""))
	    cat(paste("  std.err <- c(std.err,sqrt((proportions[i]*(1-proportions[i]))/(length(na.omit(",proportions,"[",facteurprop,"==levels(",facteurprop,")[i]]))-1)))\n",sep=""))
	    cat("}\n\n")
	  } else if (tclvalue(Env$l.var$erreur)==Env$voc[98,1]) {
	    cat("\nci.inf <- NULL\n")
	    cat("ci.sup <- NULL\n")
	    cat(paste("for (i in 1:nlevels(",facteurprop,")) {\n",sep=""))
	    cat(paste("  ci.inf <- c(ci.inf,proportions[i] - binom.test(length(",proportions,"[",proportions,"==levels(",proportions,")[",niveaux,"] & ",facteurprop,
		"==levels(",facteurprop,")[i]]),length(na.omit(",proportions,"[",facteurprop,"==levels(",facteurprop,")[i]])))$conf.int[1])\n",sep=""))
	    cat(paste("  ci.sup <- c(ci.sup,binom.test(length(",proportions,"[",proportions,"==levels(",proportions,")[",niveaux,"] & ",facteurprop,
		"==levels(",facteurprop,")[i]]),length(na.omit(",proportions,"[",facteurprop,"==levels(",facteurprop,")[i]])))$conf.int[2] - proportions[i])\n",sep=""))
	    cat("}\n\n")
	  } else {
	    cat("\n")
	  }
	} else {
  	  niveaux<-as.numeric(strsplit(tclvalue(Env$l.var$prop.niveaux),split=" ")[[1]])+1
	  cat(paste("proportions <- prop.total[c(",paste(niveaux,collapse=","),"),]\n",sep=""))
	  if (tclvalue(Env$l.var$erreur)==Env$voc[97,1]) {
	    cat("\nstd.err <- matrix(0,nrow=nrow(proportions),ncol=ncol(proportions),dimnames=list(rownames(proportions),colnames(proportions)))\n")
	    cat("for (i in 1:ncol(proportions)) {\n")
	    cat("  for (j in 1:nrow(proportions)) {\n")
	    cat(paste("    std.err[j,i] <- sqrt((proportions[j,i]*(1-proportions[j,i]))/(length(na.omit(",proportions,"[",facteurprop,"==levels(",facteurprop,")[i]]))-1))\n",sep=""))
	    cat("  }\n}\n\n")
	  } else if (tclvalue(Env$l.var$erreur)==Env$voc[98,1]) {
	    cat("\nci.inf <- matrix(0,nrow=nrow(proportions),ncol=ncol(proportions),dimnames=list(rownames(proportions),colnames(proportions)))\n")
	    cat("ci.sup <- matrix(0,nrow=nrow(proportions),ncol=ncol(proportions),dimnames=list(rownames(proportions),colnames(proportions)))\n")
	    cat("for (i in 1:ncol(proportions)) {\n")
	    cat(paste("  for (j in c(",paste(niveaux,collapse=","),")) {\n",sep=""))
	    cat(paste("    ci.inf[j,i] <- proportions[j,i] - binom.test(length(",proportions,"[",proportions,"==levels(",proportions,")[j] & ",facteurprop,
		"==levels(",facteurprop,")[i]]),length(na.omit(",proportions,"[",facteurprop,"==levels(",facteurprop,")[i]])))$conf.int[1]\n",sep=""))
	    cat(paste("    ci.sup[j,i] <- binom.test(length(",proportions,"[",proportions,"==levels(",proportions,")[j] & ",facteurprop,
		"==levels(",facteurprop,")[i]]),length(na.omit(",proportions,"[",facteurprop,"==levels(",facteurprop,")[i]])))$conf.int[2] - proportions[j,i]\n",sep=""))
	    cat("  }\n}\n\n")
	  } else {
	    cat("\n")
	  }
	}
    }
  } else if (Env$l.var$ecran=="Ca") {
    niveaux<-levels(factor(Env$dataset[,tclvalue(Env$l.var$variable)]))[as.numeric(strsplit(tclvalue(Env$l.var$parts.niveaux),split=" ")[[1]])+1]
    cat("# Preliminary data creation\n\n")
    cat(paste("prevariable1 <- factor(",variable,")\n",sep=""))
    cat(paste("prevariable2 <- droplevels(prevariable1[prevariable1 %in% c(\"",paste(niveaux,collapse="\",\""),"\")])\n",sep=""))
    cat("variable <- summary(prevariable2)\n\n")
  } else if (Env$l.var$ecran=="Co") {
    cat("# Preliminary data creation\n\n")
    if (tclvalue(Env$l.var$moyprop)=="moy") {
	varX<-tclvalue(Env$l.var$varX)
	varY<-tclvalue(Env$l.var$varY)
	if (tclvalue(Env$l.var$plusieurs)==0) {
	  if (nchar(tclvalue(Env$l.var$facteur1))>0 & tclvalue(Env$l.var$facteur1)!=Env$voc[82,1]) {
	    niveau<-levels(Env$dataset[,tclvalue(Env$l.var$facteur1)])[as.numeric(tclvalue(Env$l.var$niveau))+1]
	    cat(paste("varX <- ",varX,"[",facteur1,"==\"",niveau,"\"]\n",sep=""))
	    cat(paste("varY <- ",varY,"[",facteur1,"==\"",niveau,"\"]\n",sep=""))
	    cat("means <- tapply(varY,varX,function(x) mean(x,na.rm=TRUE))\n")
	    if (tclvalue(Env$l.var$erreur)==Env$voc[96,1]) {
		cat("\nstd.dev <- tapply(varY,varX,function(x) sd(x,na.rm=TRUE))\n\n")
	    } else if (tclvalue(Env$l.var$erreur)==Env$voc[97,1]) {
		cat("\nstd.err <- tapply(varY,varX,function(x) sd(x,na.rm=TRUE)/sqrt(length(na.omit(x))))\n\n")
	    } else if (tclvalue(Env$l.var$erreur)==Env$voc[98,1]) {
		cat("\nci <- means - tapply(varY,varX,function(x) t.test(x)$conf.int[1])\n\n")
	    } else {
		cat("\n")
	    }
	  } else {
	    cat(paste("means <- tapply(",varY,",",varX,",function(x) mean(x,na.rm=TRUE))\n",sep=""))
	    if (tclvalue(Env$l.var$erreur)==Env$voc[96,1]) {
		cat(paste("\nstd.dev <- tapply(",varY,",",varX,",function(x) sd(x,na.rm=TRUE))\n\n",sep=""))
	    } else if (tclvalue(Env$l.var$erreur)==Env$voc[97,1]) {
		cat(paste("\nstd.err <- tapply(",varY,",",varX,",function(x) sd(x,na.rm=TRUE)/sqrt(length(na.omit(x))))\n\n",sep=""))
	    } else if (tclvalue(Env$l.var$erreur)==Env$voc[98,1]) {
		cat(paste("\nci <- means - tapply(",varY,",",varX,",function(x) t.test(x)$conf.int[1])\n\n",sep=""))
	    } else {
		cat("\n")
	    }
	  }
	} else {
	  niveaux<-levels(Env$dataset[,tclvalue(Env$l.var$facteur1)])[as.numeric(strsplit(tclvalue(Env$l.var$niveau),split=" ")[[1]])+1]
	  cat(paste("varX <- ",varX,"[",facteur1," %in% c(\"",paste(niveaux,collapse="\",\""),"\")]\n",sep=""))
	  cat(paste("varY <- ",varY,"[",facteur1," %in% c(\"",paste(niveaux,collapse="\",\""),"\")]\n",sep=""))
	  cat(paste("fact <- ",facteur1,"[",facteur1," %in% c(\"",paste(niveaux,collapse="\",\""),"\")]\n",sep=""))
	  cat("means <- tapply(varY,list(fact,varX),function(x) mean(x,na.rm=TRUE))\n")
	  if (tclvalue(Env$l.var$erreur)==Env$voc[96,1]) {
	    cat("\nstd.dev <- tapply(varY,list(fact,varX),function(x) sd(x,na.rm=TRUE))\n\n")
	  } else if (tclvalue(Env$l.var$erreur)==Env$voc[97,1]) {
	    cat("\nstd.err <- tapply(varY,list(fact,varX),function(x) sd(x,na.rm=TRUE)/sqrt(length(na.omit(x))))\n\n")
	  } else if (tclvalue(Env$l.var$erreur)==Env$voc[98,1]) {
	    cat("\nci <- means - tapply(varY,list(fact,varX),function(x) t.test(x)$conf.int[1])\n\n")
	  } else {
	    cat("\n")
	  }
	}
    } else {
	varX<-tclvalue(Env$l.var$varX.prop)
	varY<-tclvalue(Env$l.var$proportions)
	varY.niv <- tclvalue(Env$l.var$prop.niveaux)
	if (tclvalue(Env$l.var$plusieurs)==0) {
	  if (nchar(tclvalue(Env$l.var$facteur1))>0 & tclvalue(Env$l.var$facteur1)!=Env$voc[82,1]) {
	    niveau<-levels(Env$dataset[,tclvalue(Env$l.var$facteur1)])[as.numeric(tclvalue(Env$l.var$niveau))+1]
	    cat(paste("varX <- factor(",varX,"[",facteur1,"==\"",niveau,"\"])\n",sep=""))
	    cat(paste("varY <- ",varY,"[",facteur1,"==\"",niveau,"\"]\n",sep=""))
	    cat("proportions <- NULL\n")
	    cat("for (i in 1:nlevels(varX)) {\n")
	    cat(paste("  proportions <- c(proportions,length(varY[varY==\"",varY.niv,"\" & varX==levels(varX)[i]])/",
		"length(na.omit(varY[varX==levels(varX)[i]])))","\n",sep=""))
	    cat("}\n")
	    if (tclvalue(Env$l.var$erreur)==Env$voc[97,1]) {
		cat("\nstd.err <- NULL\n")
		cat("for (i in 1:nlevels(varX)) {\n")
		cat("  std.err <- c(std.err,sqrt((proportions[i]*(1-proportions[i]))/(length(na.omit(varY[varX==levels(varX)[i]]))-1)))\n")
		cat("}\n\n")
	    } else if (tclvalue(Env$l.var$erreur)==Env$voc[98,1]) {
		cat("\nci.inf <- NULL\n")
		cat("ci.sup <- NULL\n")
		cat("for (i in 1:nlevels(varX)) {\n")
		cat(paste("  ci.inf <- c(ci.inf,proportions[i] - binom.test(length(varY[varY==\"",varY.niv,"\" & varX==levels(varX)[i]])",
		  ",length(na.omit(varY[varX==levels(varX)[i]])))$conf.int[1])\n",sep=""))
		cat(paste("  ci.sup <- c(ci.sup,binom.test(length(varY[varY==\"",varY.niv,"\" & varX==levels(varX)[i]])",
		  ",length(na.omit(varY[varX==levels(varX)[i]])))$conf.int[2] - proportions[i])\n",sep=""))
		cat("}\n\n")
	    } else {
		cat("\n")
	    }
	  } else {
	    cat(paste("varX <- factor(",varX,")\n",sep=""))
	    cat("proportions <- NULL\n")
	    cat(paste("for (i in 1:nlevels(varX)) {\n",sep=""))
	    cat(paste("  proportions <- c(proportions,length(",varY,"[",varY,"==\"",varY.niv,"\" & varX==levels(varX)[i]])/",
		"length(na.omit(",varY,"[varX==levels(varX)[i]])))","\n",sep=""))
	    cat("}\n")
	    if (tclvalue(Env$l.var$erreur)==Env$voc[97,1]) {
		cat("\nstd.err <- NULL\n")
		cat("for (i in 1:nlevels(varX)) {\n")
		cat(paste("  std.err <- c(std.err,sqrt((proportions[i]*(1-proportions[i]))/(length(na.omit(",varY,"[varX==levels(varX)[i]]))-1)))\n",sep=""))
		cat("}\n\n")
	    } else if (tclvalue(Env$l.var$erreur)==Env$voc[98,1]) {
		cat("\nci.inf <- NULL\n")
		cat("ci.sup <- NULL\n")
		cat(paste("for (i in 1:nlevels(varX)) {\n",sep=""))
		cat(paste("  ci.inf <- c(ci.inf,proportions[i] - binom.test(length(",varY,"[",varY,"==\"",varY.niv,"\" & varX==levels(varX)[i]])",
		  ",length(na.omit(",varY,"[varX==levels(varX)[i]])))$conf.int[1])\n",sep=""))
		cat(paste("  ci.sup <- c(ci.sup,binom.test(length(",varY,"[",varY,"==\"",varY.niv,"\" & varX==levels(varX)[i]])",
		  ",length(na.omit(",varY,"[varX==levels(varX)[i]])))$conf.int[2] - proportions[i])\n",sep=""))
		cat("}\n\n")
	    } else {
		cat("\n")
	    }
	  }
	} else {
	  niveaux<-levels(Env$dataset[,tclvalue(Env$l.var$facteur1)])[as.numeric(strsplit(tclvalue(Env$l.var$niveau),split=" ")[[1]])+1]
	  cat(paste("varX <- factor(",varX,"[",facteur1," %in% c(\"",paste(niveaux,collapse="\",\""),"\")])\n",sep=""))
	  cat(paste("varY <- ",varY,"[",facteur1," %in% c(\"",paste(niveaux,collapse="\",\""),"\")]\n",sep=""))
	  cat(paste("fact <- ",facteur1,"[",facteur1," %in% c(\"",paste(niveaux,collapse="\",\""),"\")]\n",sep=""))
	  cat("proportions <- matrix(0,nrow=nlevels(fact),ncol=nlevels(varX),dimnames=list(levels(fact),levels(varX)))\n")
	  cat("for (i in 1:nlevels(varX)) {\n")
	  cat("  for (j in 1:nlevels(fact)) {\n")
	  cat(paste("    proportions[j,i] <- length(varY[varY==\"",varY.niv,"\" & varX==levels(varX)[i] & fact==levels(fact)[j]])/length(na.omit(",
	    "varY[varX==levels(varX)[i] & fact==levels(fact)[j]]))\n",sep=""))
	  cat("  }\n}\n")
	  if (tclvalue(Env$l.var$erreur)==Env$voc[97,1]) {
	    cat("\nstd.err <- matrix(0,nrow=nrow(proportions),ncol=ncol(proportions),dimnames=list(rownames(proportions),colnames(proportions)))\n")
	    cat("for (i in 1:ncol(proportions)) {\n")
	    cat("  for (j in 1:nrow(proportions)) {\n")
	    cat("    std.err[j,i] <- sqrt((proportions[j,i]*(1-proportions[j,i]))/(length(na.omit(varY[varX==levels(varX)[i] & fact==levels(fact)[j]]))-1))\n")
	    cat("  }\n}\n\n")
	  } else if (tclvalue(Env$l.var$erreur)==Env$voc[98,1]) {
	    cat("\nci.inf <- matrix(0,nrow=nrow(proportions),ncol=ncol(proportions),dimnames=list(rownames(proportions),colnames(proportions)))\n")
	    cat("ci.sup <- matrix(0,nrow=nrow(proportions),ncol=ncol(proportions),dimnames=list(rownames(proportions),colnames(proportions)))\n")
	    cat("for (i in 1:ncol(proportions)) {\n")
	    cat(paste("  for (j in c(\"",paste(niveaux,collapse="\",\""),"\")) {\n",sep=""))
	    cat(paste("    ci.inf[j,i] <- proportions[j,i] - binom.test(length(varY[varY==\"",varY.niv,"\" & varX==levels(varX)[i] & fact==j]),",
		"length(na.omit(varY[varX==levels(varX)[i] & fact==j])))$conf.int[1]\n",sep=""))
	    cat(paste("    ci.sup[j,i] <- binom.test(length(varY[varY==\"",varY.niv,"\" & varX==levels(varX)[i] & fact==j]),",
		"length(na.omit(varY[varX==levels(varX)[i] & fact==j])))$conf.int[2] - proportions[j,i]\n",sep=""))
	    cat("  }\n}\n\n")
	  } else {
	    cat("\n")
	  }
	}
    }
  } else if (Env$l.var$ecran=="N") {
    varX<-tclvalue(Env$l.var$varX)
    varY<-tclvalue(Env$l.var$varY)
    facteur1<-tclvalue(Env$l.var$facteur1)
    if (tclvalue(Env$l.var$plusieurs)==0) {
	if (nchar(tclvalue(Env$l.var$facteur1))>0 & tclvalue(Env$l.var$facteur1)!=Env$voc[82,1]) {
	  niveau<-levels(Env$dataset[,tclvalue(Env$l.var$facteur1)])[as.numeric(tclvalue(Env$l.var$niveau))+1]
	  cat("# Preliminary data creation\n\n")
	  cat(paste("varX <- ",varX,"[",facteur1,"==\"",niveau,"\"]\n\n",sep=""))
	  cat(paste("varY <- ",varY,"[",facteur1,"==\"",niveau,"\"]\n\n",sep=""))
	}
    } else {
	cat("# Preliminary data creation\n\n")
	niveaux<-levels(Env$dataset[,tclvalue(Env$l.var$facteur1)])[as.numeric(strsplit(tclvalue(Env$l.var$niveau),split=" ")[[1]])+1]
	cat(paste("varX <- ",varX,"[",facteur1," %in% c(\"",paste(niveaux,collapse="\",\""),"\")]\n\n",sep=""))
	cat(paste("varY <- ",varY,"[",facteur1," %in% c(\"",paste(niveaux,collapse="\",\""),"\")]\n\n",sep=""))
    }
  }
}
