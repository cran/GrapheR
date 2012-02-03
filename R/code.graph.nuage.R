code.graph.nuage<-function() {
  varX<-tclvalue(Env$l.var$varX)
  varY<-tclvalue(Env$l.var$varY)
  facteur1<-tclvalue(Env$l.var$facteur1)
  texte<-""
  if (tclvalue(Env$l.var$plusieurs)==0) {
    if (nchar(facteur1)>0 & facteur1!=Env$voc[82,1]) {
	texte<-"plot(varY ~ varX, axes=FALSE, ann=FALSE"
    } else {
	texte<-paste("plot(",varY," ~ ",varX,", axes=FALSE, ann=FALSE",sep="")
    }
    texte<-paste(texte,", xlim=c(",round(Env$l.code$x.inf,2),",",round(Env$l.code$x.sup,2),")",sep="")
    texte<-paste(texte,", ylim=c(",round(Env$l.code$y.inf,2),",",round(Env$l.code$y.sup,2),")",sep="")
    if (graphe.log()!="") {texte<-paste(texte,", log=\"",graphe.log(),"\"",sep="")}
    if (tclvalue(Env$l.var$couleur2A)!="black" & tclvalue(Env$l.var$couleur2A)!="#000000") {texte<-paste(texte,", col=\"",tclvalue(Env$l.var$couleur2A),"\"",sep="")}
    texte<-paste(texte,",\n  pch=",graphe.symboles(num=as.numeric(tclvalue(Env$l.var$symboleA))),sep="")
    if (tclvalue(Env$l.var$taille.ptsA)!="1") {texte<-paste(texte,", cex=",as.numeric(tclvalue(Env$l.var$taille.ptsA)),sep="")}
    cat(paste(texte,")\n\n",sep=""))
    if (tclvalue(Env$l.var$ptlab)==1) {
	texte<-""
	if (nchar(facteur1)>0 & facteur1!=Env$voc[82,1]) {
	  texte<-"text(varX, varY, pos=3, offset=0.4"
	} else {
	  texte<-paste("text(",varX,", ",varY,", pos=3, offset=0.4",sep="")
	}
	if (tclvalue(Env$l.var$couleur2A)!="black" & tclvalue(Env$l.var$couleur2A)!="#000000") {texte<-paste(texte,", col=\"",tclvalue(Env$l.var$couleur2A),"\"",sep="")}
	texte<-paste(texte,", cex=",0.65*as.numeric(tclvalue(Env$l.var$taille.ptsA)),")\n\n",sep="")
	cat(texte)
    }
    code.graph.axes()
    code.graph.titre()
    if (tclvalue(Env$l.var$encadre)==1) {cat("box()\n\n")}
    if (tclvalue(Env$l.var$droiteA)==Env$voc[145,1]) {
	cat("# Regression line\n\n")
	texte<-""
	if (nchar(facteur1)>0 & facteur1!=Env$voc[82,1]) {
	  texte<-"model <- lm(varY ~ varX)\n"
	} else {
	  texte<-paste("model <- lm(",varY," ~ ",varX,")\n",sep="")
	}
	texte<-paste(texte,"abline(model$coefficients",sep="")
	if (tclvalue(Env$l.var$couleur2A)!="black" & tclvalue(Env$l.var$couleur2A)!="#000000") {texte<-paste(texte,", col=\"",tclvalue(Env$l.var$couleur2A),"\"",sep="")}
	texte<-paste(texte,", lty=",type.trait(type=tclvalue(Env$l.var$trait1)),sep="")
	texte<-paste(texte,", lwd=",as.numeric(tclvalue(Env$l.var$epaisseur1)),sep="")
	cat(paste(texte,")\n",sep=""))
	if (tclvalue(Env$l.var$intervalA)==Env$voc[261,1]) {
	  if (nchar(facteur1)>0 & facteur1!=Env$voc[82,1]) {
	    texte<-"varX2 <- seq(min(varX, na.rm=TRUE), max(varX, na.rm=TRUE), abs(max(varX, na.rm=TRUE) - min(varX, na.rm=TRUE)) / 1000)\n"
	    texte<-paste(texte,"varY2 <- predict(model, list(varX=varX2), interval=\"confidence\")\n",sep="")
	  } else {
	    texte<-paste("varX2 <- seq(min(",varX,", na.rm=TRUE), max(",varX,", na.rm=TRUE), abs(max(",varX,", na.rm=TRUE) - min(",varX,", na.rm=TRUE)) / 1000)\n",sep="")
	    texte<-paste(texte,"varY2 <- predict(model, list(",varX,"=varX2), interval=\"confidence\")\n",sep="")
	  }
	  texte<-paste(texte,"lines(varX2, varY2[,\"lwr\"], lty=2",sep="")
	  if (tclvalue(Env$l.var$couleur2A)!="black" & tclvalue(Env$l.var$couleur2A)!="#000000") {texte<-paste(texte,", col=\"",tclvalue(Env$l.var$couleur2A),"\"",sep="")}
	  texte<-paste(texte,")\n",sep="")
	  texte<-paste(texte,"lines(varX2, varY2[,\"upr\"], lty=2",sep="")
	  if (tclvalue(Env$l.var$couleur2A)!="black" & tclvalue(Env$l.var$couleur2A)!="#000000") {texte<-paste(texte,", col=\"",tclvalue(Env$l.var$couleur2A),"\"",sep="")}
	  texte<-paste(texte,")\n",sep="")
	  cat(texte)
	}
	if (tclvalue(Env$l.var$intervalA)==Env$voc[262,1]) {
	  if (nchar(facteur1)>0 & facteur1!=Env$voc[82,1]) {
	    texte<-"varX2 <- seq(min(varX, na.rm=TRUE), max(varX, na.rm=TRUE), abs(max(varX, na.rm=TRUE) - min(varX, na.rm=TRUE)) / 1000)\n"
	    texte<-paste(texte,"varY2 <- predict(model, list(varX=varX2), interval=\"confidence\")\n",sep="")
	  } else {
	    texte<-paste("varX2 <- seq(min(",varX,", na.rm=TRUE), max(",varX,", na.rm=TRUE), abs(max(",varX,", na.rm=TRUE) - min(",varX,", na.rm=TRUE)) / 1000)\n",sep="")
	    texte<-paste(texte,"varY2 <- predict(model, list(",varX,"=varX2), interval=\"prediction\")\n",sep="")
	  }
	  texte<-paste(texte,"lines(varX2, varY2[,\"lwr\"], lty=3",sep="")
	  if (tclvalue(Env$l.var$couleur2A)!="black" & tclvalue(Env$l.var$couleur2A)!="#000000") {texte<-paste(texte,", col=\"",tclvalue(Env$l.var$couleur2A),"\"",sep="")}
	  texte<-paste(texte,")\n",sep="")
	  texte<-paste(texte,"lines(varX2, varY2[,\"upr\"], lty=3",sep="")
	  if (tclvalue(Env$l.var$couleur2A)!="black" & tclvalue(Env$l.var$couleur2A)!="#000000") {texte<-paste(texte,", col=\"",tclvalue(Env$l.var$couleur2A),"\"",sep="")}
	  texte<-paste(texte,")\n",sep="")
	  cat(texte)
	}
	if (tclvalue(Env$l.var$intervalA)==Env$voc[263,1]) {
	  if (nchar(facteur1)>0 & facteur1!=Env$voc[82,1]) {
	    texte<-"varX2 <- seq(min(varX, na.rm=TRUE), max(varX, na.rm=TRUE), abs(max(varX, na.rm=TRUE) - min(varX, na.rm=TRUE)) / 1000)\n"
	    texte<-paste(texte,"varY2.conf <- predict(model, list(varX=varX2), interval=\"confidence\")\n",sep="")
	    texte<-paste(texte,"varY2.pred <- predict(model, list(varX=varX2), interval=\"prediction\")\n",sep="")
	  } else {
	    texte<-paste("varX2 <- seq(min(",varX,", na.rm=TRUE), max(",varX,", na.rm=TRUE), abs(max(",varX,", na.rm=TRUE) - min(",varX,", na.rm=TRUE)) / 1000)\n",sep="")
	    texte<-paste(texte,"varY2.conf <- predict(model, list(",varX,"=varX2), interval=\"confidence\")\n",sep="")
	    texte<-paste(texte,"varY2.pred <- predict(model, list(",varX,"=varX2), interval=\"prediction\")\n",sep="")
	  }
	  texte<-paste(texte,"lines(varX2, varY2.conf[,\"lwr\"], lty=2",sep="")
	  if (tclvalue(Env$l.var$couleur2A)!="black" & tclvalue(Env$l.var$couleur2A)!="#000000") {texte<-paste(texte,", col=\"",tclvalue(Env$l.var$couleur2A),"\"",sep="")}
	  texte<-paste(texte,")\n",sep="")
	  texte<-paste(texte,"lines(varX2, varY2.conf[,\"upr\"], lty=2",sep="")
	  if (tclvalue(Env$l.var$couleur2A)!="black" & tclvalue(Env$l.var$couleur2A)!="#000000") {texte<-paste(texte,", col=\"",tclvalue(Env$l.var$couleur2A),"\"",sep="")}
	  texte<-paste(texte,")\n",sep="")
	  texte<-paste(texte,"lines(varX2, varY2.pred[,\"lwr\"], lty=3",sep="")
	  if (tclvalue(Env$l.var$couleur2A)!="black" & tclvalue(Env$l.var$couleur2A)!="#000000") {texte<-paste(texte,", col=\"",tclvalue(Env$l.var$couleur2A),"\"",sep="")}
	  texte<-paste(texte,")\n",sep="")
	  texte<-paste(texte,"lines(varX2, varY2.pred[,\"upr\"], lty=3",sep="")
	  if (tclvalue(Env$l.var$couleur2A)!="black" & tclvalue(Env$l.var$couleur2A)!="#000000") {texte<-paste(texte,", col=\"",tclvalue(Env$l.var$couleur2A),"\"",sep="")}
	  texte<-paste(texte,")\n",sep="")
	  cat(texte)
	}
	cat("\n")
    } else if (tclvalue(Env$l.var$droiteA)==Env$voc[146,1]) {
	cat("# Regression line\n\n")
	texte<-""
	if (nchar(facteur1)>0 & facteur1!=Env$voc[82,1]) {
	  texte<-"b <- sd(varY, na.rm=TRUE) / sd(varX, na.rm=TRUE) * sign(cov(varX, varY, use=\"complete.obs\"))\n"
	  texte<-paste(texte,"a <- mean(varY, na.rm=TRUE) - b * mean(varX, na.rm=TRUE)\n",sep="")
	} else {
	  texte<-paste("b <- sd(",varY,", na.rm=TRUE) / sd(",varX,", na.rm=TRUE) * sign(cov(",varX,", ",varY,", use=\"complete.obs\"))\n",sep="")
	  texte<-paste(texte,"a <- mean(",varY,", na.rm=TRUE) - b * mean(",varX,", na.rm=TRUE)\n",sep="")
	}
	texte<-paste(texte,"abline(a, b",sep="")
	if (tclvalue(Env$l.var$couleur2A)!="black" & tclvalue(Env$l.var$couleur2A)!="#000000") {texte<-paste(texte,", col=\"",tclvalue(Env$l.var$couleur2A),"\"",sep="")}
	texte<-paste(texte,", lty=",type.trait(type=tclvalue(Env$l.var$trait1)),sep="")
	texte<-paste(texte,", lwd=",as.numeric(tclvalue(Env$l.var$epaisseur1)),sep="")
	cat(paste(texte,")\n\n",sep=""))
    } else if (tclvalue(Env$l.var$droiteA)==Env$voc[147,1]) {
	cat("# Regression line\n\n")
	texte<-""
	if (nchar(facteur1)>0 & facteur1!=Env$voc[82,1]) {
	  texte<-"model <- lm(varY ~ varX + I(varX ^ 2))\n"
	  texte<-paste(texte,"varX2 <- seq(min(varX, na.rm=TRUE), max(varX, na.rm=TRUE), abs(max(varX, na.rm=TRUE) - min(varX, na.rm=TRUE)) / 1000)\n",sep="")
	  texte<-paste(texte,"varY2 <- predict(model, list(varX = varX2))\n",sep="")
	} else {
	  texte<-paste("model <- lm(",varY," ~ ",varX," + I(",varX," ^ 2))\n",sep="")
	  texte<-paste(texte,"varX2 <- seq(min(",varX,", na.rm=TRUE), max(",varX,", na.rm=TRUE), abs(max(",varX,", na.rm=TRUE) - min(",varX,", na.rm=TRUE)) / 1000)\n",sep="")
	  texte<-paste(texte,"varY2 <- predict(model, list(",varX," = varX2))\n",sep="")
	}
	texte<-paste(texte,"lines(varX2, varY2",sep="")
	if (tclvalue(Env$l.var$couleur2A)!="black" & tclvalue(Env$l.var$couleur2A)!="#000000") {texte<-paste(texte,", col=\"",tclvalue(Env$l.var$couleur2A),"\"",sep="")}
	texte<-paste(texte,", lty=",type.trait(type=tclvalue(Env$l.var$trait1)),sep="")
	texte<-paste(texte,", lwd=",as.numeric(tclvalue(Env$l.var$epaisseur1)),sep="")
	cat(paste(texte,")\n",sep=""))
	if (tclvalue(Env$l.var$intervalA)==Env$voc[261,1]) {
	  texte<-""
	  if (nchar(facteur1)>0 & facteur1!=Env$voc[82,1]) {
	    texte<-paste("varY2.conf <- predict(model, list(varX=varX2), interval=\"confidence\")\n",sep="")
	  } else {
	    texte<-paste("varY2.conf <- predict(model, list(",varX,"=varX2), interval=\"confidence\")\n",sep="")
	  }
	  texte<-paste(texte,"lines(varX2, varY2.conf[,\"lwr\"], lty=2",sep="")
	  if (tclvalue(Env$l.var$couleur2A)!="black" & tclvalue(Env$l.var$couleur2A)!="#000000") {texte<-paste(texte,", col=\"",tclvalue(Env$l.var$couleur2A),"\"",sep="")}
	  texte<-paste(texte,")\n",sep="")
	  texte<-paste(texte,"lines(varX2, varY2.conf[,\"upr\"], lty=2",sep="")
	  if (tclvalue(Env$l.var$couleur2A)!="black" & tclvalue(Env$l.var$couleur2A)!="#000000") {texte<-paste(texte,", col=\"",tclvalue(Env$l.var$couleur2A),"\"",sep="")}
	  texte<-paste(texte,")\n",sep="")
	  cat(texte)
	}
	if (tclvalue(Env$l.var$intervalA)==Env$voc[262,1]) {
	  texte<-""
	  if (nchar(facteur1)>0 & facteur1!=Env$voc[82,1]) {
	    texte<-paste("varY2.pred <- predict(model, list(varX=varX2), interval=\"confidence\")\n",sep="")
	  } else {
	    texte<-paste("varY2.pred <- predict(model, list(",varX,"=varX2), interval=\"prediction\")\n",sep="")
	  }
	  texte<-paste(texte,"lines(varX2, varY2.pred[,\"lwr\"], lty=3",sep="")
	  if (tclvalue(Env$l.var$couleur2A)!="black" & tclvalue(Env$l.var$couleur2A)!="#000000") {texte<-paste(texte,", col=\"",tclvalue(Env$l.var$couleur2A),"\"",sep="")}
	  texte<-paste(texte,")\n",sep="")
	  texte<-paste(texte,"lines(varX2, varY2.pred[,\"upr\"], lty=3",sep="")
	  if (tclvalue(Env$l.var$couleur2A)!="black" & tclvalue(Env$l.var$couleur2A)!="#000000") {texte<-paste(texte,", col=\"",tclvalue(Env$l.var$couleur2A),"\"",sep="")}
	  texte<-paste(texte,")\n",sep="")
	  cat(texte)
	}
	if (tclvalue(Env$l.var$intervalA)==Env$voc[263,1]) {
	  texte<-""
	  if (nchar(facteur1)>0 & facteur1!=Env$voc[82,1]) {
	    texte<-paste("varY2.conf <- predict(model, list(varX=varX2), interval=\"confidence\")\n",sep="")
	    texte<-paste(texte,"varY2.pred <- predict(model, list(varX=varX2), interval=\"prediction\")\n",sep="")
	  } else {
	    texte<-paste("varY2.conf <- predict(model, list(",varX,"=varX2), interval=\"confidence\")\n",sep="")
	    texte<-paste(texte,"varY2.pred <- predict(model, list(",varX,"=varX2), interval=\"prediction\")\n",sep="")
	  }
	  texte<-paste(texte,"lines(varX2, varY2.conf[,\"lwr\"], lty=2",sep="")
	  if (tclvalue(Env$l.var$couleur2A)!="black" & tclvalue(Env$l.var$couleur2A)!="#000000") {texte<-paste(texte,", col=\"",tclvalue(Env$l.var$couleur2A),"\"",sep="")}
	  texte<-paste(texte,")\n",sep="")
	  texte<-paste(texte,"lines(varX2, varY2.conf[,\"upr\"], lty=2",sep="")
	  if (tclvalue(Env$l.var$couleur2A)!="black" & tclvalue(Env$l.var$couleur2A)!="#000000") {texte<-paste(texte,", col=\"",tclvalue(Env$l.var$couleur2A),"\"",sep="")}
	  texte<-paste(texte,")\n",sep="")
	  texte<-paste(texte,"lines(varX2, varY2.pred[,\"lwr\"], lty=3",sep="")
	  if (tclvalue(Env$l.var$couleur2A)!="black" & tclvalue(Env$l.var$couleur2A)!="#000000") {texte<-paste(texte,", col=\"",tclvalue(Env$l.var$couleur2A),"\"",sep="")}
	  texte<-paste(texte,")\n",sep="")
	  texte<-paste(texte,"lines(varX2, varY2.pred[,\"upr\"], lty=3",sep="")
	  if (tclvalue(Env$l.var$couleur2A)!="black" & tclvalue(Env$l.var$couleur2A)!="#000000") {texte<-paste(texte,", col=\"",tclvalue(Env$l.var$couleur2A),"\"",sep="")}
	  texte<-paste(texte,")\n",sep="")
	  cat(texte)
	}
	cat("\n")
    } else if (tclvalue(Env$l.var$droiteA)==Env$voc[148,1]) {
	cat("# Tendency curve\n\n")
	texte<-""
	if (nchar(facteur1)>0 & facteur1!=Env$voc[82,1]) {
	  texte<-"panel.smooth(varX, varY"
	} else {
	  texte<-paste("panel.smooth(",varX,", ",varY,sep="")
	}
	if (tclvalue(Env$l.var$couleur2A)!="black" & tclvalue(Env$l.var$couleur2A)!="#000000") {
	  texte<-paste(texte,", col=\"",tclvalue(Env$l.var$couleur2A),"\"",sep="")
	  texte<-paste(texte,", col.smooth=\"",tclvalue(Env$l.var$couleur2A),"\"",sep="")
	}
	texte<-paste(texte,", pch=",graphe.symboles(num=as.numeric(tclvalue(Env$l.var$symboleA))),sep="")
	if (tclvalue(Env$l.var$taille.ptsA)!="1") {texte<-paste(texte,", cex=",as.numeric(tclvalue(Env$l.var$taille.ptsA)),sep="")}
	texte<-paste(texte,", lty=",type.trait(type=tclvalue(Env$l.var$trait1)),sep="")
	texte<-paste(texte,", lwd=",as.numeric(tclvalue(Env$l.var$epaisseur1)),sep="")
	cat(paste(texte,")\n\n",sep=""))
    }
  } else {
    niveaux<-levels(Env$dataset[,facteur1])[as.numeric(strsplit(tclvalue(Env$l.var$niveau),split=" ")[[1]])+1]
    symboles<-graphe.symboles(num=Env$l.var$symboleB)
    texte<-paste("plot(varY[",facteur1,"==\"",niveaux[1],"\"] ~ varX[",facteur1,"==\"",niveaux[1],"\"], axes=FALSE, ann=FALSE",sep="")
    texte<-paste(texte,", xlim=c(",round(Env$l.code$x.inf,2),",",round(Env$l.code$x.sup,2),")",sep="")
    texte<-paste(texte,", ylim=c(",round(Env$l.code$y.inf,2),",",round(Env$l.code$y.sup,2),")",sep="")
    if (graphe.log()!="") {texte<-paste(texte,", log=\"",graphe.log(),"\"",sep="")}
    texte<-paste(texte,", col=\"",Env$l.var$couleur2B[1],"\"",sep="")
    texte<-paste(texte,",\n  pch=",symboles[1],sep="")
    if (Env$l.var$taille.ptsB[1]!="1") {texte<-paste(texte,", cex=",as.numeric(Env$l.var$taille.ptsB[1]),sep="")}
    cat(paste(texte,")\n",sep=""))
    for (i in 2:length(niveaux)) {
	texte<-paste("points(varY[",facteur1,"==\"",niveaux[i],"\"] ~ varX[",facteur1,"==\"",niveaux[i],"\"]",sep="")
	texte<-paste(texte,", col=\"",Env$l.var$couleur2B[i],"\"",sep="")
	texte<-paste(texte,", pch=",symboles[i],sep="")
	if (Env$l.var$taille.ptsB[i]!=1) {texte<-paste(texte,", cex=",Env$l.var$taille.ptsB[i],sep="")}
	cat(paste(texte,")\n",sep=""))
    }
    cat("\n")
    if (tclvalue(Env$l.var$ptlab)==1) {
	for (i in 1:length(niveaux)) {
	  texte<-paste("text(varX[",facteur1,"==\"",niveaux[i],"\"], varY[",facteur1,"==\"",niveaux[i],"\"], pos=3, offset=0.4",sep="")
	  texte<-paste(texte,", col=\"",Env$l.var$couleur2B[i],"\"",sep="")
	  texte<-paste(texte,", cex=",round(0.65*Env$l.var$taille.ptsB[i],2),")\n",sep="")
	  cat(texte)
	}
	cat("\n")
    }
    code.graph.axes()
    code.graph.titre()
    if (tclvalue(Env$l.var$encadre)==1) {cat("box()\n\n")}
    for (i in 1:length(niveaux)) {
	if (Env$l.var$droiteB[i]==Env$voc[145,1]) {
	  cat("# Regression line\n\n")
	  texte<-paste("varX.",i," <- varX[",facteur1,"==\"",niveaux[i],"\"]\n",sep="")
	  texte<-paste(texte,"varY.",i," <- varY[",facteur1,"==\"",niveaux[i],"\"]\n",sep="")
	  texte<-paste(texte,"model.",i," <- lm(varY.",i," ~ varX.",i,")\n",sep="")
	  texte<-paste(texte,"abline(model.",i,"$coefficients",sep="")
	  if (Env$l.var$couleur2B[i]!="black" & Env$l.var$couleur2B[i]!="#000000") {texte<-paste(texte,", col=\"",Env$l.var$couleur2B[i],"\"",sep="")}
	  texte<-paste(texte,", lty=",type.trait(type=Env$l.var$trait2[i]),sep="")
	  texte<-paste(texte,", lwd=",Env$l.var$epaisseur2[i],sep="")
	  cat(paste(texte,")\n",sep=""))
	  if (Env$l.var$intervalB[i]==Env$voc[261,1]) {
	    texte<-paste("varX2.",i," <- seq(min(varX.",i,", na.rm=TRUE), max(varX.",i,", na.rm=TRUE), abs(max(varX.",i,", na.rm=TRUE) - min(varX.",i,", na.rm=TRUE)) / 1000)\n",sep="")
	    texte<-paste(texte,"varY2.",i," <- predict(model.",i,", list(varX.",i,"=varX2.",i,"), interval=\"confidence\")\n",sep="")
	    texte<-paste(texte,"lines(varX2.",i,", varY2.",i,"[,\"lwr\"], lty=2",sep="")
	    if (Env$l.var$couleur2B[i]!="black" & Env$l.var$couleur2B[i]!="#000000") {texte<-paste(texte,", col=\"",Env$l.var$couleur2B[i],"\"",sep="")}
	    texte<-paste(texte,")\n",sep="")
	    texte<-paste(texte,"lines(varX2.",i,", varY2.",i,"[,\"upr\"], lty=2",sep="")
	    if (Env$l.var$couleur2B[i]!="black" & Env$l.var$couleur2B[i]!="#000000") {texte<-paste(texte,", col=\"",Env$l.var$couleur2B[i],"\"",sep="")}
	    texte<-paste(texte,")\n",sep="")
	    cat(texte)
	  }
	  if (Env$l.var$intervalB[i]==Env$voc[262,1]) {
	    texte<-paste("varX2.",i," <- seq(min(varX.",i,", na.rm=TRUE), max(varX.",i,", na.rm=TRUE), abs(max(varX.",i,", na.rm=TRUE) - min(varX.",i,", na.rm=TRUE)) / 1000)\n",sep="")
	    texte<-paste(texte,"varY2.",i," <- predict(model.",i,", list(varX.",i,"=varX2.",i,"), interval=\"prediction\")\n",sep="")
	    texte<-paste(texte,"lines(varX2.",i,", varY2.",i,"[,\"lwr\"], lty=3",sep="")
	    if (Env$l.var$couleur2B[i]!="black" & Env$l.var$couleur2B[i]!="#000000") {texte<-paste(texte,", col=\"",Env$l.var$couleur2B[i],"\"",sep="")}
	    texte<-paste(texte,")\n",sep="")
	    texte<-paste(texte,"lines(varX2.",i,", varY2.",i,"[,\"upr\"], lty=3",sep="")
	    if (Env$l.var$couleur2B[i]!="black" & Env$l.var$couleur2B[i]!="#000000") {texte<-paste(texte,", col=\"",Env$l.var$couleur2B[i],"\"",sep="")}
	    texte<-paste(texte,")\n",sep="")
	    cat(texte)
	  }
	  if (Env$l.var$intervalB[i]==Env$voc[263,1]) {
	    texte<-paste("varX2.",i," <- seq(min(varX.",i,", na.rm=TRUE), max(varX.",i,", na.rm=TRUE), abs(max(varX.",i,", na.rm=TRUE) - min(varX.",i,", na.rm=TRUE)) / 1000)\n",sep="")
	    texte<-paste(texte,"varY2.",i,".conf <- predict(model.",i,", list(varX.",i,"=varX2.",i,"), interval=\"confidence\")\n",sep="")
	    texte<-paste(texte,"varY2.",i,".pred <- predict(model.",i,", list(varX.",i,"=varX2.",i,"), interval=\"prediction\")\n",sep="")
	    texte<-paste(texte,"lines(varX2.",i,", varY2.",i,".conf[,\"lwr\"], lty=2",sep="")
	    if (Env$l.var$couleur2B[i]!="black" & Env$l.var$couleur2B[i]!="#000000") {texte<-paste(texte,", col=\"",Env$l.var$couleur2B[i],"\"",sep="")}
	    texte<-paste(texte,")\n",sep="")
	    texte<-paste(texte,"lines(varX2.",i,", varY2.",i,".conf[,\"upr\"], lty=2",sep="")
	    if (Env$l.var$couleur2B[i]!="black" & Env$l.var$couleur2B[i]!="#000000") {texte<-paste(texte,", col=\"",Env$l.var$couleur2B[i],"\"",sep="")}
	    texte<-paste(texte,")\n",sep="")
	    texte<-paste(texte,"lines(varX2.",i,", varY2.",i,".pred[,\"lwr\"], lty=3",sep="")
	    if (Env$l.var$couleur2B[i]!="black" & Env$l.var$couleur2B[i]!="#000000") {texte<-paste(texte,", col=\"",Env$l.var$couleur2B[i],"\"",sep="")}
	    texte<-paste(texte,")\n",sep="")
	    texte<-paste(texte,"lines(varX2.",i,", varY2.",i,".pred[,\"upr\"], lty=3",sep="")
	    if (Env$l.var$couleur2B[i]!="black" & Env$l.var$couleur2B[i]!="#000000") {texte<-paste(texte,", col=\"",Env$l.var$couleur2B[i],"\"",sep="")}
	    texte<-paste(texte,")\n",sep="")
	    cat(texte)
	  }
	  cat("\n")
	} else if (Env$l.var$droiteB[i]==Env$voc[146,1]) {
	  cat("# Regression line\n\n")
	  texte<-paste("b <- sd(varY[",facteur1,"==\"",niveaux[i],"\"], na.rm=TRUE) / sd(varX[",facteur1,"==\"",niveaux[i],"\"], na.rm=TRUE) * sign(cov(varX[",facteur1,"==\"",niveaux[i],"\"],\n  varY[",facteur1,"==\"",niveaux[i],"\"], use=\"complete.obs\"))\n",sep="")
	  texte<-paste(texte,"a <- mean(varY[",facteur1,"==\"",niveaux[i],"\"], na.rm=TRUE) - b * mean(varX[",facteur1,"==\"",niveaux[i],"\"], na.rm=TRUE)\n",sep="")
	  texte<-paste(texte,"abline(a, b",sep="")
	  if (Env$l.var$couleur2B[i]!="black" & Env$l.var$couleur2B[i]!="#000000") {texte<-paste(texte,", col=\"",Env$l.var$couleur2B[i],"\"",sep="")}
	  texte<-paste(texte,", lty=",type.trait(type=Env$l.var$trait2[i]),sep="")
	  texte<-paste(texte,", lwd=",Env$l.var$epaisseur2[i],sep="")
	  cat(paste(texte,")\n\n",sep=""))
	} else if (Env$l.var$droiteB[i]==Env$voc[147,1]) {
	  cat("# Regression line\n\n")
	  texte<-paste("varX.",i," <- varX[",facteur1,"==\"",niveaux[i],"\"]\n",sep="")
	  texte<-paste(texte,"varY.",i," <- varY[",facteur1,"==\"",niveaux[i],"\"]\n",sep="")
	  texte<-paste(texte,"model.",i," <- lm(varY.",i," ~ varX.",i," + I(varX.",i," ^ 2))\n",sep="")
	  texte<-paste(texte,"varX2.",i," <- seq(min(varX.",i,", na.rm=TRUE), max(varX.",i,", na.rm=TRUE), abs(max(varX.",i,", na.rm=TRUE) - min(varX.",i,", na.rm=TRUE)) / 1000)\n",sep="")
	  texte<-paste(texte,"varY2.",i," <- predict(model.",i,", list(varX.",i," = varX2.",i,"))\n",sep="")
	  texte<-paste(texte,"lines(varX2.",i,", varY2.",i,sep="")
	  if (Env$l.var$couleur2B[i]!="black" & Env$l.var$couleur2B[i]!="#000000") {texte<-paste(texte,", col=\"",Env$l.var$couleur2B[i],"\"",sep="")}
	  texte<-paste(texte,", lty=",type.trait(type=Env$l.var$trait2[i]),sep="")
	  texte<-paste(texte,", lwd=",Env$l.var$epaisseur2[i],sep="")
	  cat(paste(texte,")\n",sep=""))
	  if (Env$l.var$intervalB[i]==Env$voc[261,1]) {
	    texte<-paste("varY2.",i,".conf <- predict(model.",i,", list(varX.",i,"=varX2.",i,"), interval=\"confidence\")\n",sep="")
	    texte<-paste(texte,"lines(varX2.",i,", varY2.",i,".conf[,\"lwr\"], lty=2",sep="")
	    if (Env$l.var$couleur2B[i]!="black" & Env$l.var$couleur2B[i]!="#000000") {texte<-paste(texte,", col=\"",Env$l.var$couleur2B[i],"\"",sep="")}
	    texte<-paste(texte,")\n",sep="")
	    texte<-paste(texte,"lines(varX2.",i,", varY2.",i,".conf[,\"upr\"], lty=2",sep="")
	    if (Env$l.var$couleur2B[i]!="black" & Env$l.var$couleur2B[i]!="#000000") {texte<-paste(texte,", col=\"",Env$l.var$couleur2B[i],"\"",sep="")}
	    texte<-paste(texte,")\n",sep="")
	    cat(texte)
	  }
	  if (Env$l.var$intervalB[i]==Env$voc[262,1]) {
	    texte<-paste("varY2.",i,".pred <- predict(model.",i,", list(varX.",i,"=varX2.",i,"), interval=\"prediction\")\n",sep="")
	    texte<-paste(texte,"lines(varX2.",i,", varY2.",i,".pred[,\"lwr\"], lty=3",sep="")
	    if (Env$l.var$couleur2B[i]!="black" & Env$l.var$couleur2B[i]!="#000000") {texte<-paste(texte,", col=\"",Env$l.var$couleur2B[i],"\"",sep="")}
	    texte<-paste(texte,")\n",sep="")
	    texte<-paste(texte,"lines(varX2.",i,", varY2.",i,".pred[,\"upr\"], lty=3",sep="")
	    if (Env$l.var$couleur2B[i]!="black" & Env$l.var$couleur2B[i]!="#000000") {texte<-paste(texte,", col=\"",Env$l.var$couleur2B[i],"\"",sep="")}
	    texte<-paste(texte,")\n",sep="")
	    cat(texte)
	  }
	  if (Env$l.var$intervalB[i]==Env$voc[263,1]) {
	    texte<-paste("varY2.",i,".conf <- predict(model.",i,", list(varX.",i,"=varX2.",i,"), interval=\"confidence\")\n",sep="")
	    texte<-paste(texte,"varY2.",i,".pred <- predict(model.",i,", list(varX.",i,"=varX2.",i,"), interval=\"prediction\")\n",sep="")
	    texte<-paste(texte,"lines(varX2.",i,", varY2.",i,".conf[,\"lwr\"], lty=2",sep="")
	    if (Env$l.var$couleur2B[i]!="black" & Env$l.var$couleur2B[i]!="#000000") {texte<-paste(texte,", col=\"",Env$l.var$couleur2B[i],"\"",sep="")}
	    texte<-paste(texte,")\n",sep="")
	    texte<-paste(texte,"lines(varX2.",i,", varY2.",i,".conf[,\"upr\"], lty=2",sep="")
	    if (Env$l.var$couleur2B[i]!="black" & Env$l.var$couleur2B[i]!="#000000") {texte<-paste(texte,", col=\"",Env$l.var$couleur2B[i],"\"",sep="")}
	    texte<-paste(texte,")\n",sep="")
	    texte<-paste(texte,"lines(varX2.",i,", varY2.",i,".pred[,\"lwr\"], lty=3",sep="")
	    if (Env$l.var$couleur2B[i]!="black" & Env$l.var$couleur2B[i]!="#000000") {texte<-paste(texte,", col=\"",Env$l.var$couleur2B[i],"\"",sep="")}
	    texte<-paste(texte,")\n",sep="")
	    texte<-paste(texte,"lines(varX2.",i,", varY2.",i,".pred[,\"upr\"], lty=3",sep="")
	    if (Env$l.var$couleur2B[i]!="black" & Env$l.var$couleur2B[i]!="#000000") {texte<-paste(texte,", col=\"",Env$l.var$couleur2B[i],"\"",sep="")}
	    texte<-paste(texte,")\n",sep="")
	    cat(texte)
	  }
	  cat("\n")
	} else if (Env$l.var$droiteB[i]==Env$voc[148,1]) {
	  cat("# Tendency curve\n\n")
	  texte<-paste("panel.smooth(varX[",facteur1,"==\"",niveaux[i],"\"], varY[",facteur1,"==\"",niveaux[i],"\"]",sep="")
	  if (Env$l.var$couleur2B[i]!="black" & Env$l.var$couleur2B[i]!="#000000") {
	    texte<-paste(texte,", col=\"",Env$l.var$couleur2B[i],"\"",sep="")
	    texte<-paste(texte,", col.smooth=\"",Env$l.var$couleur2B[i],"\"",sep="")
	  }
	  texte<-paste(texte,", pch=",symboles[i],sep="")
	  if (Env$l.var$taille.ptsB[i]!=1) {texte<-paste(texte,", cex=",Env$l.var$taille.ptsB[i],sep="")}
	  texte<-paste(texte,", lty=",type.trait(type=Env$l.var$trait2[i]),sep="")
	  texte<-paste(texte,", lwd=",Env$l.var$epaisseur2[i],sep="")
	  cat(paste(texte,")\n\n",sep=""))
	}
    }
    if (tclvalue(Env$l.var$legende)==1) {
	cat("# Legend\n\n")
	texte<-paste("legend(\"",code.graph.posleg(),"\"",sep="")
	texte<-paste(texte,", legend=c(\"",paste(Env$l.var$noms1,collapse="\",\""),"\")",sep="")
	texte<-paste(texte,", col=c(\"",paste(Env$l.var$couleur2B,collapse="\",\""),"\")",sep="")
	texte<-paste(texte,",\n  pch=c(",paste(symboles,collapse=","),")",sep="")
	texte<-paste(texte,", pt.cex=c(",paste(Env$l.var$taille.ptsB,collapse=","),")",sep="")
	if (nchar(tclvalue(Env$l.var$legende.titre))>0) {texte<-paste(texte,", title=\"",tclvalue(Env$l.var$legende.titre),"\"",sep="")}
	cat(paste(texte,")\n\n",sep=""))
    }
  }
}
