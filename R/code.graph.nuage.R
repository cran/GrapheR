code.graph.nuage <-
function() {
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
    code.graph.axes()
    code.graph.titre()
    if (tclvalue(Env$l.var$encadre)==1) {cat("box()\n\n")}
    if (tclvalue(Env$l.var$droiteA)==Env$voc[145,1]) {
	cat("# Regression line\n\n")
	texte<-""
	if (nchar(facteur1)>0 & facteur1!=Env$voc[82,1]) {
	  texte<-"abline(lm(varY ~ varX)$coefficients"
	} else {
	  texte<-paste("abline(lm(",varY," ~ ",varX,")$coefficients",sep="")
	}
	if (tclvalue(Env$l.var$couleur2A)!="black" & tclvalue(Env$l.var$couleur2A)!="#000000") {texte<-paste(texte,", col=\"",tclvalue(Env$l.var$couleur2A),"\"",sep="")}
	texte<-paste(texte,", lty=",type.trait(type=tclvalue(Env$l.var$trait1)),sep="")
	texte<-paste(texte,", lwd=",as.numeric(tclvalue(Env$l.var$epaisseur1)),sep="")
	cat(paste(texte,")\n\n",sep=""))
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
	cat(paste(texte,")\n\n",sep=""))
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
    code.graph.axes()
    code.graph.titre()
    if (tclvalue(Env$l.var$encadre)==1) {cat("box()\n\n")}
    for (i in 1:length(niveaux)) {
	if (Env$l.var$droiteB[i]==Env$voc[145,1]) {
	  cat("# Regression line\n\n")
	  texte<-paste("abline(lm(varY[",facteur1,"==\"",niveaux[i],"\"] ~ varX[",facteur1,"==\"",niveaux[i],"\"])$coefficients",sep="")
	  if (Env$l.var$couleur2B[i]!="black" & Env$l.var$couleur2B[i]!="#000000") {texte<-paste(texte,", col=\"",Env$l.var$couleur2B[i],"\"",sep="")}
	  texte<-paste(texte,", lty=",type.trait(type=Env$l.var$trait2[i]),sep="")
	  texte<-paste(texte,", lwd=",Env$l.var$epaisseur2[i],sep="")
	  cat(paste(texte,")\n\n",sep=""))
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
	  texte<-paste("varX.temp <- varX[",facteur1,"==\"",niveaux[i],"\"]\n",sep="")
	  texte<-paste(texte,"varY.temp <- varY[",facteur1,"==\"",niveaux[i],"\"]\n",sep="")
	  texte<-paste(texte,"model <- lm(varY.temp ~ varX.temp + I(varX.temp ^ 2))\n",sep="")
	  texte<-paste(texte,"varX2 <- seq(min(varX.temp, na.rm=TRUE), max(varX.temp, na.rm=TRUE), abs(max(varX.temp, na.rm=TRUE) - min(varX.temp, na.rm=TRUE)) / 1000)\n",sep="")
	  texte<-paste(texte,"varY2 <- predict(model, list(varX.temp = varX2))\n",sep="")
	  texte<-paste(texte,"lines(varX2, varY2",sep="")
	  if (Env$l.var$couleur2B[i]!="black" & Env$l.var$couleur2B[i]!="#000000") {texte<-paste(texte,", col=\"",Env$l.var$couleur2B[i],"\"",sep="")}
	  texte<-paste(texte,", lty=",type.trait(type=Env$l.var$trait2[i]),sep="")
	  texte<-paste(texte,", lwd=",Env$l.var$epaisseur2[i],sep="")
	  cat(paste(texte,")\n\n",sep=""))
	} else if (tclvalue(Env$l.var$droiteA)==Env$voc[148,1]) {
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
