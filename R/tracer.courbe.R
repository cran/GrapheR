tracer.courbe <-
function(var1,var2,niv1,fact,nb.niv,niv2,type,encadre,titre,col.titre,taille.titre,taille.ax,col.ax,x.legende,y.legende,taille.legende,col.legende,x.inf,x.sup,y.inf,y.sup,x.log,y.log,
  symbol,taille.symb,col.symb,lig,tra,ep.lig,err.type,err.seg,leg,pos.leg,leg.titre,noms,fen) {
  if (nchar(var1)>0 & nchar(var2)>0) {
    na.a=which(is.na(Env$datas.GrapheR[,var1])==TRUE)
    na.b=which(is.na(Env$datas.GrapheR[,var2])==TRUE)
    datas.GrapheR2=if (length(na.a)!=0) {Env$datas.GrapheR[-na.a,]} else {Env$datas.GrapheR}
    datas.GrapheR2=if (length(na.b)!=0) {datas.GrapheR2[-na.b,]} else {datas.GrapheR2}
    nivx=as.numeric(strsplit(niv2,split=" ")[[1]])+1
    log.axes=if (x.log==1) {
      if (y.log==1) {"xy"} else {"x"}
    } else {
      if (y.log==1) {"y"} else {""}
    }
    if (type=="som") {
	if (nchar(fact)==0 | fact==Env$vocab[48,1]) {
	  sommes=tapply(datas.GrapheR2[,var2],datas.GrapheR2[,var1],sum)
	  xinf=if (x.inf=="Auto") {0.8*min(datas.GrapheR2[,var1],na.rm=TRUE)} else {as.numeric(x.inf)}
	  xsup=if (x.sup=="Auto") {1.1*max(datas.GrapheR2[,var1],na.rm=TRUE)} else {as.numeric(x.sup)}
	  yinf=if (y.inf=="Auto") {0.8*min(sommes)} else {as.numeric(y.inf)}
	  ysup=if (y.sup=="Auto") {1.1*max(sommes)} else {as.numeric(y.sup)}
	  ligne=if (lig==Env$vocab[164,1]) {"p"} else if (lig==Env$vocab[165,1]) {"l"} else if (lig==Env$vocab[166,1]) {"b"} else 
	    if (lig==Env$vocab[167,1]) {"o"} else {"h"}
	  trait=if (tra==Env$vocab[75,1]) {1} else if (tra==Env$vocab[76,1]) {2} else {3}
        if (is.numeric(dev.list()[1])==FALSE) {par(bg="white",mar=c(5,6,4,2))}
	  plot(sommes~as.numeric(names(sommes)),main="",xlab=x.legende,ylab=y.legende,xlim=c(xinf,xsup),ylim=c(yinf,ysup),
	    log=log.axes,cex.lab=taille.legende,col.lab=col.legende,xaxs="i",yaxs="i",axes=FALSE,col=col.symb,
	    pch=symbol,cex=taille.symb,type=ligne,lty=trait,lwd=ep.lig)
	  title(main=titre,col.main=col.titre,cex.main=taille.titre)
	  axis(1,col=col.ax,col.axis=col.ax,cex.axis=taille.ax)
	  axis(2,col=col.ax,col.axis=col.ax,cex.axis=taille.ax)
	  if (encadre==1) {box()}
	} else {
	  if (nb.niv==1) {
	    x=datas.GrapheR2[,var1][datas.GrapheR2[,fact]==levels(Env$datas.GrapheR[,fact])[nivx]]
	    y=datas.GrapheR2[,var2][datas.GrapheR2[,fact]==levels(Env$datas.GrapheR[,fact])[nivx]]
	    sommes1=tapply(y,x,sum)
	    xinf=if (x.inf=="Auto") {0.8*min(datas.GrapheR2[,var1])} else {as.numeric(x.inf)}
	    xsup=if (x.sup=="Auto") {1.1*max(datas.GrapheR2[,var1])} else {as.numeric(x.sup)}
	    yinf=if (y.inf=="Auto") {0.8*min(sommes1)} else {as.numeric(y.inf)}
	    ysup=if (y.sup=="Auto") {1.1*max(sommes1)} else {as.numeric(y.sup)}
	    ligne=if (lig==Env$vocab[164,1]) {"p"} else if (lig==Env$vocab[165,1]) {"l"} else if (lig==Env$vocab[166,1]) {"b"} else 
	      if (lig==Env$vocab[167,1]) {"o"} else {"h"}
	    trait=if (tra==Env$vocab[75,1]) {1} else if (tra==Env$vocab[76,1]) {2} else {3}
          if (is.numeric(dev.list()[1])==FALSE) {par(bg="white",mar=c(5,6,4,2))}
	    plot(sommes1~as.numeric(names(sommes1)),main="",xlab=x.legende,ylab=y.legende,xlim=c(xinf,xsup),ylim=c(yinf,ysup),
	      log=log.axes,cex.lab=taille.legende,col.lab=col.legende,xaxs="i",yaxs="i",axes=FALSE,col=col.symb,
	      pch=symbol,cex=taille.symb,type=ligne,lty=trait,lwd=ep.lig)
	  } else {
	    ligne=NULL
	    trait=NULL
	    liste.sommes=list()
	    for(i in 1:nb.niv) {
	      x=datas.GrapheR2[,var1][datas.GrapheR2[,fact]==levels(Env$datas.GrapheR[,fact])[nivx[i]]]
	      y=datas.GrapheR2[,var2][datas.GrapheR2[,fact]==levels(Env$datas.GrapheR[,fact])[nivx[i]]]		
	      liste.sommes[[i]]=tapply(y,x,sum)
	      ligne=c(ligne,if (lig[i]==Env$vocab[164,1]) {"p"} else if (lig[i]==Env$vocab[165,1]) {"l"} else if (lig[i]==Env$vocab[166,1]) {"b"} else 
	        if (lig[i]==Env$vocab[167,1]) {"o"} else {"h"})
		trait=c(trait,if (tra[i]==Env$vocab[75,1]) {1} else if (tra[i]==Env$vocab[76,1]) {2} else {3})
	    }
	    max=NULL;min=NULL
	    for (i in 1:length(liste.sommes)) {
		max=c(max,max(liste.sommes[[i]]))
		min=c(min,min(liste.sommes[[i]]))
	    }
	    xinf=if (x.inf=="Auto") {0.8*min(datas.GrapheR2[,var1])} else {as.numeric(x.inf)}
	    xsup=if (x.sup=="Auto") {1.1*max(datas.GrapheR2[,var1])} else {as.numeric(x.sup)}
	    yinf=if (y.inf=="Auto") {0.8*min(min)} else {as.numeric(y.inf)}
	    ysup=if (y.sup=="Auto") {1.1*max(max)} else {as.numeric(y.sup)}
          if (is.numeric(dev.list()[1])==FALSE) {par(bg="white",mar=c(5,6,4,2))}
	    plot(liste.sommes[[1]]~as.numeric(names(liste.sommes[[1]])),main="",xlab=x.legende,ylab=y.legende,xlim=c(xinf,xsup),ylim=c(yinf,ysup),
	      log=log.axes,cex.lab=taille.legende,col.lab=col.legende,xaxs="i",yaxs="i",axes=FALSE,col=col.symb[1],
	      pch=symbol[1],cex=taille.symb,type=ligne[1],lty=trait[1],lwd=ep.lig[1])
	    for (i in 2:nb.niv) {
		par(new=TRUE)
	      plot(liste.sommes[[i]]~as.numeric(names(liste.sommes[[i]])),main="",xlab="",ylab="",xlim=c(xinf,xsup),ylim=c(yinf,ysup),
	        log=log.axes,xaxs="i",yaxs="i",axes=FALSE,col=col.symb[i],pch=symbol[i],cex=taille.symb,type=ligne[i],lty=trait[i],lwd=ep.lig[i])
	    }
	  }
	  title(main=titre,col.main=col.titre,cex.main=taille.titre)
	  axis(1,col=col.ax,col.axis=col.ax,cex.axis=taille.ax)
	  axis(2,col=col.ax,col.axis=col.ax,cex.axis=taille.ax)
	  if (encadre==1) {box()}
	  if (leg==1) {
	    pch=NULL;lty=NULL
	    for (i in 1:nb.niv) {
		pch=c(pch,if (ligne[i]=="l" | ligne[i]=="h") {NA} else {symbol[i]})
		lty=c(lty,if (ligne[i]=="p") {NA} else {trait[i]})
	    }
	    position.legende=if (pos.leg==Env$vocab[126,1]) {"topleft"} else if (pos.leg==Env$vocab[127,1]) {"top"} else if (pos.leg==Env$vocab[128,1]) {"topright"} else
	      if (pos.leg==Env$vocab[129,1]) {"left"} else if (pos.leg==Env$vocab[130,1]) {"center"} else if (pos.leg==Env$vocab[131,1]) {"right"} else
	      if (pos.leg==Env$vocab[132,1]) {"bottomleft"} else if (pos.leg==Env$vocab[133,1]) {"bottom"} else if (pos.leg==Env$vocab[134,1]) {"bottomright"}
	    if (nchar(leg.titre)==0) {legend(position.legende,legend=noms,pch=pch,pt.cex=taille.symb,col=col.symb,lty=lty,lwd=ep.lig)} else {
	      legend(position.legende,legend=noms,pch=pch,pt.cex=taille.symb,col=col.symb,lty=lty,lwd=ep.lig,title=leg.titre)}
	  }
	}
    } else if (type=="moy") {
	if (nchar(fact)==0 | fact==Env$vocab[48,1]) {
	  moyennes=tapply(datas.GrapheR2[,var2],datas.GrapheR2[,var1],mean)
	  barres.inf=NULL
	  barres.sup=NULL
	  if (err.type==Env$vocab[119,1]) {
	    barres.inf=tapply(datas.GrapheR2[,var2],datas.GrapheR2[,var1],sd)
	    barres.inf[which(is.na(barres.inf)==TRUE)]=0
	    barres.sup=barres.inf
	  } else if (err.type==Env$vocab[120,1]) {
	    barres.inf=tapply(datas.GrapheR2[,var2],datas.GrapheR2[,var1],function(x) sd(x)/sqrt(length(x)))
	    barres.inf[which(is.na(barres.inf)==TRUE)]=0
	    barres.sup=barres.inf
	  } else if (err.type==Env$vocab[121,1]) {
	    if (any(tapply(datas.GrapheR2[,var2],datas.GrapheR2[,var1],length)<30)==TRUE) {
		barres.inf=tapply(datas.GrapheR2[,var2],datas.GrapheR2[,var1],function(x) mean(x)-wilcox.test(x,conf.int=TRUE)$conf.int[1])
		barres.sup=tapply(datas.GrapheR2[,var2],datas.GrapheR2[,var1],function(x) wilcox.test(x,conf.int=TRUE)$conf.int[2]-mean(x))
	    } else {
		barres.inf=tapply(datas.GrapheR2[,var2],datas.GrapheR2[,var1],function(x) mean(x)-t.test(x)$conf.int[1])
		barres.sup=barres.inf
	    }
	  } else {barres.inf=rep(0,length(moyennes));barres.sup=barres.inf}
	  xinf=if (x.inf=="Auto") {0.8*min(datas.GrapheR2[,var1],na.rm=TRUE)} else {as.numeric(x.inf)}
	  xsup=if (x.sup=="Auto") {1.1*max(datas.GrapheR2[,var1],na.rm=TRUE)} else {as.numeric(x.sup)}
	  yinf=if (y.inf=="Auto") {0.8*min(moyennes-barres.inf)} else {as.numeric(y.inf)}
	  ysup=if (y.sup=="Auto") {1.1*max(moyennes+barres.sup)} else {as.numeric(y.sup)}
	  ligne=if (lig==Env$vocab[164,1]) {"p"} else if (lig==Env$vocab[165,1]) {"l"} else if (lig==Env$vocab[166,1]) {"b"} else 
	    if (lig==Env$vocab[167,1]) {"o"} else {"h"}
	  trait=if (tra==Env$vocab[75,1]) {1} else if (tra==Env$vocab[76,1]) {2} else {3}
        if (is.numeric(dev.list()[1])==FALSE) {par(bg="white",mar=c(5,6,4,2))}
	  plot(moyennes~as.numeric(names(moyennes)),main="",xlab=x.legende,ylab=y.legende,xlim=c(xinf,xsup),ylim=c(yinf,ysup),
	    log=log.axes,cex.lab=taille.legende,col.lab=col.legende,xaxs="i",yaxs="i",axes=FALSE,col=col.symb,
	    pch=symbol,cex=taille.symb,type=ligne,lty=trait,lwd=ep.lig)
	  title(main=titre,col.main=col.titre,cex.main=taille.titre)
	  axis(1,col=col.ax,col.axis=col.ax,cex.axis=taille.ax)
	  axis(2,col=col.ax,col.axis=col.ax,cex.axis=taille.ax)
	  if (encadre==1) {box()}
	  if (nchar(err.type)>0 & err.type!=Env$vocab[118,1]) {
	    for (i in 1:length(moyennes)) {
		if (barres.inf[i]!=0 | barres.sup[i]!=0) {segments(as.numeric(names(moyennes)[i]),moyennes[i]-barres.inf[i],as.numeric(names(moyennes)[i]),moyennes[i]+barres.sup[i],col=col.symb,lty=trait)}
	      if (err.seg==1) {
		  if (barres.inf[i]!=0) {segments(as.numeric(names(moyennes)[i])-0.02*(xsup-xinf),moyennes[i]-barres.inf[i],as.numeric(names(moyennes)[i])+0.02*(xsup-xinf),moyennes[i]-barres.inf[i],col=col.symb)}
		  if (barres.sup[i]!=0) {segments(as.numeric(names(moyennes)[i])-0.02*(xsup-xinf),moyennes[i]+barres.sup[i],as.numeric(names(moyennes)[i])+0.02*(xsup-xinf),moyennes[i]+barres.sup[i],col=col.symb)}
		}
	    }
	  }
	} else {
	  ligne=NULL
	  trait=NULL
	  if (nb.niv==1) {
	    x=datas.GrapheR2[,var1][datas.GrapheR2[,fact]==levels(Env$datas.GrapheR[,fact])[nivx]]
	    y=datas.GrapheR2[,var2][datas.GrapheR2[,fact]==levels(Env$datas.GrapheR[,fact])[nivx]]
	    moyennes=tapply(y,x,mean)
	    barres.inf=NULL
	    barres.sup=NULL
	    if (err.type==Env$vocab[119,1]) {
	      barres.inf=tapply(y,x,sd)
	      barres.inf[which(is.na(barres.inf)==TRUE)]=0
	      barres.sup=barres.inf
	    } else if (err.type==Env$vocab[120,1]) {
	      barres.inf=tapply(y,x,function(x) sd(x)/sqrt(length(x)))
	      barres.inf[which(is.na(barres.inf)==TRUE)]=0
	      barres.sup=barres.inf
	    } else if (err.type==Env$vocab[121,1]) {
	      if (any(tapply(y,x,length)<30)==TRUE) {
		  barres.inf=tapply(y,x,function(x) mean(x)-wilcox.test(x,conf.int=TRUE)$conf.int[1])
		  barres.sup=tapply(y,x,function(x) wilcox.test(x,conf.int=TRUE)$conf.int[2]-mean(x))
	      } else {
		  barres.inf=tapply(y,x,function(x) mean(x)-t.test(x)$conf.int[1])
		  barres.sup=barres.inf
	      }
	    } else {barres.inf=rep(0,length(moyennes));barres.sup=barres.inf}
	    xinf=if (x.inf=="Auto") {0.8*min(datas.GrapheR2[,var1])} else {as.numeric(x.inf)}
	    xsup=if (x.sup=="Auto") {1.1*max(datas.GrapheR2[,var1])} else {as.numeric(x.sup)}
	    yinf=if (y.inf=="Auto") {0.8*min(moyennes-barres.inf)} else {as.numeric(y.inf)}
	    ysup=if (y.sup=="Auto") {1.1*max(moyennes+barres.sup)} else {as.numeric(y.sup)}
	    ligne=if (lig==Env$vocab[164,1]) {"p"} else if (lig==Env$vocab[165,1]) {"l"} else if (lig==Env$vocab[166,1]) {"b"} else 
	      if (lig==Env$vocab[167,1]) {"o"} else {"h"}
	    trait=if (tra==Env$vocab[75,1]) {1} else if (tra==Env$vocab[76,1]) {2} else {3}
          if (is.numeric(dev.list()[1])==FALSE) {par(bg="white",mar=c(5,6,4,2))}
	    plot(moyennes~as.numeric(names(moyennes)),main="",xlab=x.legende,ylab=y.legende,xlim=c(xinf,xsup),ylim=c(yinf,ysup),
	      log=log.axes,cex.lab=taille.legende,col.lab=col.legende,xaxs="i",yaxs="i",axes=FALSE,col=col.symb,
	      pch=symbol,cex=taille.symb,type=ligne,lty=trait,lwd=ep.lig)
	    if (nchar(err.type)>0 & err.type!=Env$vocab[118,1]) {
	      for (i in 1:length(moyennes)) {
		  if (barres.inf[i]!=0 | barres.sup[i]!=0) {segments(as.numeric(names(moyennes)[i]),moyennes[i]-barres.inf[i],as.numeric(names(moyennes)[i]),moyennes[i]+barres.sup[i],col=col.symb,lty=trait)}
	        if (err.seg==1) {
		    if (barres.inf[i]!=0) {segments(as.numeric(names(moyennes)[i])-0.02*(xsup-xinf),moyennes[i]-barres.inf[i],as.numeric(names(moyennes)[i])+0.02*(xsup-xinf),moyennes[i]-barres.inf[i],col=col.symb)}
		    if (barres.sup[i]!=0) {segments(as.numeric(names(moyennes)[i])-0.02*(xsup-xinf),moyennes[i]+barres.sup[i],as.numeric(names(moyennes)[i])+0.02*(xsup-xinf),moyennes[i]+barres.sup[i],col=col.symb)}
		  }
	      }
	    }
	  } else {
	    ligne=NULL
	    trait=NULL
	    liste.moyennes=list()
	    liste.barres.inf=list()
	    liste.barres.sup=list()
	    for(i in 1:nb.niv) {
	      x2=datas.GrapheR2[,var1][datas.GrapheR2[,fact]==levels(Env$datas.GrapheR[,fact])[nivx[i]]]
	      y2=datas.GrapheR2[,var2][datas.GrapheR2[,fact]==levels(Env$datas.GrapheR[,fact])[nivx[i]]]		
	      liste.moyennes[[i]]=tapply(y2,x2,mean)
	      if (err.type==Env$vocab[119,1]) {
	        liste.barres.inf[[i]]=tapply(y2,x2,sd)
	        liste.barres.inf[[i]][which(is.na(liste.barres.inf[[i]])==TRUE)]=0
	        liste.barres.sup[[i]]=liste.barres.inf[[i]]
	      } else if (err.type==Env$vocab[120,1]) {
	        liste.barres.inf[[i]]=tapply(y2,x2,function(x) sd(x)/sqrt(length(x)))
	        liste.barres.inf[[i]][which(is.na(liste.barres.inf[[i]])==TRUE)]=0
	        liste.barres.sup[[i]]=liste.barres.inf[[i]]
	      } else if (err.type==Env$vocab[120,1]) {
	        if (any(tapply(y2,x2,length)<30)==TRUE) {
		    liste.barres.inf[[i]]=tapply(y2,x2,function(x) mean(x)-wilcox.test(x,conf.int=TRUE)$conf.int[1])
		    liste.barres.sup[[i]]=tapply(y2,x2,function(x) wilcox.test(x,conf.int=TRUE)$conf.int[2]-mean(x))
	        } else {
		    liste.barres.inf[[i]]=tapply(y2,x2,function(x) mean(x)-t.test(x)$conf.int[1])
		    liste.barres.sup[[i]]=liste.barres.inf[[i]]
	        }
	      } else {liste.barres.inf[[i]]=rep(0,length(liste.moyennes[[i]]));liste.barres.sup[[i]]=liste.barres.inf[[i]]}
	      ligne=c(ligne,if (lig[i]==Env$vocab[164,1]) {"p"} else if (lig[i]==Env$vocab[165,1]) {"l"} else if (lig[i]==Env$vocab[166,1]) {"b"} else 
	        if (lig[i]==Env$vocab[167,1]) {"o"} else {"h"})
		trait=c(trait,if (tra[i]==Env$vocab[75,1]) {1} else if (tra[i]==Env$vocab[76,1]) {2} else {3})
	    }
	    max=NULL;min=NULL
	    for (i in 1:length(liste.moyennes)) {
		max=c(max,max(liste.moyennes[[i]]+liste.barres.sup[[i]]))
		min=c(min,min(liste.moyennes[[i]]-liste.barres.inf[[i]]))
	    }
	    xinf=if (x.inf=="Auto") {0.8*min(datas.GrapheR2[,var1])} else {as.numeric(x.inf)}
	    xsup=if (x.sup=="Auto") {1.1*max(datas.GrapheR2[,var1])} else {as.numeric(x.sup)}
	    yinf=if (y.inf=="Auto") {0.8*min(min)} else {as.numeric(y.inf)}
	    ysup=if (y.sup=="Auto") {1.1*max(max)} else {as.numeric(y.sup)}
          if (is.numeric(dev.list()[1])==FALSE) {par(bg="white",mar=c(5,6,4,2))}
	    plot(liste.moyennes[[1]]~as.numeric(names(liste.moyennes[[1]])),main="",xlab=x.legende,ylab=y.legende,xlim=c(xinf,xsup),ylim=c(yinf,ysup),
	      log=log.axes,cex.lab=taille.legende,col.lab=col.legende,xaxs="i",yaxs="i",axes=FALSE,col=col.symb[1],
	      pch=symbol[1],cex=taille.symb,type=ligne[1],lty=trait[1],lwd=ep.lig[1])
	    for (i in 2:nb.niv) {
		par(new=TRUE)
	      plot(liste.moyennes[[i]]~as.numeric(names(liste.moyennes[[i]])),main="",xlab="",ylab="",xlim=c(xinf,xsup),ylim=c(yinf,ysup),
	        log=log.axes,xaxs="i",yaxs="i",axes=FALSE,col=col.symb[i],pch=symbol[i],cex=taille.symb,type=ligne[i],lty=trait[i],lwd=ep.lig[i])
	    }
    	    if (nchar(err.type)>0 & err.type!=Env$vocab[118,1]) {
		for (i in 1:length(liste.moyennes)) {
		  for (j in 1:length(liste.moyennes[[i]])) {
		    if (liste.barres.inf[[i]][j]!=0 | liste.barres.sup[[i]][j]!=0) {segments(as.numeric(names(liste.moyennes[[i]])[j]),liste.moyennes[[i]][j]-liste.barres.inf[[i]][j],as.numeric(names(liste.moyennes[[i]])[j]),liste.moyennes[[i]][j]+liste.barres.sup[[i]][j],col=col.symb[i],lty=trait[i])}
	          if (err.seg==1) {
		      if (liste.barres.inf[[i]][j]!=0) {segments(as.numeric(names(liste.moyennes[[i]])[j])-0.02*(xsup-xinf),liste.moyennes[[i]][j]-liste.barres.inf[[i]][j],as.numeric(names(liste.moyennes[[i]])[j])+0.02*(xsup-xinf),liste.moyennes[[i]][j]-liste.barres.inf[[i]][j],col=col.symb[i])}
		      if (liste.barres.sup[[i]][j]!=0) {segments(as.numeric(names(liste.moyennes[[i]])[j])-0.02*(xsup-xinf),liste.moyennes[[i]][j]+liste.barres.sup[[i]][j],as.numeric(names(liste.moyennes[[i]])[j])+0.02*(xsup-xinf),liste.moyennes[[i]][j]+liste.barres.sup[[i]][j],col=col.symb[i])}
		    }
		  }
		}
	    }
	  }
	  title(main=titre,col.main=col.titre,cex.main=taille.titre)
	  axis(1,col=col.ax,col.axis=col.ax,cex.axis=taille.ax)
	  axis(2,col=col.ax,col.axis=col.ax,cex.axis=taille.ax)
	  if (encadre==1) {box()}
	  if (leg==1) {
	    pch=NULL;lty=NULL
	    for (i in 1:nb.niv) {
		pch=c(pch,if (ligne[i]=="l" | ligne[i]=="h") {NA} else {symbol[i]})
		lty=c(lty,if (ligne[i]=="p") {NA} else {trait[i]})
	    }
	    position.legende=if (pos.leg==Env$vocab[126,1]) {"topleft"} else if (pos.leg==Env$vocab[127,1]) {"top"} else if (pos.leg==Env$vocab[128,1]) {"topright"} else
	      if (pos.leg==Env$vocab[129,1]) {"left"} else if (pos.leg==Env$vocab[130,1]) {"center"} else if (pos.leg==Env$vocab[131,1]) {"right"} else
	      if (pos.leg==Env$vocab[132,1]) {"bottomleft"} else if (pos.leg==Env$vocab[133,1]) {"bottom"} else if (pos.leg==Env$vocab[134,1]) {"bottomright"}
	    if (nchar(leg.titre)==0) {legend(position.legende,legend=noms,pch=pch,pt.cex=taille.symb,col=col.symb,lty=lty,lwd=ep.lig)} else {
	      legend(position.legende,legend=noms,pch=pch,pt.cex=taille.symb,col=col.symb,lty=lty,lwd=ep.lig,title=leg.titre)}
	  }
	}
    } else if (type=="prop"){
	if (nchar(fact)==0 | fact==Env$vocab[48,1]) {
	  x=datas.GrapheR2[,var1]
	  y=datas.GrapheR2[,var2]
	  proportions=tapply(y,x,function(x) length(x[x==niv1])/length(x))
	  barres.inf=NULL
	  barres.sup=NULL
	  if (err.type==Env$vocab[121,1]) {
		barres.inf=tapply(y,x,function(x) length(x[x==niv1])/length(x)-binom.test(length(x[x==niv1]),length(x))$conf.int[1])
		barres.sup=tapply(y,x,function(x) binom.test(length(x[x==niv1]),length(x))$conf.int[2]-length(x[x==niv1])/length(x))
	  } else {barres.inf=rep(0,length(proportions));barres.sup=barres.inf}
	  xinf=if (x.inf=="Auto") {0.8*min(x,na.rm=TRUE)} else {as.numeric(x.inf)}
	  xsup=if (x.sup=="Auto") {1.1*max(x,na.rm=TRUE)} else {as.numeric(x.sup)}
	  yinf=if (y.inf=="Auto") {0} else {as.numeric(y.inf)}
	  ysup=if (y.sup=="Auto") {1.1} else {as.numeric(y.sup)}
	  ligne=if (lig==Env$vocab[164,1]) {"p"} else if (lig==Env$vocab[165,1]) {"l"} else if (lig==Env$vocab[166,1]) {"b"} else 
	    if (lig==Env$vocab[167,1]) {"o"} else {"h"}
	  trait=if (tra==Env$vocab[75,1]) {1} else if (tra==Env$vocab[76,1]) {2} else {3}
        if (is.numeric(dev.list()[1])==FALSE) {par(bg="white",mar=c(5,6,4,2))}
	  plot(proportions~as.numeric(names(proportions)),main="",xlab=x.legende,ylab=y.legende,xlim=c(xinf,xsup),ylim=c(yinf,ysup),
	    log=log.axes,cex.lab=taille.legende,col.lab=col.legende,xaxs="i",yaxs="i",axes=FALSE,col=col.symb,
	    pch=symbol,cex=as.numeric(taille.symb),type=ligne,lty=trait,lwd=ep.lig)
	  title(main=titre,col.main=col.titre,cex.main=taille.titre)
	  axis(1,col=col.ax,col.axis=col.ax,cex.axis=taille.ax)
	  axis(2,col=col.ax,col.axis=col.ax,cex.axis=taille.ax)
	  if (encadre==1) {box()}
	  if (nchar(err.type)>0 & err.type!=Env$vocab[118,1]) {
	    for (i in 1:length(proportions)) {
		if (barres.inf[i]!=0 | barres.sup[i]!=0) {segments(as.numeric(names(proportions)[i]),proportions[i]-barres.inf[i],as.numeric(names(proportions)[i]),proportions[i]+barres.sup[i],col=col.symb,lty=trait)}
	      if (err.seg==1) {
		  if (barres.inf[i]!=0) {segments(as.numeric(names(proportions)[i])-0.02*(xsup-xinf),proportions[i]-barres.inf[i],as.numeric(names(proportions)[i])+0.02*(xsup-xinf),proportions[i]-barres.inf[i],col=col.symb)}
		  if (barres.sup[i]!=0) {segments(as.numeric(names(proportions)[i])-0.02*(xsup-xinf),proportions[i]+barres.sup[i],as.numeric(names(proportions)[i])+0.02*(xsup-xinf),proportions[i]+barres.sup[i],col=col.symb)}
		}
	    }
	  }
	} else {
	  ligne=NULL
	  trait=NULL
	  if (nb.niv==1) {
	    x=datas.GrapheR2[,var1][datas.GrapheR2[,fact]==levels(Env$datas.GrapheR[,fact])[nivx]]
	    y=datas.GrapheR2[,var2][datas.GrapheR2[,fact]==levels(Env$datas.GrapheR[,fact])[nivx]]
	    proportions=tapply(y,x,function(x) length(x[x==niv1])/length(x))
	    barres.inf=NULL
	    barres.sup=NULL
	  if (err.type==Env$vocab[121,1]) {
		barres.inf=tapply(y,x,function(x) length(x[x==niv1])/length(x)-binom.test(length(x[x==niv1]),length(x))$conf.int[1])
		barres.sup=tapply(y,x,function(x) binom.test(length(x[x==niv1]),length(x))$conf.int[2]-length(x[x==niv1])/length(x))
	  } else {barres.inf=rep(0,length(proportions));barres.sup=barres.inf}
	    xinf=if (x.inf=="Auto") {0.8*min(datas.GrapheR2[,var1])} else {as.numeric(x.inf)}
	    xsup=if (x.sup=="Auto") {1.1*max(datas.GrapheR2[,var1])} else {as.numeric(x.sup)}
	    yinf=if (y.inf=="Auto") {0} else {as.numeric(y.inf)}
	    ysup=if (y.sup=="Auto") {1.1} else {as.numeric(y.sup)}
	    ligne=if (lig==Env$vocab[164,1]) {"p"} else if (lig==Env$vocab[165,1]) {"l"} else if (lig==Env$vocab[166,1]) {"b"} else 
	      if (lig==Env$vocab[167,1]) {"o"} else {"h"}
	    trait=if (tra==Env$vocab[75,1]) {1} else if (tra==Env$vocab[76,1]) {2} else {3}
          if (is.numeric(dev.list()[1])==FALSE) {par(bg="white",mar=c(5,6,4,2))}
	    plot(proportions~as.numeric(names(proportions)),main="",xlab=x.legende,ylab=y.legende,xlim=c(xinf,xsup),ylim=c(yinf,ysup),
	      log=log.axes,cex.lab=taille.legende,col.lab=col.legende,xaxs="i",yaxs="i",axes=FALSE,col=col.symb,
	      pch=symbol,cex=taille.symb,type=ligne,lty=trait,lwd=ep.lig)
	    if (nchar(err.type)>0 & err.type!=Env$vocab[118,1]) {
	      for (i in 1:length(proportions)) {
		  if (barres.inf[i]!=0 | barres.sup[i]!=0) {segments(as.numeric(names(proportions)[i]),proportions[i]-barres.inf[i],as.numeric(names(proportions)[i]),proportions[i]+barres.sup[i],col=col.symb,lty=trait)}
	        if (err.seg==1) {
		    if (barres.inf[i]!=0) {segments(as.numeric(names(proportions)[i])-0.02*(xsup-xinf),proportions[i]-barres.inf[i],as.numeric(names(proportions)[i])+0.02*(xsup-xinf),proportions[i]-barres.inf[i],col=col.symb)}
		    if (barres.sup[i]!=0) {segments(as.numeric(names(proportions)[i])-0.02*(xsup-xinf),proportions[i]+barres.sup[i],as.numeric(names(proportions)[i])+0.02*(xsup-xinf),proportions[i]+barres.sup[i],col=col.symb)}
		  }
	      }
	    }
	  } else {
	    ligne=NULL
	    trait=NULL
	    liste.proportions=list()
	    liste.barres.inf=list()
	    liste.barres.sup=list()
	    for(i in 1:nb.niv) {
	      x2=datas.GrapheR2[,var1][datas.GrapheR2[,fact]==levels(Env$datas.GrapheR[,fact])[nivx[i]]]
	      y2=datas.GrapheR2[,var2][datas.GrapheR2[,fact]==levels(Env$datas.GrapheR[,fact])[nivx[i]]]		
	      liste.proportions[[i]]=tapply(y2,x2,function(x) length(x[x==niv1])/length(x))
	      if (err.type==Env$vocab[121,1]) {
		  liste.barres.inf[[i]]=tapply(y2,x2,function(x) length(x[x==niv1])/length(x)-binom.test(length(x[x==niv1]),length(x))$conf.int[1])
		  liste.barres.sup[[i]]=tapply(y2,x2,function(x) binom.test(length(x[x==niv1]),length(x))$conf.int[2]-length(x[x==niv1])/length(x))
	      } else {liste.barres.inf[[i]]=rep(0,length(liste.proportions[[i]]));liste.barres.sup[[i]]=liste.barres.inf[[i]]}
	      ligne=c(ligne,if (lig[i]==Env$vocab[164,1]) {"p"} else if (lig[i]==Env$vocab[165,1]) {"l"} else if (lig[i]==Env$vocab[166,1]) {"b"} else 
	        if (lig[i]==Env$vocab[167,1]) {"o"} else {"h"})
		trait=c(trait,if (tra[i]==Env$vocab[75,1]) {1} else if (tra[i]==Env$vocab[76,1]) {2} else {3})
	    }
	    max=NULL;min=NULL
	    for (i in 1:length(liste.proportions)) {
		max=c(max,max(liste.proportions[[i]]+liste.barres.sup[[i]]))
		min=c(min,min(liste.proportions[[i]]-liste.barres.inf[[i]]))
	    }
	    xinf=if (x.inf=="Auto") {0.8*min(datas.GrapheR2[,var1])} else {as.numeric(x.inf)}
	    xsup=if (x.sup=="Auto") {1.1*max(datas.GrapheR2[,var1])} else {as.numeric(x.sup)}
	    yinf=if (y.inf=="Auto") {0} else {as.numeric(y.inf)}
	    ysup=if (y.sup=="Auto") {1.1} else {as.numeric(y.sup)}
          if (is.numeric(dev.list()[1])==FALSE) {par(bg="white",mar=c(5,6,4,2))}
	    plot(liste.proportions[[1]]~as.numeric(names(liste.proportions[[1]])),main="",xlab=x.legende,ylab=y.legende,xlim=c(xinf,xsup),ylim=c(yinf,ysup),
	      log=log.axes,cex.lab=taille.legende,col.lab=col.legende,xaxs="i",yaxs="i",axes=FALSE,col=col.symb[1],
	      pch=symbol[1],cex=taille.symb,type=ligne[1],lty=trait[1],lwd=ep.lig[1])
	    for (i in 2:nb.niv) {
		par(new=TRUE)
	      plot(liste.proportions[[i]]~as.numeric(names(liste.proportions[[i]])),main="",xlab="",ylab="",xlim=c(xinf,xsup),ylim=c(yinf,ysup),
	        log=log.axes,xaxs="i",yaxs="i",axes=FALSE,col=col.symb[i],pch=symbol[i],cex=taille.symb,type=ligne[i],lty=trait[i],lwd=ep.lig[i])
	    }
    	    if (nchar(err.type)>0 & err.type!=Env$vocab[118,1]) {
		for (i in 1:length(liste.proportions)) {
		  for (j in 1:length(liste.proportions[[i]])) {
		    if (liste.barres.inf[[i]][j]!=0 | liste.barres.sup[[i]][j]!=0) {segments(as.numeric(names(liste.proportions[[i]])[j]),liste.proportions[[i]][j]-liste.barres.inf[[i]][j],as.numeric(names(liste.proportions[[i]])[j]),liste.proportions[[i]][j]+liste.barres.sup[[i]][j],col=col.symb[i],lty=trait[i])}
	          if (err.seg==1) {
		      if (liste.barres.inf[[i]][j]!=0) {segments(as.numeric(names(liste.proportions[[i]])[j])-0.02*(xsup-xinf),liste.proportions[[i]][j]-liste.barres.inf[[i]][j],as.numeric(names(liste.proportions[[i]])[j])+0.02*(xsup-xinf),liste.proportions[[i]][j]-liste.barres.inf[[i]][j],col=col.symb[i])}
		      if (liste.barres.sup[[i]][j]!=0) {segments(as.numeric(names(liste.proportions[[i]])[j])-0.02*(xsup-xinf),liste.proportions[[i]][j]+liste.barres.sup[[i]][j],as.numeric(names(liste.proportions[[i]])[j])+0.02*(xsup-xinf),liste.proportions[[i]][j]+liste.barres.sup[[i]][j],col=col.symb[i])}
		    }
		  }
		}
	    }
	  }
	  title(main=titre,col.main=col.titre,cex.main=taille.titre)
	  axis(1,col=col.ax,col.axis=col.ax,cex.axis=taille.ax)
	  axis(2,col=col.ax,col.axis=col.ax,cex.axis=taille.ax)
	  if (encadre==1) {box()}
	  if (leg==1) {
	    pch=NULL;lty=NULL
	    for (i in 1:nb.niv) {
		pch=c(pch,if (ligne[i]=="l" | ligne[i]=="h") {NA} else {symbol[i]})
		lty=c(lty,if (ligne[i]=="p") {NA} else {trait[i]})
	    }
	    position.legende=if (pos.leg==Env$vocab[126,1]) {"topleft"} else if (pos.leg==Env$vocab[127,1]) {"top"} else if (pos.leg==Env$vocab[128,1]) {"topright"} else
	      if (pos.leg==Env$vocab[129,1]) {"left"} else if (pos.leg==Env$vocab[130,1]) {"center"} else if (pos.leg==Env$vocab[131,1]) {"right"} else
	      if (pos.leg==Env$vocab[132,1]) {"bottomleft"} else if (pos.leg==Env$vocab[133,1]) {"bottom"} else if (pos.leg==Env$vocab[134,1]) {"bottomright"}
	    if (nchar(leg.titre)==0) {legend(position.legende,legend=noms,pch=pch,pt.cex=taille.symb,col=col.symb,lty=lty,lwd=ep.lig)} else {
	      legend(position.legende,legend=noms,pch=pch,pt.cex=taille.symb,col=col.symb,lty=lty,lwd=ep.lig,title=leg.titre)}
	  }
	}
    }
    if (Env$toolbar.GrapheR==1) {tkdestroy(Env$Toolbar)}
    tkgrab.release(fen)
    toolbar(type="courbe",type.hist="",sequence="",log.ax=log.axes,ht="",abs="")
  } else {tkmessageBox(title=Env$vocab[3,1],message=Env$vocab[157,1],icon="error",type="ok")}
}

