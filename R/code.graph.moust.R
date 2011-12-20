code.graph.moust <-
function() {
  variable<-tclvalue(Env$l.var$variable)
  facteur1<-tclvalue(Env$l.var$facteur1)
  facteur2<-tclvalue(Env$l.var$facteur2)
  texte<-""
  if (nchar(facteur2)>0 & facteur2!=Env$voc[82,1]) {
    texte<-paste("boxplot(",variable," ~ interaction, axes=FALSE, ann=FALSE",sep="")
  } else {
    texte<-paste("boxplot(",variable," ~ ",facteur1,", axes=FALSE, ann=FALSE",sep="")
  }
  orient<-ifelse (tclvalue(Env$l.var$box.orient)==Env$voc[67,1],"hor","ver")
  log.axes<-if (orient=="ver" & tclvalue(Env$l.var$log.axevaleurs)==1) {
    "y"
  } else if (orient=="hor" & tclvalue(Env$l.var$log.axevaleurs)==1) {
    "x"
  } else {""}
  texte<-paste(texte,", horizontal=",ifelse(orient=="hor","TRUE","FALSE"),sep="")
  if (tclvalue(Env$l.var$couleur1A)!="white" & tclvalue(Env$l.var$couleur1A)!="#ffffff") {texte<-paste(texte,", col=\"",tclvalue(Env$l.var$couleur1A),"\"",sep="")}
  if (tclvalue(Env$l.var$col.borduresA)!="black" & tclvalue(Env$l.var$col.borduresA)!="#000000") {texte<-paste(texte,", boxcol=\"",tclvalue(Env$l.var$col.borduresA),
    "\", medcol=\"",tclvalue(Env$l.var$col.borduresA),"\"",sep="")}
  if (tclvalue(Env$l.var$couleur2A)!="black" & tclvalue(Env$l.var$couleur2A)!="#000000") {texte<-paste(texte,", whiskcol=\"",tclvalue(Env$l.var$couleur2A),
    "\", staplecol=\"",tclvalue(Env$l.var$couleur2A),"\"",sep="")}
  if (tclvalue(Env$l.var$couleur3)!="black" & tclvalue(Env$l.var$couleur3)!="#000000") {texte<-paste(texte,", outcol=\"",tclvalue(Env$l.var$couleur3),"\"",sep="")}
  texte<-paste(texte,",\n  whisklty=",type.trait(type=tclvalue(Env$l.var$trait1)),sep="")
  texte<-paste(texte,", outline=",ifelse(tclvalue(Env$l.var$outliers)=="1","TRUE","FALSE"),sep="")
  texte<-paste(texte,", outpch=",ifelse(tclvalue(Env$l.var$box.symbol)==Env$voc[80,1],16,1),sep="")
  texte<-paste(texte,", range=",as.numeric(tclvalue(Env$l.var$lg.moustaches)),sep="")
  texte<-paste(texte,", notch=",ifelse(tclvalue(Env$l.var$ICmediane)=="1","TRUE","FALSE"),sep="")
  if (log.axes!="") {texte<-paste(texte,", log=",log.axes,sep="")}
  texte<-paste(texte,", ylim=c(",round(Env$l.code$y.inf,2),",",round(Env$l.code$y.sup,2),"))\n\n",sep="")
  cat(texte)
  code.graph.axes()
  code.graph.titre()
  if (tclvalue(Env$l.var$encadre)==1) {cat("box()\n\n")}
}
