courbe.vary <-
function(var2Value,erreur.lab,erreur.choose,segment.lab,segment.check,niv.lab,niv.choose,nivValue,typeValue,rb1,rb2,rb3) {
  courbe.erreur.act(act=TRUE,erreur.lab=erreur.lab,erreur.choose=erreur.choose,segment.lab=segment.lab,segment.check=segment.check)
  if (is.factor(Env$datas.GrapheR[,tclvalue(var2Value)])==TRUE) {
    courbe.var2(type="fact",niv.lab=niv.lab,niv.choose=niv.choose,var2Value=var2Value,nivValue=nivValue,typeValue=typeValue,rb1=rb1,rb2=rb2,rb3=rb3)
    courbe.erreur.types(type="prop",erreur.choose=erreur.choose)
  } else {
    courbe.var2(type="num",niv.lab=niv.lab,niv.choose=niv.choose,var2Value=var2Value,nivValue=nivValue,typeValue=typeValue,rb1=rb1,rb2=rb2,rb3=rb3)
    courbe.erreur.types(type="moy",erreur.choose=erreur.choose)
  }
}

