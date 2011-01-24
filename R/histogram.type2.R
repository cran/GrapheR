histogram.type2 <-
function(x.inf,x.sup,distrib.lab,distrib.check,col.distrib.lab,col.distrib,col.distrib.choose,type.distrib.lab,type.distrib.choose,ep.distrib.lab,ep.distrib.choose) {
  tkdelete(x.inf,0,"end")
  tkinsert(x.inf,"end","Auto")
  tkconfigure(x.inf,state="disabled")
  tkdelete(x.sup,0,"end")
  tkinsert(x.sup,"end","Auto")
  tkconfigure(x.sup,state="disabled")
  histogram.distrib(act=FALSE,distrib.lab=distrib.lab,distrib.check=distrib.check,col.distrib.lab=col.distrib.lab,col.distrib=col.distrib,col.distrib.choose=col.distrib.choose,
    type.distrib.lab=type.distrib.lab,type.distrib.choose=type.distrib.choose,ep.distrib.lab=ep.distrib.lab,ep.distrib.choose=ep.distrib.choose)
}

