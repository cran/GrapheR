histogram.type1 <-
function(x.inf,x.sup,distrib.lab,distrib.check,col.distrib.lab,col.distrib,col.distrib.choose,type.distrib.lab,type.distrib.choose,ep.distrib.lab,ep.distrib.choose) {
  tkconfigure(x.inf,state="normal")
  tkconfigure(x.sup,state="normal")
  histogram.distrib(act=FALSE,distrib.lab=distrib.lab,distrib.check=distrib.check,col.distrib.lab=col.distrib.lab,col.distrib=col.distrib,col.distrib.choose=col.distrib.choose,
    type.distrib.lab=type.distrib.lab,type.distrib.choose=type.distrib.choose,ep.distrib.lab=ep.distrib.lab,ep.distrib.choose=ep.distrib.choose)
}

