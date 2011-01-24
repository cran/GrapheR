barres.motifs <-
function(varValue,fact2Value) {
  if (nchar(tclvalue(varValue))>0) {
    if (is.factor(Env$datas.GrapheR[,tclvalue(varValue)])==TRUE) {
      if (nchar(Env$niv.prop)==1) {
        unmotif(dens.motif=Env$dens.motif,ang.motif=Env$ang.motif,col.motif=Env$col.motif)
	} else {
	  plusieursmotifs(nb.niv=Env$nb.niv2,noms=Env$noms2,dens.motif=Env$dens.motif,ang.motif=Env$ang.motif,col.motif=Env$col.motif)
	}
    } else {
      if (nchar(tclvalue(fact2Value))==0 | tclvalue(fact2Value)==Env$vocab[48,1]) {
	  unmotif(dens.motif=Env$dens.motif,ang.motif=Env$ang.motif,col.motif=Env$col.motif)
	} else {
	  plusieursmotifs(nb.niv=Env$nb.niv2,noms=Env$noms2,dens.motif=Env$dens.motif,ang.motif=Env$ang.motif,col.motif=Env$col.motif)
	}
    }
  } else {tkmessageBox(title=Env$vocab[3,1],message=Env$vocab[6,1],icon="error",type="ok")}
}

