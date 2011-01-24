nuage.symbol <-
function(factValue) {
  if (nchar(tclvalue(factValue))==0 | tclvalue(factValue)==Env$vocab[48,1]) {
    unsymbole(symbol=Env$symb,col=Env$col.str1)
  } else {
    plusieurssymboles(nb.niv=Env$nb.niv1,noms=Env$noms1,symbol=Env$symb,col=Env$col.str1)
  }
}

