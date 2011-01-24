courbe.droite <-
function(factValue) {
  if (nchar(tclvalue(factValue))==0 | tclvalue(factValue)==Env$vocab[48,1]) {
    unecourbe(lig=Env$lignes,tra=Env$traits,ep=Env$ep.lignes)
  } else {
    plusieurscourbes(nb.niv=Env$nb.niv1,noms=Env$noms1,lig=Env$lignes,tra=Env$traits,ep=Env$ep.lignes)
  }
}

