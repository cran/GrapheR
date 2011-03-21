tracer.legende.2 <-
function(nb.niv,ligne,symbol,trait,pos.leg,leg.titre,noms,taille.symb,col.symb,ep.lig) {
  pch=integer(nb.niv)
  lty=integer(nb.niv)
  for (i in 1:nb.niv) {
    pch[i]=if (ligne[i]=="l" | ligne[i]=="h") {NA} else {symbol[i]}
    lty[i]=if (ligne[i]=="p") {NA} else {trait[i]}
  }
  position.legende=if (pos.leg==Env$vocab[126,1]) {"topleft"} else if (pos.leg==Env$vocab[127,1]) {"top"} else if (pos.leg==Env$vocab[128,1]) {"topright"} else
    if (pos.leg==Env$vocab[129,1]) {"left"} else if (pos.leg==Env$vocab[130,1]) {"center"} else if (pos.leg==Env$vocab[131,1]) {"right"} else
    if (pos.leg==Env$vocab[132,1]) {"bottomleft"} else if (pos.leg==Env$vocab[133,1]) {"bottom"} else if (pos.leg==Env$vocab[134,1]) {"bottomright"}
  if (nchar(leg.titre)==0) {
    legend(position.legende,legend=noms,pch=pch,pt.cex=taille.symb,col=col.symb,lty=lty,lwd=ep.lig)
  } else {
    legend(position.legende,legend=noms,pch=pch,pt.cex=taille.symb,col=col.symb,lty=lty,lwd=ep.lig,title=leg.titre)
  }
}

