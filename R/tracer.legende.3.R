tracer.legende.3 <-
function(pos.leg,leg.titre,noms,symbol,taille.symb,col.symb) {
  position.legende=if (pos.leg==Env$vocab[126,1]) {"topleft"} else if (pos.leg==Env$vocab[127,1]) {"top"} else if (pos.leg==Env$vocab[128,1]) {"topright"} else
    if (pos.leg==Env$vocab[129,1]) {"left"} else if (pos.leg==Env$vocab[130,1]) {"center"} else if (pos.leg==Env$vocab[131,1]) {"right"} else
    if (pos.leg==Env$vocab[132,1]) {"bottomleft"} else if (pos.leg==Env$vocab[133,1]) {"bottom"} else if (pos.leg==Env$vocab[134,1]) {"bottomright"}
  if (nchar(leg.titre)==0) {
    legend(position.legende,legend=noms,pch=symbol,pt.cex=taille.symb,col=col.symb)
  } else {
    legend(position.legende,legend=noms,pch=symbol,pt.cex=taille.symb,col=col.symb,title=leg.titre)
  }
}

