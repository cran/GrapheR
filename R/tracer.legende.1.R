tracer.legende.1 <-
function(pos.leg,leg,leg.titre,labels,couleurs) {
  position.legende=if (pos.leg==Env$vocab[126,1]) {"topleft"} else if (pos.leg==Env$vocab[127,1]) {"top"} else if (pos.leg==Env$vocab[128,1]) {"topright"} else
    if (pos.leg==Env$vocab[129,1]) {"left"} else if (pos.leg==Env$vocab[130,1]) {"center"} else if (pos.leg==Env$vocab[131,1]) {"right"} else
    if (pos.leg==Env$vocab[132,1]) {"bottomleft"} else if (pos.leg==Env$vocab[133,1]) {"bottom"} else if (pos.leg==Env$vocab[134,1]) {"bottomright"}
  if (leg==1) {
    if (nchar(leg.titre)==0) {
	legend(position.legende,legend=labels,fill=couleurs)
    } else {
	legend(position.legende,legend=labels,fill=couleurs,title=leg.titre)
    }
  }
}

