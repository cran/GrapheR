graphe.erreurs.tracer <-
function(abscisses,valeurs,erreur.inf,erreur.sup,couleur,amplitude=NULL) {
  if (tclvalue(Env$l.var$erreur)%in%Env$voc[96:98,1]) {
    segments(abscisses,valeurs-erreur.inf,abscisses,valeurs+erreur.sup,col=couleur)
    if (is.null(amplitude)) {
	segments(abscisses-0.1,valeurs-erreur.inf,abscisses+0.1,valeurs-erreur.inf,col=couleur)
	segments(abscisses-0.1,valeurs+erreur.sup,abscisses+0.1,valeurs+erreur.sup,col=couleur)
    } else {
	segments(abscisses-0.015*amplitude,valeurs-erreur.inf,abscisses+0.015*amplitude,valeurs-erreur.inf,col=couleur)
	segments(abscisses-0.015*amplitude,valeurs+erreur.sup,abscisses+0.015*amplitude,valeurs+erreur.sup,col=couleur)
    }
    msg(text="",type="info")
  }
}
