aide <-
function() {
  if (Env$lang=="en") {browseURL(file.path(.path.package("GrapheR"),"doc","manual_en.pdf",fsep=.Platform$file.sep))}
  if (Env$lang=="fr") {browseURL(file.path(.path.package("GrapheR"),"doc","manual_fr.pdf",fsep=.Platform$file.sep))}
  if (Env$lang=="es") {browseURL(file.path(.path.package("GrapheR"),"doc","manual_en.pdf",fsep=.Platform$file.sep))}
  if (Env$lang=="de") {browseURL(file.path(.path.package("GrapheR"),"doc","manual_de.pdf",fsep=.Platform$file.sep))}
}
