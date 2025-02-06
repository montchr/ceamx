js-mode js-ts-mode

(com & "/*" n> " * " p n> " */" n>)
(export & "export { " p " } from '" p "'" q)
(exportd "export default " p)
(exporta & "export { " p " as " p " } from '" p "'" q)
(foreach & p ".forEach(" (p "it") " => {" n r> n> "})" n>)
(fun & "function " p "(" p ") {
  " p "
}" n n)
(importa & "import * as " p " from '" p "'" n)

(comtype "/** @type " p " */" q)
(param "@param {" (p "TYPE")"} " (p "NAME") " - " (p "DESCRIPTION"))
