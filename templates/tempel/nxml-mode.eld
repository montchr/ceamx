;;; templates/tempel/nxml-mode.eld

nxml-mode

(phpcsrule & > "<rule ref=\"" r "\"/>")
(phpcsruleconf & > "<rule ref=\" p \">" n> "<properties>" n> "<property name=\"" p "\" value=\"" p "\"/>" q "</properties>" n "</rule>" n)
(phpcsexclude & > "<exclude name=\"" r "\"/>")
