copy fond.prg fond_b.prg
type menu.prg >> fond_b.prg
type come.prg >> fond_b.prg
type func.prg >> fond_b.prg
type inp.prg >> fond_b.prg
type print.prg >> fond_b.prg
type arhivs.prg >> fond_b.prg
type tunes.prg >> fond_b.prg
type title.prg >> fond_b.prg
type vars.prg >> fond_b.prg
clipper fond_b -m -q -l
rtlink FI fond_b OUT fond LIB clipper,extend,mcl
del fond_b.*
