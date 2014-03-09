RX_OPTS = 

RTS_OPTS = +RTS -H50M -K10M -RTS

# compiled version
RX = ../src/RX  $(RTS_OPTS)

# interpreted version
# RX = runhugs -h1m -l ../src/RX.hs


###########################################################################

check : 
	$(RX) $(RX_OPTS) basic.lit check.lit

clean : 
	rm -f *~ 

# latex stuff ##############################################################


%.tex : %.lit 
	$(RX) $(RTS_OPTS) $(RX_OPTS) current=text code=latex $< |tee $@

%.out : %.in
	$(RX) $(RTS_OPTS) $(RX_OPTS) current=code code=plain $< |tee $@

%.dvi : %.tex
	- latex "\\nonstopmode\\input $<"

%.ps : %.dvi
	dvips -t a4 $< -o $@

