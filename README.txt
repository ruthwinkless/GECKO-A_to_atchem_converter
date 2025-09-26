This will convert GECKO-A outputs into mcm-style inputs for use in atchem models.
It will only do so at fixed temperature and pressure.

To run, open Runscript.R and follow it's instructions.


I recommend changing a GECKO-A data file before compiling your gecko code:
	mch_inorg.dat replaced with the file of the same name in this folder
	this has slightly different constants for the inorganic Troe (aka FALLOFF) equations, that are more closely based on experimental data (and what the mcm uses)

This is written in R because thats what I know, feel free to use this to make your own in your own favorite language

gecko-a code	: https://gitlab.in2p3.fr/ipsl/lisa/geckoa/public/gecko-a
atchem2			: https://github.com/AtChem/AtChem2
the mcm			: https://mcm.york.ac.uk/MCM
