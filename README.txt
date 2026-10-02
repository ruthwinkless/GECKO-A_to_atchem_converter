This will convert GECKO-A outputs into mcm-style inputs for use in atchem models.
It will only do so at fixed temperature and pressure.

To run, open Runscript.R and follow it's instructions.

-------------------------

This script includes the option to treat GECKO-A RO2 + NO reactions in the same way as the mcm (set mcmRO2 = T in Runscript.R).
In GECKO-A, some fragmentation of RO fragments is treated as happening instantly, so a fraction of RO2 + NO products becomes the products of RO fragmentation.
As this code was written in part to try and bring mcm and GECKO-A results in line with each other, an option to ignore this treatment of RO fragmentation was written, this is the code in ROwait.R.

-------------------------

I recommend changing a GECKO-A data file before compiling your gecko code:
	mch_inorg.dat replaced with the file of the same name in this folder
	this has slightly different constants for the inorganic Troe (aka FALLOFF) equations, that are more closely based on experimental data (and what the mcm uses)

-------------------------

This is written in R because thats what I know, feel free to use this to make your own in your own favorite language

gecko-a code	: https://gitlab.in2p3.fr/ipsl/lisa/geckoa/public/gecko-a
atchem2			: https://github.com/AtChem/AtChem2
the mcm			: https://mcm.york.ac.uk/MCM
