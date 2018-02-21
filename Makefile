docs:
	cd .. ; rm -f bnclassify.pdf; R CMD Rd2pdf bnclassify; 
	cd .. ; xo  bnclassify.pdf 
