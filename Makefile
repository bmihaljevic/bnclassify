technical: ~/code-papers/tex-includes/macros-paper.tex ~/code-papers/tex-includes/macros-math.tex ~/code-papers/paper-bnclassify-rjournal/background.tex
	cp ~/code-papers/tex-includes/macros-paper.tex  vignettes/  
	cp ~/code-papers/tex-includes/macros-math.tex  vignettes/ 
	cp ~/code-papers/paper-bnclassify-rjournal/background.tex  vignettes/ 

clean-technical:
	cd vignettes; latexmk -c technical.tex
	cd vignettes; latexmk -C technical.tex
	rm -f vignettes/technical-concordance.tex
	rm -f vignettes/technical.bbl
	rm -f vignettes/technical.synctex.gz
	rm -f vignettes/technical.tex

docs:
	cd .. ; rm -f bnclassify.pdf; R CMD Rd2pdf bnclassify; 
	cd .. ; xo  bnclassify.pdf 
