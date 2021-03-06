printl: 
	$(eval TMP := $(du  myprof* | sort -n -r | head -n 1 | cut -f 2))
	$(eval TMP := $(ls))
	@echo $(TMP) 
	$(TMP) 
	ls 

profile: 
	rm -f myprof.log*
	export LD_PRELOAD=/usr/lib/libprofiler.so; CPUPROFILE="myprof.log" R -f run-test.r 
	du -h myprof.log*
	FILE = du  myprof* | sort -n -r | head -n 1 | cut -f 2
	google-pprof --text /usr/bin/R $(FILE) 

clean:
	rm src/*.o
	rm src/*.so

clean-vignettes:
	find vignettes/ -type f -print | grep -Fvf vignettes/.install_extras | grep "tex" | xargs rm -f
	find vignettes/ -type f -print | grep -Fvf vignettes/.install_extras | grep "pdf" | xargs rm -f
	rm vignettes/*.log
	rm vignettes/*.R

.PHONY: printl

vignette-includes: ~/code-papers/tex-includes/macros-paper.tex ~/code-papers/tex-includes/macros-math.tex ~/code-papers/paper-bnclassify-rjournal/RJreferences.bib ~/code-papers/paper-bnclassify-rjournal/pg_*.png 
	cp ~/code-papers/tex-includes/macros-paper.tex  vignettes/  
	cp ~/code-papers/tex-includes/macros-math.tex  vignettes/ 
	cp ~/code-papers/tex-includes/macros-rjournal.tex  vignettes/ 
	cat vignettes/methods.bib > vignettes/bnclassify.bib
	cat ~/code-papers/paper-bnclassify-rjournal/RJreferences.bib >> vignettes/bnclassify.bib
	cp ~/code-papers/paper-bnclassify-rjournal/pg_*.png vignettes/

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

check:
	R < meta/check.r --no-save >> ~/Desktop/checks-output
