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

.PHONY: printl
