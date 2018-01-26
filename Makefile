objects = ha.o
comp = gfortran
outdir = ~/bin/
NFDIR = /usr

ha: $(objects)
	$(comp) -o ha.exe -static $(objects) -I$(NFDIR)/include -L$(NFDIR)/lib/x86_64-linux-gnu -lnetcdff

ha.o: ha.f03
	$(comp) -c ha.f03 -I$(NFDIR)/include 

install: ha
	cp ./ha $(outdir)

uninstall: ha 
	rm $(outdir)/ha

clean:
	rm -rf *.mod
	rm -rf *.o
	rm -rf *.exe
