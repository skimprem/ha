objects = hamod.o ncmod.o shmod.o ha.o
comp = gfortran
outdir = ~/bin/
NFDIR = /usr
exec = ha.exe
SHDIR = /usr/local

ha: $(objects)
	$(comp) -o $(exec) $(objects) -L$(NFDIR)/lib/x86_64-linux-gnu/ -lnetcdff -L$(SHDIR)/lib/ -lSHTOOLS -lfftw3

ha.o: ha.f08
	$(comp) -c -I$(NFDIR)/include/ -I$(SHDIR)/include/ ha.f08

hamod.o: hamod.f08
	$(comp) -c -I$(NFDIR)/include/ hamod.f08

ncmod.o: ncmod.f08
	$(comp) -c -I$(NFDIR)/include/ ncmod.f08

shmod.o: shmod.f08
	$(comp) -c -I$(NFDIR)/include/ shmod.f08

install: ha
	cp ./$(exec) $(outdir)

uninstall: ha 
	rm $(outdir)/$(exec)

clean:
	rm -rf *.mod
	rm -rf *.o
	rm -rf $(exec)
