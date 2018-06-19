objects = hamod.o ncmod.o shmod.o ha.o
comp = gfortran
outdir = ~/bin/
NFDIR = /usr
exec = ha.exe
SHDIR = /usr/local

ha: $(objects)
	$(comp) -o $(exec) $(objects) -L$(NFDIR)/lib/x86_64-linux-gnu/ -lnetcdff -L$(SHDIR)/lib/ -lSHTOOLS -lfftw3

ha.o: ha.f90
	$(comp) -c -I$(NFDIR)/include/ -I$(SHDIR)/include/ ha.f90

hamod.o: hamod.f90
	$(comp) -c -I$(NFDIR)/include/ hamod.f90

ncmod.o: ncmod.f90
	$(comp) -c -I$(NFDIR)/include/ ncmod.f90

shmod.o: shmod.f90
	$(comp) -c -I$(NFDIR)/include/ shmod.f90

install: ha
	cp ./$(exec) $(outdir)

uninstall: ha 
	rm $(outdir)/$(exec)

clean:
	rm -rf *.mod
	rm -rf *.o
	rm -rf $(exec)
