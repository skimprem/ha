objects = hamod.o ncmod.o ha.o
comp = gfortran
outdir = ~/bin/
NFDIR = /usr
exec = ha.exe

ha: $(objects)
	$(comp) -o $(exec) $(objects) -L$(NFDIR)/lib/x86_64-linux-gnu/ -lnetcdff

ha.o: ha.f90
	$(comp) -c -I$(NFDIR)/include/ ha.f90

hamod.o: hamod.f90
	$(comp) -c -I$(NFDIR)/include/ hamod.f90

ncmod.o: ncmod.f90
	$(comp) -c -I$(NFDIR)/include/ ncmod.f90

install: ha
	cp ./$(exec) $(outdir)

uninstall: ha 
	rm $(outdir)/$(exec)

clean:
	rm -rf *.mod
	rm -rf *.o
	rm -rf $(exec)
