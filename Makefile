objects = ha.o
comp = gfortran
outdir = ~/bin/
NFDIR = /usr
exec = ha.exe

ha: $(objects)
	$(comp) -o $(exec) $(objects) -L$(NFDIR)/lib/x86_64-linux-gnu/ -lnetcdff

ha.o: ha.f90
	$(comp) -c -I$(NFDIR)/include/ ha.f90

install: ha
	cp ./$(exec) $(outdir)

uninstall: ha 
	rm $(outdir)/$(exec)

clean:
	rm -rf *.mod
	rm -rf *.o
	rm -rf $(exec)
