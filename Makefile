objects = ha.o
comp = gfortran
outdir = ~/bin/

ha: $(objects)
	$(comp) -o ha.exe -static $(objects)

ha.o: ha.f03
	$(comp) -c ha.f03

install: ha
	cp ./ha $(outdir)

uninstall: ha 
	rm $(outdir)/ha

clean:
	rm -rf *.mod
	rm -rf *.o
	rm -rf *.exe
