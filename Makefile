objects = hamod.o ncmod.o shmod.o ha.o
comp = gfortran
std = f2008
src = ./
build = ./build/
install = ~/bin/
netcdf = /usr
exec = ha.exe
shtools = /usr/local
params = -fbackslash -O3 -march=native -ffast-math -m64 -fopenmp

ha: mkdir $(objects)
	$(comp) -o $(build)$(exec) $(params) $(build)*.o -L$(netcdf)/lib/x86_64-linux-gnu/ -lnetcdff -L$(shtools)/lib/ -lSHTOOLS-mp -lfftw3

mkdir:
	mkdir -p $(build)

ha.o: $(src)ha.f08
	$(comp) -o $(build)ha.o -c -std=$(std) $(params) -I$(shtools)/include/ $(src)ha.f08 -J$(build)

hamod.o: $(src)hamod.f08
	$(comp) -o $(build)hamod.o -c -std=$(std) $(params) $(src)hamod.f08 -J$(build)

ncmod.o: $(src)ncmod.f08
	$(comp) -o $(build)ncmod.o -c -std=$(std) $(params) -I$(netcdf)/include/ $(src)ncmod.f08 -J$(build)

shmod.o: $(src)shmod.f08
	$(comp) -o $(build)shmod.o -c -std=$(std) $(params) $(src)shmod.f08 -J$(build)

install:
	cp $(build)$(exec) $(install)

uninstall: 
	rm $(install)/$(exec)

clean:
	rm -rf $(build)
