FC=gfortran
FFLAGS=-Wall -std=f2008ts -fimplicit-none -Wno-maybe-uninitialized -static-libgfortran -flto
FOPT=-O3 -ftree-vectorize -fopt-info-vec

all:
	$(FC) $(FFLAGS) -c src/environment.f90 -J obj/ -o obj/environment.o
	$(FC) $(FFLAGS) -c src/stack_impl.f90 -J obj/ -o obj/stack_impl.o
	$(FC) $(FFLAGS) -c src/expr_io.f90 -J obj/ -o obj/expr_io.o
	$(FC) $(FFLAGS) -c src/expr_process.f90 -J obj/ -o obj/expr_process.o
	$(FC) $(FFLAGS) $(FOPT) -c src/main.f90 -I obj/ -o obj/main.o
	$(FC) $(FFLAGS) $(FOPT) -o bin/app ./obj/environment.o ./obj/expr_io.o ./obj/stack_impl.o ./obj/expr_process.o ./obj/main.o

clean:
	rm -rf obj/*
	rm -rf bin/*

run:
	cd ./bin; ./app;
	cat bin/output.txt
