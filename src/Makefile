
MODULES = constants_m.f90 time_m.f90 vector2_m.f90 atom_m.f90 io_m.f90 pcontrol_m.f90 
F90 = gfortran

all: main.f90 $(MODULES)
	$(F90) $(MODULES) main.f90 -o main.x

clean:
	rm -rf *.x *.mod *.o
