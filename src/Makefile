
MODULES = constants_m.f90 time_m.f90 vector2_m.f90 physics_m.f90 rigid_body_m.f90 io_m.f90 log_m.f90 pcontrol_m.f90 
F90 = gfortran

all: main.x

main.x: main.f90 $(MODULES)
	$(F90) $(MODULES) main.f90 -o main.x

main.d.x: main.f90 $(MODULES)
	$(F90) -DDEBUG $(MODULES) main.f90 -o main.x

test: main.d.x
	./main.x ../test/simple.inp
	$(MAKE) clean

clean:
	rm -rf *.x *.mod *.o
