
MODULES = constants_m.f90 time_m.f90 vector2_m.f90 physics_m.f90 rigid_body_m.f90 io_xyz_m.f90 io_m.f90 log_m.f90 pcontrol_m.f90 
F90 = gfortran

FLAGS=-cpp

all: main.x

main.x: main.f90 $(MODULES)
	$(F90) $(FLAGS) $(MODULES) main.f90 -o main.x

main.d.x: main.f90 $(MODULES)
	$(F90) $(FLAGS) -DDEBUG $(MODULES) main.f90 -o main.d.x

main.xyz.x: main.f90 $(MODULES)
	$(F90) $(FLAGS) -DXYZ $(MODULES) main.f90 -o main.xyz.x

run: main.x
	./main.x ../test/simple.inp
	$(MAKE) clean

test: main.d.x
	./main.d.x ../test/simple.inp
	$(MAKE) clean

xyz: main.xyz.x
	./main.xyz.x ../test/simple.inp | sed '$d' > simulation.xyz
	$(MAKE) clean

clean:
	rm -rf *.x *.mod *.o

distclean:
	$(MAKE) clean
	rm -rf *.xyz
