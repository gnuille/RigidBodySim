module pcontrol_m
        use io_m
        use rigid_body_m
        use time_m
        use physics_m
        use log_m
        implicit none 
        integer, parameter :: input_fd = 3 
        integer :: nbodies = 0
        type(rigid_body_t), target, dimension(:), allocatable :: bodies1, bodies2
        type(rigid_body_t), pointer, dimension(:) :: current, tmp
        type(time_t) :: sim_time

        public :: parse_args

contains
        subroutine parse_args() 
                implicit none
                character(len = 255) :: filename
                integer :: narg

                narg=command_argument_count()
                if ( narg /= 1 ) then
                        call usage()
                        call end_program(1)
                end if

                call get_command_argument(1, filename)
                call open_read(filename, input_fd)
                print *, "Succefully loaded input file"
end subroutine parse_args

        subroutine usage()
                implicit none
                print *, "Usage ./main.x inputfile"
        end subroutine usage

        subroutine end_program(ec)
                implicit none
                integer :: ec
                if (ec == 0) then
                        stop
                else
                        error stop
                endif
        end subroutine end_program

        subroutine parse_input()
                implicit none
                character(len = 255) :: id
                integer :: stat = 0, I

                do while( stat == 0 )
                read (input_fd, *, IOSTAT=stat) id
                if (id == "BODIES") then
                        read (input_fd, *, IOSTAT=stat) nbodies
                        allocate(bodies1(nbodies)) 
                        allocate(bodies2(nbodies))
                        current =>  bodies1
                        tmp     =>  bodies2
                        do I=1,nbodies
                                call read_rigid_body(input_fd, current(i), stat)        
                                call copy_rigid_body( current(i), tmp(i))
                                call print_rigid_body(current(i))
                        end do
                        id = "NONE"
                else if (id == "TIME") then
                        call read_time(input_fd, sim_time, stat)
                        call print_time(sim_time)
                        id = "NONE"
                end if
                end do
        end subroutine parse_input

        subroutine simulate()
                implicit none
                logical :: ended
                call ended_time( sim_time, ended)
                do while (.not. ended )
                        print *, "New iteration: "
                        call print_time( sim_time )
                        call step()
                        call ended_time( sim_time, ended)
                end do
                call end_sim()
        end subroutine simulate

        subroutine step()
                implicit none
                type(rigid_body_t), pointer, dimension(:) :: swp

                integer :: I
                do I=1,nbodies
                        call compute_rigid_body_step(I)        
                end do
                !swap current and tmp pointers
                swp => current
                current => tmp
                tmp => swp
                ! update time step for next iteration
                call step_time( sim_time )

        end subroutine step

        subroutine end_sim()
                implicit none
                print *, "Simulation has ended"
                stop 0
        end subroutine end_sim

        subroutine compute_rigid_body_step(i)
                implicit none
                integer :: i
                type(vector2_t) :: a, v, p
                real(kind=dp) :: dt
#ifdef DEBUG
                type(vector2_t) :: ao, vo, po
#endif DEBUG

                !calculate F
                !calculate A = ( F/m )
                call get_acc_rigid_body(current(i), a)
                call get_vel_rigid_body(current(i), v)
                call get_pos_rigid_body(current(i), p)
#ifdef DEBUG
                ao = a
                vo = v
                po = p
#endif DEBUG
                
                call get_delta_time( sim_time, dt) 
                p = pos_mrua( p, a, v, dt)
                v = vel_mrua( v, a, dt)
#ifdef DEBUG
                call log_rigid_body_step(i, ao, vo, po, a, v, p)
#endif

                call update_rigid_body( tmp(i), a, v, p)

        end subroutine compute_rigid_body_step 

end module pcontrol_m

