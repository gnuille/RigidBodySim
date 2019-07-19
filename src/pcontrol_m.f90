module pcontrol_m
      use io_m
      use atom_m
      implicit none
      integer, parameter :: input_fd = 3
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

              subroutine parse_input(atoms)
                      implicit none
                      type(atom_t), dimension(:), allocatable :: atoms
                      character(len = 255) :: id
                      integer :: stat = 0, npart = 0, I

                      do while( stat == 0 )
                        read (input_fd, *, IOSTAT=stat) id
                        if (id == "ATOMS") then
                                read (input_fd, *, IOSTAT=stat) npart
                                allocate(atoms(npart)) 
                                do I=1,npart
                                        call read_atom(input_fd, atoms(i), stat)        
                                        call print_atom(atoms(i))
                                end do
                                id = "NONE"
                        end if
                      end do
              end subroutine parse_input

end module pcontrol_m

