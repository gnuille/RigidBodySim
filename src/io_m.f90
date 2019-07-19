module io_m
      use atom_m
      use constants_m
      implicit none
      public :: open_read
      contains
              subroutine open_read( filename, fd )
                        implicit none
                        character(len = 255) :: filename
                        integer :: fd
                        open ( unit=fd, file=filename, status='old', &
                               access='sequential', form='formatted', & 
                               action='read')
              end subroutine open_read

              subroutine read_id( fd, id, stat)
                      implicit none
                      integer :: fd, stat
                      character(len=255) :: id
                      read(fd, *, IOSTAT=stat) id
              end subroutine read_id

              subroutine read_atom( fd, atom, stat )
                      implicit none
                      integer :: fd, stat
                      type(atom_t) :: atom
                      real(kind=dp) :: vi, vj, pi, pj
                      read (fd, *, IOSTAT=stat) pi, pj, vi, vj
                      call set_atom(atom, pi, pj, vi, vj) 
              end subroutine read_atom

              subroutine print_atom(atom)
                      implicit none
                      type(atom_t) :: atom
                      real(kind=dp) pi, pj, vi, vj
                      call get_atom(atom, pi, pj, vi, vj)
                      print *, "Atom:"
                      print *, "-Position: (",pi,",",pj,")"
                      print *, "-Velocity: (",vi,",",vj,")"
              end subroutine print_atom


end module io_m

