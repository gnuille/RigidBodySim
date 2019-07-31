module io_m
      use rigid_body_m
      use time_m
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

              subroutine read_rigid_body( fd, rigid_body, stat )
                      implicit none
                      integer :: fd, stat
                      type(rigid_body_t) :: rigid_body
                      real(kind=dp) :: vi, vj, pi, pj, q, m
                      read (fd, *, IOSTAT=stat) pi, pj, vi, vj, q, m
                      call set_rigid_body(rigid_body, pi, pj, vi, vj, q, m) 
              end subroutine read_rigid_body

              subroutine print_rigid_body(rigid_body)
                      implicit none
                      type(rigid_body_t) :: rigid_body
                      real(kind=dp) :: pi, pj, vi, vj, q, m
                      character(len=80) :: vecfmt
                      call get_rigid_body(rigid_body, pi, pj, vi, vj, q, m)

                      print *, "Body:"
                      call print_vec2d("        -Position: ",len("        -Position: "), pi, pj)
                      call print_vec2d("        -Velocity: ",len("        -Position: "), vi, vj)
                      call print_real("        -Mass: ", len("        -Mass:"), m) 
                      call print_real("        -Charge: ", len("       -Charge: "), q)

              end subroutine print_rigid_body

              subroutine read_time( fd, time, stat )
                      implicit none
                      integer :: fd, stat
                      type(time_t) :: time
                      real(kind=dp) :: bt, st, et
                      read (fd, *, IOSTAT=stat) bt, st, et
                      call set_time( time, bt, et, st )
              end subroutine read_time

              subroutine print_time( time )
                      implicit none
                      type(time_t) :: time
                      real(kind=dp) :: b, e, c, s
                      call get_time(time, b, e, c, s)
                      print *, "Time: "
                      call print_real("        -Begin time: ", len("        -Begin time: "), b)
                      call print_real("        -Step: ",len("        -Step: "), s)
                      call print_real("        -Current time: ", len("        -Current time: "), c)
                      call print_real("        -End time: ", len("        -End time: "), e)   
              end subroutine print_time

              subroutine print_vec2d( label, lsize, i, j)
                      implicit none
                      real(kind=dp) :: i, j
                      integer :: lsize
                      character(len=lsize) :: label
                      character(len=80):: vecfmt
                      vecfmt = "(A,A,F0.5,A,F0.5,A)"
                      write(*, vecfmt) label,"(",i,",",j,")"
              end subroutine print_vec2d

              subroutine print_real( label, lsize, r )
                      implicit none
                      real(kind=dp) :: r
                      integer :: lsize
                      character(len = lsize) :: label
                      character(len=80) :: vecfmt
                      vecfmt ="(A,F0.5)"
                      write(*, vecfmt) label, r
              end subroutine print_real


end module io_m

