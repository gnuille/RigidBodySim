module io_m
      use rigid_body_m
      use vector2_m
      use time_m
      use constants_m
      use io_xyz_m
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
                      real(kind=dp) :: vi, vj, pi, pj, ai, aj, q, m
                      read (fd, *, IOSTAT=stat) pi, pj, vi, vj, ai, aj, q, m
                      call set_rigid_body(rigid_body, pi, pj, vi, vj, ai, aj, q, m) 
              end subroutine read_rigid_body

              subroutine print_rigid_body(rigid_body)
                      implicit none
                      type(rigid_body_t) :: rigid_body
                      real(kind=dp) :: pi, pj, vi, vj, ai, aj, q, m
                      call get_rigid_body(rigid_body, pi, pj, vi, vj, ai, aj, q, m)

                      print *, "Body:"
                      call print_vec2d_double("        -Position: ",len("        -Position: "), pi, pj)
                      call print_vec2d_double("        -Velocity: ",len("        -Position: "), vi, vj)
                      call print_vec2d_double("        -Acceleration",len("        -Acceleration: "), ai, aj)
                      call print_real("        -Mass: ", len("        -Mass: "), m) 
                      call print_real("        -Charge: ", len("        -Charge: "), q)

              end subroutine print_rigid_body

              subroutine print_pos_rigid_body(rigid_body)
                      implicit none
                      type(rigid_body_t) :: rigid_body
                      real(kind=dp) :: pi, pj, vi, vj, ai, aj, q, m
                      call get_rigid_body(rigid_body, pi, pj, vi, vj, ai, aj, q, m)

                      call print_vec2d_double("Body is at position: ", len("Body is at position: "), pi, pj)
              end subroutine print_pos_rigid_body

              subroutine read_time( fd, time, stat )
                      implicit none
                      integer :: fd, stat
                      type(time_t) :: time
                      real(kind=dp) :: bt, st, et
                      read (fd, *, IOSTAT=stat) bt, st, et
                      call set_time( time, bt, et, st )
              end subroutine read_time

              subroutine read_vector2( fd, v2, stat )
                      implicit none
                      integer :: fd, stat
                      type(vector2_t) :: v2
                      real(kind=dp) :: i, j
                      read (fd, *, IOSTAT=stat) i, j
                      call set_v2( v2, i, j)
              end subroutine read_vector2

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

              subroutine print_vec2d_double( label, lsize, i, j)
                      implicit none
                      real(kind=dp) :: i, j
                      integer :: lsize
                      character(len=lsize) :: label
                      character(len=80):: vecfmt
                      vecfmt = "(A,A,F0.5,A,F0.5,A)"
                      write(*, vecfmt) label,"(",i,",",j,")"
              end subroutine print_vec2d_double

              subroutine print_vector2( label, lsize, v)
                      implicit none
                      type(vector2_t) :: v 
                      integer :: lsize
                      character(len=lsize) :: label
                      real(kind=dp) i, j
                      call get_v2(v, i, j)
                      call print_vec2d_double( label, lsize, i, j)
              end subroutine print_vector2

              subroutine print_real( label, lsize, r )
                      implicit none
                      real(kind=dp) :: r
                      integer :: lsize
                      character(len = lsize) :: label
                      character(len=80) :: vecfmt
                      vecfmt ="(A,F0.5)"
                      write(*, vecfmt) label, r
              end subroutine print_real

              subroutine print_integer( label, lsize, i )
                      implicit none
                      integer :: i, lsize
                      character(len=lsize) :: label
                      character(len=80) :: ifmt
                      print '("",A,I0)', label, i
              end subroutine print_integer

              subroutine print_update_vector2(label, lsize, vo, vf )
                      implicit none
                      type(vector2_t) :: vo, vf
                      integer :: lsize
                      character(len=lsize) :: label
                      character(len=80) :: f
                      real(kind=dp) :: io, jo, i, j
                      f="(A,A,F0.5,A,F0.5,A,F0.5,A,F0.5,A)"
                      call get_v2(vo, io, jo)
                      call get_v2(vf, i, j)
                      write(*, f) label, "from (",io,",",jo,") to (",i,",",j,")"
              end subroutine print_update_vector2
                      
              subroutine print_iteration_xyz( i, t, b, n)
                      implicit none
                      integer :: i, n, j
                      type(time_t) :: t
                      type(rigid_body_t), pointer, dimension(:) :: b
                     
                      call print_header_xyz(i, t, n)
                      do j = 1,n
                        call print_rigid_body_xyz(b(j))
                      end do
              end subroutine print_iteration_xyz

              subroutine print_force( vec2 )
                      implicit none
                      type(vector2_t) :: vec2
                      real(kind=dp) i, j
                      call get_v2(vec2, i, j)
                      print *, "Global uniform force:"
                      write(*, "(A,F0.5,A,F0.5,A)") "        -Value (",i,",",j,")"
              end subroutine print_force


end module io_m

