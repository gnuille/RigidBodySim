module io_xyz_m
      use rigid_body_m
      use time_m
      use constants_m
      implicit none
      contains

             subroutine float2char(f, c, nchars )
                     implicit none
                     character(*) :: c
                     real(kind=dp) :: f
                     integer :: nchars

                     write(c, '(F0.5)') f 
                     if(c(1:1) == '.') then
                         c= '0' // c
                         nchars = 7
                     else if(c(1:2) == '-.') then
                         c= '-0.' // c(3:)
                         nchars = 8
                     else
                         nchars = -1
                     end if
             end subroutine float2char


              subroutine print_header_xyz(i, t, n)
                      implicit none
                      type(time_t) :: t
                      integer :: i, n, ch
                      real(kind=dp) :: ct
                      character(len=80) :: header_fmt, ptime
                      character(len=8) :: c8
                      character(len=7) :: c7

                      call get_current_time(t, ct)
                      call float2char(ct, ptime, ch)
                      print '("",I0)', n

                      if (ch == 7) then
                              call float2char(ct, c7, ch) 
                              print '("",A,I0,A,A)', " i = ",i," , time = ", c7

                      else if (ch == 8) then
                              call float2char(ct, c8, ch) 
                              print '("",A,I0,A,A)', " i = ",i," , time = ", c8
                      else
                              print '("",A,I0,A,F0.5)', " i = ",i," , time = ", ct
                              
                      end if

              end subroutine print_header_xyz 

              subroutine print_rigid_body_xyz(rigid_body)
                      implicit none
                      type(rigid_body_t) :: rigid_body
                      real(kind=dp) :: pi, pj, vi, vj, ai, aj, q, m
                      character(len=80) :: cc, cf, fc, ff, ci, cj
                      character(len=8) :: c18, c28
                      character(len=7) :: c17, c27
                      integer :: ch1, ch2
                      cc="(A,A,A,A,A,F0.5)"
                      cf="(A,A,A,F0.5,A,F0.5)"
                      fc="(A,F0.5,A,A,A,F0.5)"
                      ff="(A,F0.5,A,F0.5,A,F0.5)"

                      call get_rigid_body(rigid_body, pi, pj, vi, vj, ai, aj, q, m)
                      call float2char(pi, ci, ch1)
                      call float2char(pj, cj, ch2)

                      if ( (ch1 .GT. 0) .AND. (ch2 .GT. 0 ) ) then
                              if ( ch1 .EQ. 8  ) then
                                      call float2char(pi, c18, ch1)
                                      if ( ch2 .EQ. 8 ) then
                                              call float2char(pj, c28, ch2)
                                              write(*, cc) "F ", c18, " ", c28, " ", 1.000
                                      else
                                              call float2char(pj, c27, ch2)
                                              write(*, cc) "F ", c18, " ", c27, " ", 1.000
                                      end if
                              else
                                      call float2char(pi, c17, ch1)
                                      if ( ch2 .EQ. 8) then
                                              call float2char(pj, c28, ch2)
                                              write(*, cc) "F ", c17, " ", c28, " ", 1.000
                                      else
                                              call float2char(pj, c27, ch2)
                                              write(*, cc) "F ", c17, " ", c27, " ", 1.000
                                      end if
                              end if
                      else if ( ch1 .GT. 0 ) then
                              if ( ch1 .EQ. 8 ) then
                                      call float2char(pi, c18, ch1)
                                      write(*, cf) "F ", c18, " ", pj, " ", 1.000
                              else
                                      call float2char(pi, c17, ch1)
                                      write(*, cf) "F ", c17, " ", pj, " ", 1.000
                              end if
                      else if ( ch2 .GT. 0) then
                              if (ch2 .EQ. 8 ) then
                                      call float2char(pj, c28, ch2)
                                      write(*, fc) "F ", pi, " ", c28, " ", 1.000
                              else
                                      call float2char(pj, c27, ch2)
                                      write(*, fc) "F ", pi, " ", c27, " ", 1.000
                              end if
                      else
                              write(*, ff) "F ", pi, " ", pj, " ", 1.000
                      end if

              end subroutine print_rigid_body_xyz
                      
end module io_xyz_m

