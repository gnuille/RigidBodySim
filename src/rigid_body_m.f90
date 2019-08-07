module rigid_body_m
      use vector2_m
      use constants_m
      implicit none
      type rigid_body_t
              type(vector2_t) :: pos, vel
              real(kind=dp) :: q, m
      end type rigid_body_t

      public :: set_rigid_body, get_rigid_body, get_vel_rigid_body, get_pos_rigid_body

      contains
              subroutine set_rigid_body( a, pi, pj, vi, vj, q, m )
                      implicit none
                      type(rigid_body_t) :: a
                      real(kind=dp) :: vi, vj, pi, pj, q, m

                      call set_v2(a%pos, pi, pj)
                      call set_v2(a%vel, vi, vj) 
                      a%q = q
                      a%m = m

              end subroutine set_rigid_body

              subroutine get_rigid_body( a, pi, pj, vi, vj, q, m )
                      implicit none
                      type(rigid_body_t) :: a
                      real(kind=dp) :: vi, vj, pi, pj, q, m

                      call get_i_v2(a%vel, vi)
                      call get_j_v2(a%vel, vj)
                      call get_i_v2(a%pos, pi)
                      call get_j_v2(a%pos, pj)
                      q = a%q
                      m = a%m

              end subroutine get_rigid_body

              subroutine get_vel_rigid_body(a, v)
                      implicit none
                      type(rigid_body_t) :: a
                      type(vector2_t) :: v
                      v = a%vel
              end subroutine get_vel_rigid_body

              subroutine get_pos_rigid_body(a, p)
                      implicit none
                      type(rigid_body_t) :: a
                      type(vector2_t) :: p
                      p = a%pos
              end subroutine get_pos_rigid_body

              subroutine copy_rigid_body( inp, src )
                      implicit none
                      type(rigid_body_t) :: inp, src
                      src = inp
              end subroutine copy_rigid_body 

end module rigid_body_m

