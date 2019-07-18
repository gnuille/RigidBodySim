module vector2_m
      implicit none
      type vector2_t
              real :: i,j
      end type vector2_t
      public :: get_i_v2, get_j_v2, set_v2

      contains
              subroutine set_v2( v, i, j) 
                        implicit none
                        type(vector2_t) :: v
                        real :: i, j
                        v%i = i
                        v%j = j
              end subroutine set_v2
              
              subroutine get_i_v2( v, i ) 
                        implicit none
                        type(vector2_t) :: v
                        real :: i
                        i = v%i
              end subroutine get_i_v2              

              subroutine get_j_v2( v, j ) 
                        implicit none
                        type(vector2_t) :: v
                        real :: j
                        j = v%j
              end subroutine get_j_v2              

end module vector2_m

