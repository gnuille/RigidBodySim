module vector2_m
      use constants_m
      implicit none
      type vector2_t
              real(kind=dp) :: i,j
      end type vector2_t
      public :: get_i_v2, get_j_v2, set_v2
      contains
              subroutine set_v2( v, i, j) 
                        implicit none
                        type(vector2_t) :: v
                        real(kind=dp) :: i, j
                        v%i = i
                        v%j = j
              end subroutine set_v2

              subroutine get_v2(v, i, j)
                      implicit none
                      type(vector2_t) :: v
                      real(kind=dp) :: i, j
                      i = v%i
                      j = v%j
              end subroutine get_v2
              
              subroutine get_i_v2( v, i ) 
                        implicit none
                        type(vector2_t) :: v
                        real(kind=dp) :: i
                        i = v%i
              end subroutine get_i_v2              

              subroutine get_j_v2( v, j ) 
                        implicit none
                        type(vector2_t) :: v
                        real(kind=dp) :: j
                        j = v%j
              end subroutine get_j_v2              

              function scalar_product_v2( v, k ) result(ret)
                      implicit none
                      type(vector2_t), intent(in) :: v
                      real(kind=dp), intent(in) :: k
                      type(vector2_t) :: ret
                      ret%i = v%i*k
                      ret%j = v%j*k
              end function scalar_product_v2

              function add_v2( a, b) result(ret)
                      implicit none
                      type(vector2_t), intent(in) :: a, b
                      type(vector2_t) :: ret
                      ret%i = a%i + b%i
                      ret%j = a%j + b%j
              end function add_v2

end module vector2_m

