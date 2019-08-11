module log_m
      use constants_m
      use io_m
      contains
              subroutine log_rigid_body_step(i, ao, vo, po, a, v, p) 
                        implicit none
                        integer :: i
                        type(vector2_t) :: ao, vo, po, a, v, p
                        call print_integer( "--Updated body ", len("--updated body "), i)
                        call print_update_vector2("  -Acceleration modified ", len("  -Acceleration modified "), ao, a)
                        call print_update_vector2("  -Velocity modified ", len("  -velocity modified "), vo, v)
                        call print_update_vector2("  -Position modified ", len("  -velocity modified "), po, p)
                                                  
              end subroutine log_rigid_body_step
end module log_m

