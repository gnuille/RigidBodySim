module time_m
        use constants_m
        implicit none
        type time_t
                real(kind=dp) :: begin_time
                real(kind=dp) :: end_time
                real(kind=dp) :: step_time
                real(kind=dp) :: current_time
        end type time_t

        contains
                subroutine set_time( t, b, e, s)
                        implicit none
                        type(time_t) :: t
                        real(kind=dp) :: b, e, s
                        t%begin_time = b
                        t%end_time = e
                        t%step_time = s
                        t%current_time = b
                end subroutine set_time

                subroutine get_time( t, b, e, c, s)
                        implicit none
                        type(time_t) :: t
                        real(kind=dp) :: b, e, c, s
                        b = t%begin_time
                        e = t%end_time
                        c = t%current_time
                        s = t%step_time
                end subroutine get_time

                subroutine get_delta_time( t, dt)
                        implicit none
                        type(time_t) :: t
                        real(kind=dp) :: dt
                        dt = t%step_time
                end subroutine get_delta_time 

                subroutine step_time( t )
                        implicit none
                        type(time_t) :: t
                        t%current_time = t%current_time + t%step_time
                end subroutine step_time

                subroutine ended_time( t, b )
                        implicit none
                        type(time_t) :: t
                        logical :: b

                        b = (t%end_time <= t%current_time)
                end subroutine ended_time

end module time_m
