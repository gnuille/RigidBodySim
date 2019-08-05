PROGRAM test
        use pcontrol_m

        call parse_args()
        call parse_input()
        call simulate()
        !PRINT '("",I0)', ACUM

END PROGRAM test


