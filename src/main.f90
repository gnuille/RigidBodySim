PROGRAM test
        use pcontrol_m
        use atom_m
        type(atom_t), dimension(:), allocatable :: atoms

        call parse_args()
        call parse_input(atoms)
        !PRINT '("",I0)', ACUM

END PROGRAM test


