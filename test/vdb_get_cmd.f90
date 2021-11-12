! fpm test vdb_init --runner build\gfortran_2A42023B310FA28D\test\vdb_get_cmd.exe -- ARGS

!> Results:

! build\gfortran_2A42023B310FA28D\test\vdb_init.exe
! ARGS

program vdb_get_cmd

    implicit none
    character(80) :: cmd
    
    ! fpm test vdb_get_cmd -- build xx
    call get_command_argument(1, cmd)
    print *, cmd
    
    call get_command_argument(2, cmd)
    print *, cmd

end program vdb_get_cmd