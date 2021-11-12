!> fpm test vdb_init --runner build\gfortran_2A42023B310FA28D\app\vdb.exe -- ARGS
program vdb

    use context, only: get_fpm_cmd
    use stdlib_string_type, only: string_type, char
    implicit none
    type(string_type) :: app
    type(string_type), allocatable :: args(:)
    
    print *, "Hello, vdb ~ "
    call get_fpm_cmd(app, args)
    print *, char(app)
    print *, char(args), size(args)

end program vdb