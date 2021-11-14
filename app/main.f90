!> fpm test vdb_init --runner build\gfortran_2A42023B310FA28D\app\vdb.exe -- ARGS
program vdb

    use context, only: get_fpm_cmd, processing_json, vdb_clean
    use stdlib_string_type, only: string_type, write (formatted)
    implicit none
    type(string_type) :: app
    type(string_type), allocatable :: args(:)
    character(:), allocatable :: type

    print *, ""

    call get_fpm_cmd(app, args, type)

    select case (type)
    case ("build")
        call processing_json(app, args)
    case ("clean")
        
        call vdb_clean()
        
    end select

end program vdb
