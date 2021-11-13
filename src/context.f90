module context

    use stdlib_string_type, only: string_type, assignment(=),char
    use json_module, only: json_file, json_core,json_value
    implicit none
    private

    public :: get_fpm_cmd, processing_json

contains

    !> Parse fpm CMD
    subroutine get_fpm_cmd(app, args)

        type(string_type), intent(out) :: app
        type(string_type), intent(out), allocatable :: args(:)
        character(80) :: buffer
        integer :: i, nargs

        nargs = command_argument_count()
        allocate (args(nargs - 1))

        call get_command_argument(1, buffer)
        app = trim(buffer)

        if (nargs >= 2) then

            do i = 2, nargs

                call get_command_argument(i, buffer)
                args(i - 1) = trim(buffer)

            end do

        end if

    end subroutine get_fpm_cmd

    !> Parse file dirction
    subroutine processing_json(app, args)

        type(string_type), intent(in) :: app
        type(string_type), intent(in) :: args(:)
        type(string_type) :: dir
        type(json_file) :: json
        character(*), parameter :: NL = new_line("")
        logical :: exist
        integer :: unit

        dir = "./.vscode/launch.json"

        inquire (file=char(dir), exist=exist)

        call json%initialize(verbose=.true., spaces_per_tab=4)

        if (exist) then
        
            call json%load(filename=char(dir))
            call construct_json_from_fpm(json, app, args)
            ! call json%print(filename=char(dir))
            call json%print()
            
        else
        
            call system("mkdir "//char(dir))
            call construct_json_from_fpm(json, app, args)
            call json%print(filename=char(dir))
            
        end if

    end subroutine processing_json
    
    subroutine construct_json_from_fpm(json, app, args)

        type(json_file), intent(inout) :: json
        type(string_type), intent(in) :: app
        type(string_type), intent(in) :: args(:)
        
        type(json_value), pointer :: configuration
        type(json_value), pointer :: child
        character(:), allocatable :: version
        
        call json%get("version", version)
        print *, "version: ", trim(version)
        
        call json%get("configurations(1).type", version)
        print *, "type: ", version
        
        call json%get("configurations(1).program", version)
        print *, "program: ", version
        
    end subroutine construct_json_from_fpm

end module context
