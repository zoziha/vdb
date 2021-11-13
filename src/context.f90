module context

    use stdlib_string_type, only: string_type, assignment(=), char, write (formatted)
    use json_module, only: json_file, json_core, json_value
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

        dir = "example/vscode/launch.json"

        inquire (file=char(dir), exist=exist)

        !> Verboese = .true. will throw error(warnning) if path not exist
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

        use stdlib_strings, only: find, slice, replace_all, to_string
        type(json_file), intent(inout) :: json
        type(string_type), intent(in) :: app
        type(string_type), intent(in) :: args(:)

        type(json_value), pointer :: configuration
        type(json_value), pointer :: child
        character(:), allocatable :: version, task_app
        type(string_type) :: name, app_
        logical :: found
        character(:), allocatable :: args_
        integer :: i, j

        call json%get("version", version)
        print *, "version: ", trim(version)

        call json%get("configurations(1).type", version)
        print *, "type: ", version

        call json%get("configurations(1).program", version)
        print *, "program: ", version
        
        app_ = replace_all(app, "\", "/")
        name = slice(app_, find(app_, "/", 2) + 1)

        print *, name, app_
        
        i = 0
        do
            i = i + 1
            call json%get("configurations("//to_string(i)//").name", task_app, found)
            if (.not. found) exit
            
            found = task_app == char(name)
            if (found) exit
        end do
        
        !> Update
        if (found) then
        
            args_ = ""
            do j = 1, size(args) - 1
                args_ = args_//char(args(j))//" "
            end do
            args_ = args_//char(args(size(args)))
        
            call json%update("configurations("//to_string(i)//").program",char(app_), found)
            call json%update("configurations("//to_string(i)//").programArgs", args_, found)
        
        !> Create
        else
        
            args_ = ""
            do j = 1, size(args) - 1
                args_ = args_//char(args(j))//" "
            end do
            args_ = args_//char(args(size(args)))
            
            call json%add("configurations("//to_string(i)//").type", "by-gdb")
            call json%add("configurations("//to_string(i)//").request", "launch")
            call json%add("configurations("//to_string(i)//").name", char(name))
            call json%add("configurations("//to_string(i)//").program", char(app_))
            call json%add("configurations("//to_string(i)//").cwd", "${workspaceRoot}")
            call json%add("configurations("//to_string(i)//").programArgs", args_)
        
        end if

    end subroutine construct_json_from_fpm

end module context
