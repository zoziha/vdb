module context

    use stdlib_string_type, only: string_type, assignment(=), char, write (formatted)
    use json_module, only: json_file, json_core, json_value
    implicit none
    private

    public :: get_fpm_cmd, processing_json, vdb_clean

    character(*), parameter :: NL = new_line("")
    character(*), parameter :: dir = ".vscode/launch.json"

    interface
        module subroutine vdb_clean()
        end subroutine vdb_clean
    end interface

contains

    !> Parse fpm CMD
    subroutine get_fpm_cmd(app, args, type)

        use stdlib_strings, only: starts_with
        use stdlib_ascii, only: to_lower
        use forlab_color, only: yellow, default, red
        type(string_type), intent(out) :: app
        type(string_type), intent(out), allocatable :: args(:)
        character(:), allocatable, intent(out) :: type
        character(80) :: buffer
        integer :: i, nargs

        nargs = command_argument_count()
        if (nargs == 0) then

            print *, yellow//"*<WARNNING>*"//default//" Nothing to do .."//NL
            print *, "Usage 1: fpm [options] <test name> --runner vdb [-- ARGS]"
            print *, "Usage 2: vdb [options] [--<input>] ..."//NL
50          print *, "VDB Version: 0.0.1"
            print *, "License    : MIT"
            print *, "Repo       : https://github.com/zoziha/vdb"
            print *, "Copyright (c) 2021, VDB Developers"//NL
            stop

        end if

        !> Get first argument: maybe a `fpm` app name or a `vdb` argument
        call get_command_argument(1, buffer)
        app = trim(buffer)

        !> Judge command type
        if (starts_with(app, "build")) then
            type = "build"

        elseif (trim(buffer) == "clean") then
            type = "clean"
            return

        elseif (trim(buffer) == "--version" .or. to_lower(trim(buffer)) == "-v") then
            goto 50
            
        else

            print *, red//"*<ERROR>*"//default//" Unknown command: "//trim(buffer)//NL
            goto 50

        end if

        allocate (args(nargs - 1))

        !> Get actual arguments
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
        type(json_file) :: json
        logical :: exist
        integer :: unit

        inquire (file=dir, exist=exist)

        !> Verboese = .true. will throw error(warnning) if path not exist
        call json%initialize(verbose=.false., spaces_per_tab=4)

        if (.not. exist) then

            call system("mkdir .vscode")
            ! call json%destroy()
75          call json%load_from_string("{"//NL// &
                                       '    "version": "0.2.0",'//NL// &
                                       '    "configurations": ['//NL// &
                                       '        {'//NL// &
                                       '            "type": "by-gdb",'//NL// &
                                       '            "request": "launch",'//NL// &
                                       '            "name": "Launch(gdb)",'//NL// &
                                       '            "program": "${fileBasenameNoExtension}",'//NL// &
                                       '            "cwd": "${workspaceRoot}",'//NL// &
                                       '            "programArgs": ""'//NL// &
                                       '        }'//NL// &
                                       '    ]'//NL// &
                                       '}'//NL)
        else

            call json%load(filename=dir)
            block
                character(:), allocatable :: buffer
                call json%get("version", buffer, exist)
                if (.not. exist) then
                    goto 75
                end if
            end block

        end if

        call construct_json_from_fpm(json, app, args, exist)
        ! call json%print()

        if (.not. exist) then

            call json%print(filename=dir)
            print *, NL//"JSON::launch.json file updated!"

        else

            print *, NL//"JSON::launch.json file not updated, since the operation is repeated!"

        end if

    end subroutine processing_json

    subroutine construct_json_from_fpm(json, app, args, found)

        use stdlib_strings, only: find, slice, replace_all, to_string
        use forlab_color, only: cyan, green, default, yellow
        type(json_file), intent(inout) :: json
        type(string_type), intent(in) :: app
        type(string_type), intent(in) :: args(:)

        type(json_value), pointer :: configuration
        character(:), allocatable :: status, task_app
        character(:), allocatable :: name, app_
        logical, intent(out) :: found

        logical :: check(2)
        character(:), allocatable :: args_
        integer :: i, j

        app_ = replace_all(char(app), "\", "/")
        name = slice(app_, find(app_, "/", 2) + 1)

        print *, cyan//" - Task name  : "//default, name
        print *, cyan//" - App path   : "//default, app_

        i = 0
        do
            i = i + 1
            call json%get("configurations("//to_string(i)//").name", task_app, found)

            !> Get init task_app name
            if (.not. found) then
                call json%get("configurations("//to_string(i - 1)//").name", task_app, found)
                found = .false.
                exit
            end if

            !> Judge task_app name
            found = task_app == name
            if (found) exit

        end do

        !> Get all args
        args_ = ""
        if (size(args) == 1) then
            goto 52
            
        elseif (size(args) > 1) then
        
            do j = 1, size(args) - 1
                args_ = args_//char(args(j))//" "
            end do
52          args_ = args_//char(args(size(args)))

        end if

        !> Repeat or update
        if (found) then

            block

                character(:), allocatable :: app, args

                check = .false.

                call json%get("configurations("//to_string(i)//").program", app, found)
                if (found) then
                    check(1) = app_ == app
                end if

                call json%get("configurations("//to_string(i)//").programArgs", args, found)
                if (found) then
                    check(2) = args_ == args
                end if

            end block

            !> Repeat and not update
            if (all(check)) then

                found = .true.
                status = yellow//"Repeated"//default

                !> Different and update
            else

100             call json%update("configurations("//to_string(i)//").program", app_, found)
                call json%update("configurations("//to_string(i)//").programArgs", args_, found)
                found = .false.
                status = green//merge("Updated", "Created", i /= 1 .and. task_app /= "Launch(gdb)")//default

            end if

            !> Init and create
        elseif (.not. found .and. i == 2 .and. task_app == "Launch(gdb)") then

            call json%update("configurations("//to_string(i - 1)//").name", name, found)
            i = i - 1
            goto 100

            !> Create another task
        else

            call json%add("configurations("//to_string(i)//").type", "by-gdb")
            call json%add("configurations("//to_string(i)//").request", "launch")
            call json%add("configurations("//to_string(i)//").name", name)
            call json%add("configurations("//to_string(i)//").program", app_)
            call json%add("configurations("//to_string(i)//").cwd", "${workspaceRoot}")
            call json%add("configurations("//to_string(i)//").programArgs", args_)
            status = green//"Created"//default

        end if

        print *, cyan//" - Args       : "//default, args_

        !> Get the task list
        i = 0
        args_ = ""
        do
            i = i + 1
            call json%get("configurations("//to_string(i)//").name", task_app, check(1))
            if (.not. check(1)) exit

            if (task_app == name) then
                task_app = green//task_app//default
            end if

            args_ = args_//'"'//task_app//'"  '
        end do

        print *, cyan//" - Task status: "//default, status
        print *, cyan//" - Task list  : "//default, args_

    end subroutine construct_json_from_fpm

end module context
