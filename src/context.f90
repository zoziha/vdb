module context

    use stdlib_string_type, only: string_type, assignment(=)
    implicit none
    private
    
    public :: get_fpm_cmd

contains

    subroutine get_fpm_cmd(app, args)
    
        type(string_type), intent(out) :: app
        type(string_type), intent(out), allocatable :: args(:)
        character(80) :: buffer
        integer :: i, nargs
        
        nargs = command_argument_count()
        allocate(args(nargs-1))
        
        call get_command_argument(1, buffer)
        app = trim(buffer)
        
        if (nargs >= 2) then
        
            do i = 2, nargs
            
                call get_command_argument(i, buffer)
                args(i-1) = trim(buffer)
            
            end do
            
        end if
        
    end subroutine get_fpm_cmd

end module context