program vdb_init

    use json_module, only: json_file
    implicit none
    
    type(json_file) :: json
    integer :: unit
    
    call json%initialize(verbose=.true., spaces_per_tab=4)
    
    call json%load(filename = "example/launch-file/launch.json")
    
    call json%print()
    
    open (newunit=unit, file="example/launch-file/out.json", form="formatted", status="unknown")
    call json%print(unit)
    close(unit)
    
end program vdb_init