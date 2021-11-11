program vdb_init

    use json_module, only: json_file
    implicit none
    
    type(json_file) :: json
    
    call json%initialize(verbose=.true., spaces_per_tab=4)
    
    call json%load(filename = "example/launch-file/launch.json")
    
    call json%print()
    
end program vdb_init