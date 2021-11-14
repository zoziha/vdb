submodule(context) context_clean

    implicit none

contains

    !> clean launch.json
    module subroutine vdb_clean()
        use forlab_color, only: yellow, default
        logical :: exist

        inquire (file=dir, exist=exist)
        if (exist) then
            call system("mv "//dir//" "//dir//"_bak")
            print *, "JSON::luanch.json has removed!"//NL

        else
            print *, yellow//"*<WARNNING>*"//default//" JSON::luanch.json does not exist, nothing to do!"//NL

        end if

    end subroutine vdb_clean

end submodule context_clean
