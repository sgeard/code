module clib
    use iso_c_binding
    
    integer, parameter :: S_ISUID = int(o'4000')   ! set-user-ID bit
    integer, parameter :: S_ISGID = int(o'2000')   ! set-group-ID bit (see below)
    integer, parameter :: S_ISVTX = int(o'1000')   ! sticky bit (see below)

    integer, parameter :: S_IRWXU = int(o'0700')   ! owner has read, write, and execute permission
    integer, parameter :: S_IRUSR = int(o'0400')   ! owner has read permission
    integer, parameter :: S_IWUSR = int(o'0200')   ! owner has write permission
    integer, parameter :: S_IXUSR = int(o'0100')   ! owner has execute permission

    integer, parameter :: S_IRWXG = int(o'0070')   ! group has read, write, and execute permission
    integer, parameter :: S_IRGRP = int(o'0040')   ! group has read permission
    integer, parameter :: S_IWGRP = int(o'0020')   ! group has write permission
    integer, parameter :: S_IXGRP = int(o'0010')   ! group has execute permission

    integer, parameter :: S_IRWXO = int(o'0007')   ! others (not in group) have read, write, and execute permission
    integer, parameter :: S_IROTH = int(o'0004')   ! others have read permission
    integer, parameter :: S_IWOTH = int(o'0002')   ! others have write permission
    integer, parameter :: S_IXOTH = int(o'0001')   ! others have execute permission

    interface
    
        function mkdir(pathname, mode) bind(c)
            import
            integer(c_int) :: mkdir
            character(c_char), intent(in)     :: pathname(*)
            integer(c_int), intent(in), value :: mode
        end function mkdir
    
        function mkdirat(pathname, mode) bind(c)
            import
            integer(c_int) :: mkdirat
            character(c_char), intent(in)     :: pathname(*)
            integer(c_int), intent(in), value :: mode
        end function mkdirat
        
    end interface

end module clib