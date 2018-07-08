module clib
    use iso_c_binding
    
    integer, parameter :: S_ISUID = o'4000'   ! set-user-ID bit
    integer, parameter :: S_ISGID = o'2000'   ! set-group-ID bit (see below)
    integer, parameter :: S_ISVTX = o'1000'   ! sticky bit (see below)

    integer, parameter :: S_IRWXU = o'0700'   ! owner has read, write, and execute permission
    integer, parameter :: S_IRUSR = o'0400'   ! owner has read permission
    integer, parameter :: S_IWUSR = o'0200'   ! owner has write permission
    integer, parameter :: S_IXUSR = o'0100'   ! owner has execute permission

    integer, parameter :: S_IRWXG = o'0070'   ! group has read, write, and execute permission
    integer, parameter :: S_IRGRP = o'0040'   ! group has read permission
    integer, parameter :: S_IWGRP = o'0020'   ! group has write permission
    integer, parameter :: S_IXGRP = o'0010'   ! group has execute permission

    integer, parameter :: S_IRWXO = o'0007'   ! others (not in group) have read, write, and execute permission
    integer, parameter :: S_IROTH = o'0004'   ! others have read permission
    integer, parameter :: S_IWOTH = o'0002'   ! others have write permission
    integer, parameter :: S_IXOTH = o'0001'   ! others have execute permission

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

        !function errno() bind(c, name='ierrno')
        !    import
        !    integer(c_int) :: errno
        !end function errno
        
    end interface

end module clib