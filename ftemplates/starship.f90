
module starship
    implicit none
    private
    public :: comp_t, operator(.star.)
    
    ! Simple starship operator inspired by the recent addition to cxx
    ! The idea is to use the generics mechanism to genetrate all the required
    ! comparison functions for .star.
    
    ! Comparison type
    type comp_t
        private
        integer :: result = -2
    contains
        procedure :: to_string => to_string_c
    end type comp_t

    ! Map operator to function
    interface operator(.star.)
        module procedure cf_d_d
    end interface

contains
    pure function to_string_c(this) result(s)
        class(comp_t), intent(in) :: this
        character(len=:), allocatable :: s
        if (this%result == -1) then
            s = ' less than '
        else if (this%result == 1) then
            s = ' greater than '
        else if (this%result == 0) then
            s = ' equal to '
        else
            s = ' ***UNSET*** '
        end if
    end function to_string_c
    
    pure function cf_d_d(a, b) result(r)
        real(8), intent(in) :: a, b
        type(comp_t) :: r
        if (a < b) then
            r%result = -1
        else if (a > b) then
            r%result = 1
        else
            r%result = 0
        end if
    end function cf_d_d
end module starship


program utest
    use starship
    implicit none
    
    real(8) :: x, y
    type(comp_t) :: s
    x = 3.0d0; y = 4.0d0
    
    s = (x .star. y)
    write(*,'(f0.3,a,f0.3)') x,s%to_string(),y
    
    ! The line below doesn't compile because there's no integer(4)/integer(4) comparator
    !s = (1 .star. -3)
    stop
end program utest
