module template_add_m
    implicit none
    private
    public :: add_t

    requirement R(T, F) 
        type :: T; end type
        function F(x, y) result(z)
            type(T), intent(in) :: x, y
            type(T) :: z
        end function
    end requirement

    template add_t(T, F)
        requires R(T, F)
        private
        public :: add_generic
    contains
        function add_generic(x, y) result(z)
            type(T), intent(in) :: x, y
            type(T) :: z
            z = F(x, y)
        end function
    end template
    
    ! Old world
    interface check_result
        module procedure func_check_result_i
        module procedure func_check_result_r
    end interface check_result

contains
    
    real function func_arg_real(x, y) result(z)
        real, intent(in) :: x, y
        z = x + y
    end function

    integer function func_arg_int(x, y) result(z)
        integer, intent(in) :: x, y
        z = x + y
    end function

    subroutine func_check_result_i(r, ref)
        integer, intent(in) :: r, ref
        write(*,advance='no',fmt='(a,i0)') "The result is ",r
        call res(r == ref)
    end subroutine func_check_result_i
    
    subroutine func_check_result_r(r, ref)
        real, intent(in) :: r, ref
        write(*,advance='no',fmt='(a,g0.7)') "The result is ",r
        call res(abs(r-ref) < 1.e-5)
    end subroutine func_check_result_r
   
    subroutine res(ok)
        logical, intent(in) :: ok
        if (ok) then
            write(*,'(a)') ' => passed'
        else
            write(*,'(a)') ' => FAILED'
        end if
    end subroutine res

    subroutine test_template()
        block
        instantiate add_t(real, func_arg_real), only: add_real => add_generic
            real :: x, y, r
            x = 5.1
            y = 7.2
            r = add_real(x, y)
            call check_result(r, 12.3)
        end block
        
        instantiate add_t(integer, func_arg_int), only: add_integer => add_generic
        block
            integer :: a, b, r
            a = 5
            b = 9
            r = add_integer(a, b)
            call check_result(r, 14)
        end block
    end subroutine
end module

program template_add
    use template_add_m
    implicit none

    call test_template()

end program template_add
