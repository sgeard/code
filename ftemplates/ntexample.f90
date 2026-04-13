module template_add_m
    implicit none
    private
    public :: add_t, test_template

    interface add_t
        module procedure func_arg_real
        module procedure func_arg_int
    end interface add_t

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
            real :: x, y, r
            x = 5.1
            y = 7.2
            r = add_t(x,y)
            call check_result(r,12.3)
        end block
        
        block
            integer :: a,b, r
            a = 5
            b = 9
            r = add_t(a, b)
            call check_result(r, 14)
        end block
    end subroutine test_template
end module

program template_add
    use template_add_m
    implicit none

    call test_template()

end program template_add
