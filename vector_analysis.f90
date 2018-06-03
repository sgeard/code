! Some very basic vector analysis operations using autodiff for vectors in the x-y plane

! grad, div, curl, del_squared

module vector_analysis
    use auto_diff
    implicit none
    
    abstract interface
        function f(p)
            import
            type(auto_var) :: f
            type(auto_var), intent(in) :: p(3)
        end function f
    end interface
    
    interface
        module function grad(p, phi) result(r)
            real(8) :: r(3)
            real(8), intent(in) :: p(3)
            procedure(f)        :: phi
        end function grad
      
        module function div(p, phi)
            real(8) :: div
            real(8), intent(in) :: p(3)
            procedure(f)        :: phi
        end function div

        !module function del_squared(p, phi)
        !    real(8) :: del_squared
        !    real(8), intent(in) :: p(3)
        !    procedure(f)        :: phi
        !end function del_squared
        !
        !module function curl(p, phi)
        !    real(8) :: curl
        !    real(8), intent(in) :: p(3)
        !    procedure(f)        :: phi
        !end function curl
        
        module function evaluate(p, phi)
            real(8) :: evaluate
            real(8), intent(in) :: p(3)
            procedure(f)        :: phi
        end function evaluate

    end interface  
         
    ! Access the cross product as an operator so that the code reads better
    interface operator(.cross.)
        module pure function cross(a, b) result(r)
            real(8), intent(in) :: a(3), b(3)
            real(8) :: r(3)
        end function cross
    end interface
       
end module vector_analysis
