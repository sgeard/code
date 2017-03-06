! Some very basic vector analysis operations using autodiff for vectors in the x-y plane

! grad, div, curl, del_squared

module vector_analysis
    use auto_diff
    implicit none
    
    abstract interface
        function f(x, y)
            import
            type(auto_var) :: f
            type(auto_var), intent(in) :: x, y
        end function f
    end interface
    
contains

    ! Calculate the gradient
    function grad(x, y, phi)
        real(8) :: grad(2)
        real(8), intent(in) :: x, y
        procedure(f)        :: phi
        type(auto_var) :: xa, ya, ra
        
        ! x-component varies, y-component constant
        call xa%set(x)
        call ya%set_constant(y)
        ra = phi(xa, ya)
        grad(1) = ra%get_derivative()

        ! y-component varies, x-component constant
        call xa%set_constant(x)
        call ya%set(y)
        ra = phi(xa, ya)
        grad(2) = ra%get_derivative()
        
    end function grad

    ! Calculate the divergence
    function div(x, y, phi)
        real(8) :: div
        real(8) :: grad(2)
        real(8), intent(in) :: x, y
        procedure(f)        :: phi
        type(auto_var) :: xa, ya, ra
        
        ! x-component varies, y-component constant
        call xa%set(x)
        call ya%set_constant(y)
        ra = phi(xa, ya)
        grad(1) = ra%get_derivative()

        ! y-component varies, x-component constant
        call xa%set_constant(x)
        call ya%set(y)
        ra = phi(xa, ya)
        grad(2) = ra%get_derivative()
        
        div = sum(grad)
    end function div

    ! Calculate the del_squared (== div(grad))
    function del_squared(x, y, phi)
        real(8) :: del_squared
        real(8) :: grad(2)
        real(8), intent(in) :: x, y
        procedure(f)        :: phi
        type(auto_var) :: xa, ya, ra
        
        ! x-component varies, y-component constant
        call xa%set(x)
        call ya%set_constant(y)
        ra = phi(xa, ya)
        grad(1) = ra%get_derivative()

        ! y-component varies, x-component constant
        call xa%set_constant(x)
        call ya%set(y)
        ra = phi(xa, ya)
        grad(2) = ra%get_derivative()
        
        del_squared = sum(grad**2)
    end function del_squared

    ! This is actually the z-component of curl
    function curl(x, y, phi)
        real(8) :: curl
        real(8) :: grad(2)
        real(8), intent(in) :: x, y
        procedure(f)        :: phi
        type(auto_var) :: xa, ya, ra
        
        ! x-component varies, y-component constant
        call xa%set(x)
        call ya%set_constant(y)
        ra = phi(xa, ya)
        grad(1) = ra%get_derivative()

        ! y-component varies, x-component constant
        call xa%set_constant(x)
        call ya%set(y)
        ra = phi(xa, ya)
        grad(2) = ra%get_derivative()
        
        curl = grad(1) - grad(2)
    end function curl

    ! Evaluate the given function
    function evaluate(x, y, phi)
        real(8) :: evaluate
        real(8), intent(in) :: x, y
        procedure(f)        :: phi
        type(auto_var) :: xa, ya, ra
        
        ! Make parameters constant - no derivatives
        call xa%set_constant(x)
        call ya%set_constant(y)
        ra = phi(xa, ya)
        evaluate = ra%get_value()
    end function evaluate

end module vector_analysis
