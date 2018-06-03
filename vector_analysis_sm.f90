! Implementation code for vector_analysis module
submodule (vector_analysis) vector_analysis_sm

contains

    ! Calculate the gradient
    module function grad(p, phi) result(r)
        real(8) :: r(3)
        real(8), intent(in) :: p(3)
        procedure(f)        :: phi
        type(auto_var) :: q(3), s
        integer        :: i
                
        do i=1,3
            call make_var(p, q, i)
            s = phi(q)
            r(i) = s%get_derivative()
        end do
        
    end function grad

    ! Calculate the divergence
    module function div(p, phi)
        real(8) :: div
        real(8), intent(in) :: p(3)
        procedure(f)        :: phi
        real(8) :: q(3)
        
        q = grad(p, phi)
        
        div = sum(q)
    end function div

    !! Calculate the del_squared (== div(grad))
    !module function del_squared(x, y, phi)
    !    real(8) :: del_squared
    !    real(8), intent(in) :: x, y
    !    procedure(f)        :: phi
    !    real(8) :: grad(2)
    !    type(auto_var) :: xa, ya, ra
    !    
    !    ! x-component varies, y-component constant
    !    call xa%set(x)
    !    call ya%set_constant(y)
    !    ra = phi(xa, ya)
    !    grad(1) = ra%get_derivative()
    !
    !    ! y-component varies, x-component constant
    !    call xa%set_constant(x)
    !    call ya%set(y)
    !    ra = phi(xa, ya)
    !    grad(2) = ra%get_derivative()
    !    
    !    del_squared = sum(grad**2)
    !end function del_squared

    ! This is actually the z-component of curl
    !module function curl(x, y, phi)
    !    real(8) :: curl
    !    real(8), intent(in) :: x, y
    !    procedure(f)        :: phi
    !    real(8) :: grad(2)
    !    type(auto_var) :: xa, ya, ra
    !    
    !    ! x-component varies, y-component constant
    !    call xa%set(x)
    !    call ya%set_constant(y)
    !    ra = phi(xa, ya)
    !    grad(1) = ra%get_derivative()
    !
    !    ! y-component varies, x-component constant
    !    call xa%set_constant(x)
    !    call ya%set(y)
    !    ra = phi(xa, ya)
    !    grad(2) = ra%get_derivative()
    !    
    !    curl = grad(1) - grad(2)
    !end function curl


    ! Evaluate the given function
    module function evaluate(p, phi)
        real(8) :: evaluate
        real(8), intent(in) :: p(3)
        procedure(f)        :: phi
        type(auto_var) :: q(3), ra
        integer :: i
        
        ! Make parameters constant - no derivatives
        do i=1,3
            call q(i)%set_constant(p(i))
        end do
        ra = phi(q)
        evaluate = ra%get_value()
    end function evaluate

    ! Cross product of two vectors
    module pure function cross(a, b) result(r)
        real(8), intent(in) :: a(3), b(3)
        real(8) :: r(3)
        integer :: i, j, k
        
        do i=1,3
            j = merge(1,i+1,i==3)
            k = merge(1,j+1,j==3)
            r(i) = a(j)*b(k)-a(k)*b(j)
        end do
    end function cross

end submodule vector_analysis_sm