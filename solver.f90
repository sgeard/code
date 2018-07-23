! Ode solver
!
! Adapted from Numerical Recipes 77

module solver
    
    implicit none

    private

    public rk_exp4, rk_exp38, rk_cash_karp, rk_get_error, realft
    public f_p, derivatives_p, solver_p
    
    abstract interface
         
        function f_p(t,x) result(dxdt)
            real(8), intent(in)             :: t
            real(8), intent(in), contiguous :: x(:,:)
            real(8)                         :: dxdt(size(x,1),size(x,2))
        end function f_p
        
        subroutine derivatives_p(t, y, dydx)
            real(8), intent(in)  :: t
            real(8), intent(in)  :: y(:)
            real(8), intent(out) :: dydx(:)
        end subroutine derivatives_p
        
       function solver_p(h,t,x,fp) result(xout)
            import
            real(8), intent(in)             :: h, t
            real(8), intent(in), contiguous :: x(:,:)
            procedure(f_p), pointer         :: fp
            real(8)                         :: xout(size(x,1),size(x,2))
        end function solver_p
        
    end interface
    
    
    interface
        
        module subroutine odeint(ystart,x1,x2,eps,h1,hmin,nok,nbad,derivs)
            real(8), intent(inout)   :: ystart(:)
            real(8), intent(in)      :: x1, x2, eps, h1, hmin
            integer                  :: nbad, nok
            procedure(derivatives_p) :: derivs
        end subroutine odeint

        module subroutine rkqs(y,dydx,n,x,htry,eps,yscal,hdid,hnext,derivs)
            integer, intent(in)      :: n
            real(8), intent(in)      :: eps, htry, dydx(n), yscal(n)
            real(8), intent(out)     :: hnext, hdid, y(n)
            real(8), intent(inout)   :: x
            procedure(derivatives_p) :: derivs
        end subroutine rkqs
        
        module subroutine rk_cash_karp_original(y,dydx,t,h,yout,yerr,derivs)
            real(8), intent(in)               :: h, t, dydx(:), y(:)
            real(8), allocatable, intent(out) :: yerr(:), yout(:)
            procedure(derivatives_p)          :: derivs
        end subroutine rk_cash_karp_original
        
        module function rk_cash_karp(h,t,x,fp) result(xout)
            real(8), intent(in)             :: h, t
            real(8), intent(in), contiguous :: x(:,:)
            procedure(f_p), pointer :: fp
            real(8)                         :: xout(size(x,1),size(x,2))
        end function rk_cash_karp

        module function rk_get_error() result(r)
            real(8), dimension(:,:), pointer :: r
        end function rk_get_error
        
        module function rk_exp4(h,t,x,fp) result(xout)
            real(8), intent(in)             :: h, t
            real(8), intent(in), contiguous :: x(:,:)
            procedure(f_p), pointer :: fp
            real(8)                         :: xout(size(x,1),size(x,2))
        end function rk_exp4
        
        module function rk_exp38(h,t,x,fp) result(xout)
            real(8), intent(in)             :: h, t
            real(8), intent(in), contiguous :: x(:,:)
            procedure(f_p), pointer :: fp
            real(8)                         :: xout(size(x,1),size(x,2))
        end function rk_exp38
        
        module subroutine four1(data,nn,isign)
            integer, intent(in) :: isign, nn
            real(8) :: data(2*nn)
        end subroutine four1

        module subroutine realft(data, isign)
            integer, intent(in)     :: isign
            real(8), intent(inout) :: data(:)
        end subroutine realft
    end interface

 
end module solver
