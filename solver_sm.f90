! Ode solver
!
! Adapted from Numerical Recipes 77

submodule (solver) solver_sm
    
    integer, parameter :: MAXSTP = 1000000
    integer, parameter :: KMAX = 10000
    real(8), parameter :: pi = 4*atan(1.0d0)
    real(8), parameter :: two_pi = 2*pi
    integer                      :: kount
    real(8)                      :: dxsav
    real(8), allocatable, target :: xp_data(:), yp_data(:,:)
    real(8), allocatable, target :: x_error(:,:)
   
contains
!
!    module subroutine odeint(ystart,x1,x2,eps,h1,hmin,nok,nbad,derivs)
!        real(8), intent(inout)   :: ystart(:)
!        real(8), intent(in)      :: x1, x2, eps, h1, hmin
!        integer                  :: nbad, nok, nvar
!        procedure(derivatives_p) :: derivs
!        real(8), allocatable     :: dydx(:)
!
!        real(8), parameter       :: small = 1.0d-30
!        integer                  :: kount, nstp
!        real(8)                  :: dxsav, h, hdid, hnext, x, xsav
!        real(8), allocatable     :: y(:), yscal(:)
!        real(8), allocatable     :: xp(:), yp(:,:)
!
!        nvar = size(ystart)
!        allocate(xp(KMAX), yp(nvar,KMAX))
!        allocate(dydx(nvar))
!
!        dxsav = (x2-x1)/kmax
!        x = x1
!        h = sign(h1,x2-x1)
!        nok = 0
!        nbad = 0
!        kount = 0
!        y = ystart
!        if (kmax > 0) xsav = x-2*dxsav
!        do nstp=1,MAXSTP
!            call derivs(x,y,dydx)
!            
!            yscal = abs(y) + abs(h*dydx) + small
!            
!            if(kmax > 0) then
!              if(abs(x-xsav) > abs(dxsav)) then
!                  if(kount < kmax-1)then
!                    kount = kount+1
!                    xp(kount) = x
!                    yp(:,kount) = y
!                    xsav = x
!                  endif
!              endif
!            endif
!            if((x+h-x2)*(x+h-x1) > 0) h=x2-x
!            call rkqs(y,dydx,nvar,x,h,eps,yscal,hdid,hnext,derivs)
!            if(hdid == h) then
!                nok=nok+1
!            else
!                nbad=nbad+1
!            endif
!            if((x-x2)*(x2-x1) >= 0) then
!                ystart = y
!                if(kount < kmax) then
!                    kount = kount+1
!                    xp(kount) = x
!                    yp(:,kount) = y
!                endif
!                xp_data = xp(1:kount)
!                yp_data = yp(:,1:kount)
!                return
!            endif
!            if(abs(hnext) < hmin) then
!                stop 'stepsize smaller than minimum in odeint'
!            end if
!            h = hnext
!        end do
!        stop 'too many steps in odeint'
!        return
!    end subroutine odeint
!!
!    module subroutine rkqs(y, dydx, n, x, htry, eps, yscal, hdid, hnext, derivs)
!        integer, intent(in)      :: n
!        real(8), intent(in)      :: eps, htry, dydx(n), yscal(n)
!        real(8), intent(out)     :: hnext, hdid, y(n)
!        real(8), intent(inout)   :: x
!        procedure(derivatives_p) :: derivs
!
!        real(8)              :: errmax, h, htemp, xnew
!        real(8), allocatable :: yerr(:), ytemp(:)
!        real(8), parameter   :: SAFETY = 0.9d0
!        real(8), parameter   :: PGROW = -0.2d0
!        real(8), parameter   :: PSHRNK = -0.25d0
!        real(8), parameter   :: ERRCON = 1.89d-4
!      
!        h = htry
!        do
!            call rk_cash_karp_original(y,dydx,x,h,ytemp,yerr,derivs)
!            errmax = maxval(abs(yerr/yscal))/eps
!            if(errmax > 1) then
!                htemp = SAFETY*h*(errmax**PSHRNK)
!                h = sign(max(abs(htemp),0.1d0*abs(h)),h)
!                if (abs(h) < epsilon(h)) then
!                    stop 'stepsize underflow in rkqs'
!                end if
!                xnew = x + h
!                cycle
!            else
!                if(errmax > ERRCON) then
!                    hnext = SAFETY*h*(errmax**PGROW)
!                else
!                    hnext = 5*h
!                endif
!                hdid = h
!                x = x+h
!                y = ytemp
!                exit
!            endif
!        end do
!    end subroutine rkqs
!
!   Cash-Karp - an adaptive Runge-Kutta method
!   see https://en.wikipedia.org/wiki/Cash%E2%80%93Karp_method
    module function rk_cash_karp(h,t,x,fp) result(xout)
        real(8), intent(in)             :: h, t
        real(8), intent(in), contiguous :: x(:,:)
        procedure(f_p), pointer         :: fp
        real(8)                         :: xout(size(x,1),size(x,2))
        real(8), dimension(size(x,1),size(x,2)) :: k1, k2, k3, k4, k5, k6
        real(8) :: a2,a3,a4,a5,a6,b21,b31,b32,b41,b42,b43,b51,b52,b53
        real(8) :: b54,b61,b62,b63,b64,b65,c1,c3,c4,c6,dc1,dc3,dc4,dc5,dc6
     
        parameter (a2=1/5.0d0, a3=3/10.0d0, a4=3/5.0d0, a5=1.0d0 ,a6=7/8.0d0,                               &
                   b21=1/5.0d0,                                                                             &
                   b31=3./40., b32=9./40.,                                                                  &
                   b41=.3, b42=-.9, b43=1.2,                                                                &
                   b51=-11./54., b52=2.5, b53=-70./27., b54=35./27.,                                        &
                   b61=1631./55296.,b62=175./512., b63=575./13824.,b64=44275./110592.,b65=253./4096.,       &
                   c1=37./378., c3=250./621., c4=125./594., c6=512./1771.,                                  &
                   dc1=c1-2825./27648., dc3=c3-18575./48384., dc4=c4-13525./55296., dc5=-277./14336., dc6=c6-.25)
        
        k1 = h*fp(t         , x   )
        k2 = h*fp(t + a2*h  , x + b21*k1  )
        k3 = h*fp(t + a3*h  , x + b31*k1 + b32*k2  )
        k4 = h*fp(t + a4*h  , x + b41*k1 + b42*k2 + b43*k3   )
        k5 = h*fp(t + a5*h  , x + b51*k1 + b52*k2 + b53*k3 + b54*k4   )
        k6 = h*fp(t + a6*h  , x + b61*k1 + b62*k2 + b63*k3 + b64*k4 + b65*k5   )
        xout = x + c1*k1 + c3*k3 + c4*k4 + c6*k6
        x_error = dc1*k1 + dc3*k3 + dc4*k4 + dc5*k5 + dc6*k6
    end function rk_cash_karp

    module function rk_get_error() result(r)
        real(8), dimension(:,:), pointer :: r
        if (allocated(x_error)) then
            r => x_error
        else
            nullify(r)
        end if
    end function rk_get_error
        
    ! Explicit 4th order Runge-Kutta method
    module function rk_exp4(h,t,x,fp) result(xout)
        real(8), intent(in)             :: h, t
        real(8), intent(in), contiguous :: x(:,:)
        procedure(f_p), pointer :: fp
        real(8)                         :: xout(size(x,1),size(x,2))
        real(8), dimension(size(x,1),size(x,2)) :: k1, k2, k3, k4
        k1   = h*fp(t    , x     )
        k2   = h*fp(t+h/2, x+k1/2)
        k3   = h*fp(t+h/2, x+k2/2)
        k4   = h*fp(t+h  , x+k3  )
        xout = x + k1/6 + k2/3 + k3/3 + k4/6
    end function rk_exp4

    ! Explicit 4th order Runge-Kutta method - the 3/8 rule
    module function rk_exp38(h,t,x,fp) result(xout)
        real(8), intent(in)             :: h, t
        real(8), intent(in), contiguous :: x(:,:)
        procedure(f_p), pointer :: fp
        real(8)                         :: xout(size(x,1),size(x,2))
        real(8), dimension(size(x,1),size(x,2)) :: k1, k2, k3, k4
        k1   = h*fp(t      , x     )
        k2   = h*fp(t+h/3  , x + k1/3)
        k3   = h*fp(t+2*h/3, x - k1/3 + k2)
        k4   = h*fp(t+h    , x + k1 - k2 + k3  )
        xout = x + k1/8 + 3*k2/8 + 3*k3/8 + k4/8
    end function rk_exp38
    
    module subroutine four1(data,nn,isign)
        integer, intent(in) :: isign, nn
        real(8) :: data(2*nn)
        integer :: i,istep,j,m,mmax,n
        real(8) :: temp(2)
        real(8) :: theta,wi,wpi,wpr,wr,wtemp
        n = 2*nn
        j = 1
        do i=1,n,2
            if(j > i) then
              temp = data(j:j+1)
              data(j:j+1) = data(i:i+1)
              data(i:i+1) = temp
            endif
            m = n/2
            do
                if((m < 2) .or. (j <= m)) exit
                j = j-m
                m = m/2
            end do
            j = j+m
        end do
        
        mmax = 2
        do
            if (n <= mmax) exit
            istep = 2*mmax
            theta = two_pi/(isign*mmax)
            wpr = -2*sin(0.5d0*theta)**2
            wpi = sin(theta)
            wr = 1
            wi = 0
            do m=1,mmax,2
                do i=m,n,istep
                    j=i+mmax
                    temp = [wr*data(j)-wi*data(j+1), wr*data(j+1)+wi*data(j)]
                    data(j:j+1) = data(i:i+1) - temp
                    data(i:i+1) = data(i:i+1) + temp
                end do
                wtemp = wr
                wr = wr*wpr - wi*wpi+wr
                wi = wi*wpr + wtemp*wpi+wi
            end do
            mmax = istep
        end do
        return
    end subroutine four1
!
    module subroutine realft(data, isign)
        integer, intent(in)     :: isign
        real(8), intent(inout) :: data(:)
  
        integer  :: i,i1,i2,i3,i4,n2p3, n
        real(8) :: c1,c2,h1i,h1r,h2i,h2r
        real(8) :: theta,wi,wpi,wpr,wr,wtemp
        
        n = size(data)
        theta = 2*pi/n
        c1 = 0.5
        if (isign == 1) then
            c2 = -0.5d0
            call four1(data,n/2,+1)
        else
            c2 = 0.5d0
            theta = -theta
        end if
        wpr = -2*sin(0.5d0*theta)**2
        wpi = sin(theta)
        wr = 1 + wpr
        wi = wpi
        n2p3 = n+3
        do i=2,n/4
            i1 = 2*i-1
            i2 = i1+1
            i3 = n2p3-i2
            i4 = i3+1
            h1r = c1*(data(i1)+data(i3))
            h1i = c1*(data(i2)-data(i4))
            h2r = -c2*(data(i2)+data(i4))
            h2i = c2*(data(i1)-data(i3))
            data(i1) = h1r+wr*h2r-wi*h2i
            data(i2) = h1i+wr*h2i+wi*h2r
            data(i3) = h1r-wr*h2r+wi*h2i
            data(i4) = -h1i+wr*h2i+wi*h2r
            wtemp = wr
            wr = wr*wpr-wi*wpi+wr
            wi = wi*wpr+wtemp*wpi+wi
        end do
        if (isign == 1) then
            h1r = data(1)
            data(1) = h1r+data(2)
            data(2) = h1r-data(2)
        else
            h1r = data(1)
            data(1) = c1*(h1r+data(2))
            data(2) = c1*(h1r-data(2))
            call four1(data,n/2,-1)
        end if
        return
    end subroutine realft

end submodule solver_sm
