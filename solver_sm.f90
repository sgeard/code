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
   
contains
!
    module subroutine odeint(ystart,x1,x2,eps,h1,hmin,nok,nbad,derivs)
        real(8), intent(inout)   :: ystart(:)
        real(8), intent(in)      :: x1, x2, eps, h1, hmin
        integer                  :: nbad, nok, nvar
        procedure(derivatives_p) :: derivs
        real(8), allocatable     :: dydx(:)

        real(8), parameter       :: small = 1.0d-30
        integer                  :: kount, nstp
        real(8)                  :: dxsav, h, hdid, hnext, x, xsav
        real(8), allocatable     :: y(:), yscal(:)
        real(8), allocatable     :: xp(:), yp(:,:)

        nvar = size(ystart)
        allocate(xp(KMAX), yp(nvar,KMAX))
        allocate(dydx(nvar))

        dxsav = (x2-x1)/kmax
        x = x1
        h = sign(h1,x2-x1)
        nok = 0
        nbad = 0
        kount = 0
        y = ystart
        if (kmax > 0) xsav = x-2*dxsav
        do nstp=1,MAXSTP
            call derivs(x,y,dydx)
            
            yscal = abs(y) + abs(h*dydx) + small
            
            if(kmax > 0) then
              if(abs(x-xsav) > abs(dxsav)) then
                  if(kount < kmax-1)then
                    kount = kount+1
                    xp(kount) = x
                    yp(:,kount) = y
                    xsav = x
                  endif
              endif
            endif
            if((x+h-x2)*(x+h-x1) > 0) h=x2-x
            call rkqs(y,dydx,nvar,x,h,eps,yscal,hdid,hnext,derivs)
            if(hdid == h) then
                nok=nok+1
            else
                nbad=nbad+1
            endif
            if((x-x2)*(x2-x1) >= 0) then
                ystart = y
                if(kount < kmax) then
                    kount = kount+1
                    xp(kount) = x
                    yp(:,kount) = y
                endif
                xp_data = xp(1:kount)
                yp_data = yp(:,1:kount)
                return
            endif
            if(abs(hnext) < hmin) then
                stop 'stepsize smaller than minimum in odeint'
            end if
            h = hnext
        end do
        stop 'too many steps in odeint'
        return
    end subroutine odeint
!
    module subroutine rkqs(y, dydx, n, x, htry, eps, yscal, hdid, hnext, derivs)
        integer, intent(in)      :: n
        real(8), intent(in)      :: eps, htry, dydx(n), yscal(n)
        real(8), intent(out)     :: hnext, hdid, y(n)
        real(8), intent(inout)   :: x
        procedure(derivatives_p) :: derivs

        real(8)              :: errmax, h, htemp, xnew
        real(8), allocatable :: yerr(:), ytemp(:)
        real(8), parameter   :: SAFETY = 0.9d0
        real(8), parameter   :: PGROW = -0.2d0
        real(8), parameter   :: PSHRNK = -0.25d0
        real(8), parameter   :: ERRCON = 1.89d-4
      
        h = htry
        do
            call rk_cash_karp(y,dydx,x,h,ytemp,yerr,derivs)
            errmax = maxval(abs(yerr/yscal))/eps
            if(errmax > 1) then
                htemp = SAFETY*h*(errmax**PSHRNK)
                h = sign(max(abs(htemp),0.1d0*abs(h)),h)
                if (abs(h) < epsilon(h)) then
                    stop 'stepsize underflow in rkqs'
                end if
                xnew = x + h
                cycle
            else
                if(errmax > ERRCON) then
                    hnext = SAFETY*h*(errmax**PGROW)
                else
                    hnext = 5*h
                endif
                hdid = h
                x = x+h
                y = ytemp
                exit
            endif
        end do
    end subroutine rkqs
!
!   Cash-Karp - an adaptive Runge-Kutta method
!   see https://en.wikipedia.org/wiki/Cash%E2%80%93Karp_method
    module subroutine rk_cash_karp(y, dydx, x, h, yout, yerr, derivs)

        real(8), intent(in)               :: h, x, dydx(:), y(:)
        real(8), allocatable, intent(out) :: yerr(:), yout(:)
        
        procedure(derivatives_p)          :: derivs

        real(8) :: ak2(size(y)),ak3(size(y)),ak4(size(y)),ak5(size(y)),ak6(size(y))
        real(8) :: ytemp(size(y)),A2,A3,A4,A5,A6,B21,B31,B32,B41,B42,B43,B51,B52,B53
        real(8) :: B54,B61,B62,B63,B64,B65,C1,C3,C4,C6,DC1,DC3,DC4,DC5,DC6
     
        parameter (A2=.2,A3=.3,A4=.6,A5=1.,A6=.875,B21=.2,B31=3./40.,              &
                   B32=9./40.,B41=.3,B42=-.9,B43=1.2,B51=-11./54.,B52=2.5,         &
                   B53=-70./27.,B54=35./27.,B61=1631./55296.,B62=175./512.,        &
                   B63=575./13824.,B64=44275./110592.,B65=253./4096.,C1=37./378.,  &
                   C3=250./621.,C4=125./594.,C6=512./1771.,DC1=C1-2825./27648.,    &
                   DC3=C3-18575./48384.,DC4=C4-13525./55296.,DC5=-277./14336.,     &
                   DC6=C6-.25)
     
        ytemp = y + B21*h*dydx

        call derivs(x+A2*h,ytemp,ak2)

        ytemp = y + h*(B31*dydx + B32*ak2)

        call derivs(x+A3*h,ytemp,ak3)

        ytemp =y+h*(B41*dydx+B42*ak2+B43*ak3)

        call derivs(x+A4*h,ytemp,ak4)

        ytemp=y+h*(B51*dydx+B52*ak2+B53*ak3+B54*ak4)

        call derivs(x+A5*h,ytemp,ak5)

        ytemp=y+h*(B61*dydx+B62*ak2+B63*ak3+B64*ak4+B65*ak5)

        call derivs(x+A6*h,ytemp,ak6)

        yout=y+h*(C1*dydx+C3*ak3+C4*ak4+C6*ak6)

        yerr=h*(DC1*dydx+DC3*ak3+DC4*ak4+DC5*ak5+DC6*ak6)

        return
    end subroutine rk_cash_karp

    ! Explicit 4th order Runge-Kutta method
    module function rk_exp4(h,t,x,fp) result(xout)
        real(8), intent(in)             :: h, t
        real(8), intent(in), contiguous :: x(:,:)
        procedure(f_p), pointer :: fp
        real(8)                         :: xout(size(x,1),size(x,2))
        real(8), dimension(size(x,1),size(x,2)) :: k1, k2, k3, k4
        k1   = h*fp(t    ,x     )
        k2   = h*fp(t+h/2,x+k1/2)
        k3   = h*fp(t+h/2,x+k2/2)
        k4   = h*fp(t+h  ,x+k3  )
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
