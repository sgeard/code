! Ode solver
!
! Adapted from Numerical Recipes 77

submodule (solver) solver_sm
    
    real(8), parameter :: pi = 4*atan(1.0d0)
    real(8), parameter :: two_pi = 2*pi
    real(8), allocatable, target :: xp_data(:), yp_data(:,:)
    real(8), allocatable, target :: x_error(:,:)
   
contains
!
!   DOP835 adapted from dop853.f, see
!   https://www.unige.ch/~hairer/software.html
    module function rk_dop853(h,t,x,fp) result(xout)
        real(8), intent(in)             :: h, t
        real(8), intent(in), contiguous :: x(:,:)
        procedure(f_p), pointer         :: fp
        real(8)                         :: xout(size(x,1),size(x,2))
        
        real(8) :: c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14
        parameter (                                     &
            c2  = 0.526001519587677318785587544488D-01, &
            c3  = 0.789002279381515978178381316732D-01, &
            c4  = 0.118350341907227396726757197510D+00, &
            c5  = 0.281649658092772603273242802490D+00, &
            c6  = 0.333333333333333333333333333333D+00, &
            c7  = 0.25D+00,                             &
            c8  = 0.307692307692307692307692307692D+00, &
            c9  = 0.651282051282051282051282051282D+00, &
            c10 = 0.6D+00,                              &
            c11 = 0.857142857142857142857142857142D+00, &
            c12 = 0.1D+00,                              &
            c13 = 0.2D+00,                              &
            c14 = 0.777777777777777777777777777778D+00 )
            
        real(8) :: b1, b6, b7, b8, b9, b10, b11, b12
        parameter (                                     &
            b1 =   5.42937341165687622380535766363D-2,  &
            b6 =   4.45031289275240888144113950566D0,   &
            b7 =   1.89151789931450038304281599044D0,   &
            b8 =  -5.8012039600105847814672114227D0,    &
            b9 =   3.1116436695781989440891606237D-1,   &
            b10 = -1.52160949662516078556178806805D-1,  &
            b11 =  2.01365400804030348374776537501D-1,  &
            b12 =  4.47106157277725905176885569043D-2)
        
        real(8) :: bhh1, bhh2, bhh3
        parameter (                                      &
            bhh1 = 0.244094488188976377952755905512D+00, &
            bhh2 = 0.733846688281611857341361741547D+00, &
            bhh3 = 0.220588235294117647058823529412D-01)
            
        real(8) :: er1, er6, er7, er8, er9, er10, er11, er12 
        parameter (                                     &
            er1 =  0.1312004499419488073250102996D-01,  &
            er6 = -0.1225156446376204440720569753D+01,  &
            er7 = -0.4957589496572501915214079952D+00,  &
            er8 =  0.1664377182454986536961530415D+01,  &
            er9 = -0.3503288487499736816886487290D+00,  &
            er10 =  0.3341791187130174790297318841D+00, &
            er11 =  0.8192320648511571246570742613D-01, &
            er12 = -0.2235530786388629525884427845D-01)
            
        real(8) :: a21, a31, a32, a41, a43, a51, a53, a54, a61
        real(8) :: a64, a65, a71, a74, a75, a76
        parameter (                                     &
            a21 =  5.26001519587677318785587544488D-2,  &
            a31 =  1.97250569845378994544595329183D-2,  &
            a32 =  5.91751709536136983633785987549D-2,  &
            a41 =  2.95875854768068491816892993775D-2,  &
            a43 =  8.87627564304205475450678981324D-2,  &
            a51 =  2.41365134159266685502369798665D-1,  &
            a53 = -8.84549479328286085344864962717D-1,  &
            a54 =  9.24834003261792003115737966543D-1,  &
            a61 =    3.7037037037037037037037037037D-2, &
            a64 =    1.70828608729473871279604482173D-1,&
            a65 =    1.25467687566822425016691814123D-1,&
            a71 =    3.7109375D-2,                      &
            a74 =    1.70252211019544039314978060272D-1,&
            a75 =    6.02165389804559606850219397283D-2,&
            a76 =   -1.7578125D-2)
            
        real(8) :: a81, a84, a85, a86, a87, a91, a94, a95, a96, a97
        real(8) :: a98, a101, a104, a105, a106, a107, a108, a109
        parameter (                                      &
            a81 = 3.70920001185047927108779319836D-2,    &  
            a84 = 1.70383925712239993810214054705D-1,    &
            a85 = 1.07262030446373284651809199168D-1,    &
            a86 = -1.53194377486244017527936158236D-2,   &
            a87 = 8.27378916381402288758473766002D-3,    &
            a91 = 6.24110958716075717114429577812D-1,    &
            a94 = -3.36089262944694129406857109825D0,    &
            a95 =   -8.68219346841726006818189891453D-1, &
            a96 =    2.75920996994467083049415600797D1,  &
            a97 =    2.01540675504778934086186788979D1,  &
            a98 =   -4.34898841810699588477366255144D1,  &
            a101 =   4.77662536438264365890433908527D-1, &
            a104 =  -2.48811461997166764192642586468D0,  &
            a105 =  -5.90290826836842996371446475743D-1, &
            a106 =   2.12300514481811942347288949897D1,  &
            a107 =   1.52792336328824235832596922938D1,  &
            a108 =  -3.32882109689848629194453265587D1,  &
            a109 =  -2.03312017085086261358222928593D-2)
            
        real(8) :: a111, a114, a115, a116, a117, a118, a119, a1110
        real(8) :: a121, a124, a125, a126, a127, a128, a129, a1210, a1211
        parameter (                                     &
            a111 =  -9.3714243008598732571704021658D-1, &
            a114 =   5.18637242884406370830023853209D0, &
            a115 =   1.09143734899672957818500254654D0, &
            a116 =  -8.14978701074692612513997267357D0, &
            a117 =  -1.85200656599969598641566180701D1, &
            a118 =   2.27394870993505042818970056734D1, &
            a119 =   2.49360555267965238987089396762D0, &
            a1110 = -3.0467644718982195003823669022D0,  &
            a121 =   2.27331014751653820792359768449D0, &
            a124 =  -1.05344954667372501984066689879D1, &
            a125 =  -2.00087205822486249909675718444D0, &
            a126 =  -1.79589318631187989172765950534D1, &
            a127 =   2.79488845294199600508499808837D1, &
            a128 =  -2.85899827713502369474065508674D0, &
            a129 =  -8.87285693353062954433549289258D0, &
            a1210 =  1.23605671757943030647266201528D1, &
            a1211 =  6.43392746015763530355970484046D-1)
      
      real(8) :: a141, a147, a148, a149, a1410, a1411, a1412, a1413
      parameter (                                        &
            a141 =  5.61675022830479523392909219681D-2,  &
            a147 =  2.53500210216624811088794765333D-1,  &
            a148 = -2.46239037470802489917441475441D-1,  &
            a149 = -1.24191423263816360469010140626D-1,  &
            a1410 =  1.5329179827876569731206322685D-1,  &
            a1411 =  8.20105229563468988491666602057D-3, &
            a1412 =  7.56789766054569976138603589584D-3, &
            a1413 = -8.298D-3)
            
      real(8) :: a151, a156, a157, a158, a1511, a1512, a1513, a1514
      real(8) :: a161, a166, a167, a168, a169, a1613, a1614, a1615
      parameter (                                        &
            a151 =  3.18346481635021405060768473261D-2,  &
            a156 =  2.83009096723667755288322961402D-2,  &
            a157 =  5.35419883074385676223797384372D-2,  &
            a158 = -5.49237485713909884646569340306D-2,  &
            a1511 = -1.08347328697249322858509316994D-4, &
            a1512 =  3.82571090835658412954920192323D-4, &
            a1513 = -3.40465008687404560802977114492D-4, &
            a1514 =  1.41312443674632500278074618366D-1, &
            a161 = -4.28896301583791923408573538692D-1,  &
            a166 = -4.69762141536116384314449447206D0,   &
            a167 =  7.68342119606259904184240953878D0,   &
            a168 =  4.06898981839711007970213554331D0,   &
            a169 =  3.56727187455281109270669543021D-1,  &
            a1613 = -1.39902416515901462129418009734D-3, &
            a1614 =  2.9475147891527723389556272149D0,   &
            a1615 = -9.15095847217987001081870187138D0)
            
      real(8) :: d41, d46, d47, d48, d49, d410, d411, d412
      real(8) :: d413, d414, d415, d416
      parameter (                                        &
            d41  = -0.84289382761090128651353491142D+01, &
            d46  =  0.56671495351937776962531783590D+00, &
            d47  = -0.30689499459498916912797304727D+01, &
            d48  =  0.23846676565120698287728149680D+01, &
            d49  =  0.21170345824450282767155149946D+01, &
            d410 = -0.87139158377797299206789907490D+00, &
            d411 =  0.22404374302607882758541771650D+01, &
            d412 =  0.63157877876946881815570249290D+00, &
            d413 = -0.88990336451333310820698117400D-01, &
            d414 =  0.18148505520854727256656404962D+02, &
            d415 = -0.91946323924783554000451984436D+01, &
            d416 = -0.44360363875948939664310572000D+01)
            
      real(8) :: d51, d56, d57, d58, d59, d510, d511, d512
      real(8) :: d513, d514, d515, d516
      parameter (                                        &
            d51  =  0.10427508642579134603413151009D+02, &
            d56  =  0.24228349177525818288430175319D+03, &
            d57  =  0.16520045171727028198505394887D+03, &
            d58  = -0.37454675472269020279518312152D+03, &
            d59  = -0.22113666853125306036270938578D+02, &
            d510 =  0.77334326684722638389603898808D+01, &
            d511 = -0.30674084731089398182061213626D+02, &
            d512 = -0.93321305264302278729567221706D+01, &
            d513 =  0.15697238121770843886131091075D+02, &
            d514 = -0.31139403219565177677282850411D+02, &
            d515 = -0.93529243588444783865713862664D+01, &
            d516 =  0.35816841486394083752465898540D+02)
            
      real(8) :: d61, d66, d67, d68, d69, d610, d611
      real(8) :: d612, d613, d614, d615, d616
      parameter (                                    &
        d61 =  0.19985053242002433820987653617D+02,  &
        d66 = -0.38703730874935176555105901742D+03,  &
        d67 = -0.18917813819516756882830838328D+03,  &
        d68 =  0.52780815920542364900561016686D+03,  &
        d69 = -0.11573902539959630126141871134D+02,  &
        d610 =  0.68812326946963000169666922661D+01, &
        d611 = -0.10006050966910838403183860980D+01, &
        d612 =  0.77771377980534432092869265740D+00, &
        d613 = -0.27782057523535084065932004339D+01, &
        d614 = -0.60196695231264120758267380846D+02, &
        d615 =  0.84320405506677161018159903784D+02, &
        d616 =  0.11992291136182789328035130030D+02)
        
      real(8) :: d71, d76, d77, d78, d79, d710, d711
      real(8) :: d712, d713, d714, d715, d716
      parameter (                                    &
        d71  = -0.25693933462703749003312586129D+02, &
        d76  = -0.15418974869023643374053993627D+03, &
        d77  = -0.23152937917604549567536039109D+03, &
        d78  =  0.35763911791061412378285349910D+03, &
        d79  =  0.93405324183624310003907691704D+02, &
        d710 = -0.37458323136451633156875139351D+02, &
        d711 =  0.10409964950896230045147246184D+03, &
        d712 =  0.29840293426660503123344363579D+02, &
        d713 = -0.43533456590011143754432175058D+02, &
        d714 =  0.96324553959188282948394950600D+02, &
        d715 = -0.39177261675615439165231486172D+02, &
        d716 = -0.14972683625798562581422125276D+03)
        
        real(8), dimension(size(x,1),size(x,2)) ::   &
            k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12
            
        k1  = h*fp(t,         x)                                                                                                                                                                                        
        k2  = h*fp(t + c2*h,  x + a21*k1)                                                                                                                                                                               
        k3  = h*fp(t + c3*h,  x + a31*k1 + a32*k2)                                                                                                                                                                      
        k4  = h*fp(t + c4*h,  x + a41*k1 + a43*k3)                                                                                                                                                                    
        k5  = h*fp(t + c5*h,  x + a51*k1 + a53*k3 + a54*k4)                                                                                                                                                             
        k6  = h*fp(t + c6*h,  x + a61*k1 + a64*k4 + a65*k5)                                                                                                                                                           
        k7  = h*fp(t + c7*h,  x + a71*k1 + a74*k4 + a75*k5 + a76*k6)                                                                                                                                                    
        k8  = h*fp(t + c8*h,  x + a81*k1 + a84*k4 + a85*k5 + a86*k6 + a87*k7)                                                                                                                                           
        k9  = h*fp(t + c9*h,  x + a91*k1 + a94*k4 + a95*k5 + a96*k6 + a97*k7 + a98*k8)                                                                                                                                  
        k10 = h*fp(t + c10*h, x + a101*k1 + a104*k4 + a105*k5 + a106*k6 &                                                                                                                                               
                                + a107*k7 + a108*k8 + a109*k9)                                                                                                                                                         
        k11 = h*fp(t + c11*h, x + a111*k1 + a114*k4 + a115*k5 + a116*k6 &                                                                                                                                               
                                + a117*k7 + a118*k8 + a119*k9 + a1110*k10)                                                                                                                                             
        k12 = h*fp(t + h,     x + a121*k1 + a124*k4 + a125*k5 + a126*k6 &                                                                                                                                               
                                + a127*k7 + a128*k8 + a129*k9 + a1210*k10 + a1211*k11)                                                                                                                                 
                                                                                                                                                                                                                  
        xout    = x + b1*k1 + b6*k6 + b7*k7 + b8*k8 + b9*k9 + b10*k10 + b11*k11 + b12*k12                                                                                                                               
        x_error = er1*k1 + er6*k6 + er7*k7 + er8*k8 + er9*k9 + er10*k10 + er11*k11 + er12*k12                                                                                                                         

    end function rk_dop853

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
                   b31=3/40.0d0, b32=9/40.0d0,                                                              &
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
    
    ! Yoshida-Forest-Ruth 4th-order symplectic integrator
    ! Ref: Yoshida (1990), Phys. Lett. A 150, 262
    ! Suitable for separable Hamiltonians H(q,p) = T(p) + V(q)
    ! State vector x(1:npos,:) = positions, x(npos+1:2*npos,:) = velocities
    module function yfr(h,t,x,fp) result(xout)
        real(8), intent(in)             :: h, t
        real(8), intent(in), contiguous :: x(:,:)
        procedure(f_p), pointer         :: fp
        real(8)                         :: xout(size(x,1),size(x,2))
        real(8), dimension(size(x,1),size(x,2)) :: dxdt
        integer  :: npos
        real(8), parameter :: cr = 2.0d0**(1.0d0/3.0d0)
        real(8), parameter :: w1 = 1.0d0/(2.0d0 - cr)
        real(8), parameter :: w0 = -cr*w1
        real(8), parameter :: c1 = w1/2.0d0
        real(8), parameter :: c4 = c1
        real(8), parameter :: c2 = (w0 + w1)/2.0d0
        real(8), parameter :: c3 = c2
        real(8), parameter :: d1 = w1
        real(8), parameter :: d3 = d1
        real(8), parameter :: d2 = w0

        npos = size(x,1)/2
        xout = x

        ! Drift c1
        xout(1:npos,:) = xout(1:npos,:) + c1*h*xout(npos+1:2*npos,:)
        ! Kick d1
        dxdt = fp(t, xout)
        xout(npos+1:2*npos,:) = xout(npos+1:2*npos,:) + d1*h*dxdt(npos+1:2*npos,:)
        ! Drift c2
        xout(1:npos,:) = xout(1:npos,:) + c2*h*xout(npos+1:2*npos,:)
        ! Kick d2
        dxdt = fp(t, xout)
        xout(npos+1:2*npos,:) = xout(npos+1:2*npos,:) + d2*h*dxdt(npos+1:2*npos,:)
        ! Drift c3
        xout(1:npos,:) = xout(1:npos,:) + c3*h*xout(npos+1:2*npos,:)
        ! Kick d3
        dxdt = fp(t, xout)
        xout(npos+1:2*npos,:) = xout(npos+1:2*npos,:) + d3*h*dxdt(npos+1:2*npos,:)
        ! Drift c4
        xout(1:npos,:) = xout(1:npos,:) + c4*h*xout(npos+1:2*npos,:)
    end function yfr

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
        c1 = 0.5d0
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
