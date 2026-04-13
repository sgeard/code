! Created by  on 22/02/2020.

module AG

    integer, parameter, private :: max_derivative = 4;

    type g_auto_var(n)
        integer, len :: n
        real(8)      :: v
        real(8)      :: d(n)
    contains
        procedure            :: set => set_gav
        procedure            :: set_from_gav
        generic, public      :: assignment(=) => set_from_gav
        procedure            :: gav_times_gav
        procedure            :: gav_times_r
        procedure            :: gav_times_i
        procedure, pass(obj) :: i_times_gav
        procedure, pass(obj) :: r_times_gav
        generic, public      :: operator(*) => gav_times_gav, gav_times_r, gav_times_i, i_times_gav, r_times_gav
        procedure            :: gav_plus_gav
        procedure            :: gav_plus_r
        procedure            :: gav_plus_i
        procedure, pass(obj) :: i_plus_gav
        procedure, pass(obj) :: r_plus_gav
        generic, public      :: operator(+) => gav_plus_gav, gav_plus_r, gav_plus_i, i_plus_gav, r_plus_gav
        procedure            :: gav_minus_gav
        procedure            :: gav_minus_r
        procedure            :: gav_minus_i
        procedure            :: uminus_gav
        procedure, pass(obj) :: i_minus_gav
        procedure, pass(obj) :: r_minus_gav
        generic, public      :: operator(-) => gav_minus_gav, gav_minus_r, gav_minus_i, uminus_gav, i_minus_gav, r_minus_gav
        procedure            :: print_gav
        procedure            :: print_gav_s
        generic, public      :: print => print_gav, print_gav_s
    end type g_auto_var

    ! == Generic functions =======================================================================
    abstract interface
        pure function f_t(x) result(r)
            real(8)             :: r
            real(8), intent(in) :: x
        end function f_t
    end interface

    interface operator(.mult.)
        module procedure i_times_gav, gav_times_i, r_times_gav, gav_times_r, gav_times_gav
    end interface

    interface operator(.plus.)
        module procedure i_plus_gav, gav_plus_i, r_plus_gav, gav_plus_r, gav_plus_gav
    end interface

    interface operator(.minus.)
        module procedure i_minus_gav, gav_minus_i, r_minus_gav, gav_minus_r, gav_minus_gav
    end interface
!    interface generic_f
!        module procedure generic_f_3
!    end interface generic_f

contains

    pure subroutine set_gav(this, v)
        class(g_auto_var(*)), intent(inout) :: this
        real(8), intent(in) :: v
        this%v = v
        this%d(1) = 1
        this%d(2:this%n) = 0
    end subroutine set_gav

    pure subroutine set_from_gav(this, v)
        class(g_auto_var(*)), intent(inout) :: this
        class(g_auto_var(*)), intent(in)    :: v
        this%v = v%v
        this%d = v%d
    end subroutine set_from_gav

    ! === times =========================================================================
    pure function i_times_gav(a, obj) result(r)
        integer, intent(in)              :: a
        class(g_auto_var(*)), intent(in) :: obj
        type(g_auto_var(obj%n)) :: r
        r = obj * a
    end function i_times_gav

    pure function r_times_gav(a, obj) result(r)
        real(8), intent(in)   :: a
        class(g_auto_var(*)), intent(in) :: obj
        type(g_auto_var(obj%n)) :: r
        r = obj * a
    end function r_times_gav

    pure function gav_times_i(this,a) result(r)
        class(g_auto_var(*)), intent(in) :: this
        integer, intent(in)   :: a
        type(g_auto_var(this%n)) :: r
        r%v = this%v * a
        r%d = this%d * a
    end function gav_times_i

    pure function gav_times_r(this,a) result(r)
        class(g_auto_var(*)), intent(in) :: this
        real(8), intent(in)   :: a
        type(g_auto_var(this%n)) :: r
        r%v = this%v * a
        r%d = this%d * a
    end function gav_times_r

    pure function gav_times_gav(this, a) result(r)
        class(g_auto_var(*)), intent(in) :: this
        type(g_auto_var(*)), intent(in) :: a
        type(g_auto_var(this%n)) :: r
        r%v = this%v*a%v
    end function gav_times_gav

    ! === plus  =========================================================================

    pure function r_plus_gav(a, obj) result(r)
        real(8), intent(in)   :: a
        class(g_auto_var(*)), intent(in) :: obj
        type(g_auto_var(obj%n)) :: r
        r = obj + a
    end function r_plus_gav

    pure function i_plus_gav(a, obj) result(r)
        integer, intent(in)   :: a
        class(g_auto_var(*)), intent(in) :: obj
        type(g_auto_var(obj%n)) :: r
        r = obj + a
    end function i_plus_gav
    
    pure function gav_plus_i(this,a) result(r)
        class(g_auto_var(*)), intent(in) :: this
        integer, intent(in)              :: a
        type(g_auto_var(this%n)) :: r
        r%v = this%v + a
        r%d = this%d
    end function gav_plus_i

    pure function gav_plus_r(this,a) result(r)
        class(g_auto_var(*)), intent(in) :: this
        real(8), intent(in)   :: a
        type(g_auto_var(this%n)) :: r
        r%v = this%v + a
        r%d = this%d
    end function gav_plus_r

    pure function gav_plus_gav(this, a) result(r)
        class(g_auto_var(*)), intent(in) :: this
        type(g_auto_var(*)), intent(in) :: a
        type(g_auto_var(this%n)) :: r
        r%v = this%v + a%v
        r%d = this%d + a%d
    end function gav_plus_gav

    ! === minus  =========================================================================

    pure function r_minus_gav(a, obj) result(r)
        real(8), intent(in)   :: a
        class(g_auto_var(*)), intent(in) :: obj
        type(g_auto_var(obj%n)) :: r
        r = -(obj - a)
    end function r_minus_gav

    pure function i_minus_gav(a, obj) result(r)
        integer, intent(in)   :: a
        class(g_auto_var(*)), intent(in) :: obj
        type(g_auto_var(obj%n)) :: r
        r = -(obj - a)
    end function i_minus_gav

    pure function gav_minus_i(this,a) result(r)
        class(g_auto_var(*)), intent(in) :: this
        integer, intent(in)              :: a
        type(g_auto_var(this%n)) :: r
        r%v = this%v - a
        r%d = this%d
    end function gav_minus_i

    pure function gav_minus_r(this,a) result(r)
        class(g_auto_var(*)), intent(in) :: this
        real(8), intent(in)   :: a
        type(g_auto_var(this%n)) :: r
        r%v = this%v - a
        r%d = this%d
    end function gav_minus_r

    pure function gav_minus_gav(this, a) result(r)
        class(g_auto_var(*)), intent(in) :: this
        type(g_auto_var(*)), intent(in) :: a
        type(g_auto_var(this%n)) :: r
        r%v = this%v - a%v
        r%d = this%d - a%d
    end function gav_minus_gav

    pure function uminus_gav(this) result(r)
        class(g_auto_var(*)), intent(in) :: this
        type(g_auto_var(this%n)) :: r
        r%v = -this%v
        r%d = -this%d
    end function uminus_gav

    ! === print  =======================================================================

    subroutine print_gav(this)
        class(g_auto_var(*)), intent(in) :: this
        integer :: i
        character(len=200), target :: buff
        character(len=:), pointer  :: c
        c => buff(1:)
        write(c,fmt='(f0.4,a,f0.4)') this%v,':',this%d(1)
        do i=2,this%n-1
            c => buff(len_trim(buff):)
            write(c,fmt='(a,f0.4)') ',',this%d(i)
        end do
        c => buff(len_trim(buff):)
        write(c,fmt='(a,f0.4)') ',',this%d(size(this%d))
        write(*,'(a)') trim(buff)
    end subroutine print_gav

    subroutine print_gav_s(this, str)
        class(g_auto_var(*)), intent(in) :: this
        character(len=*), intent(in)  :: str
        integer :: i
        character(len=200), target :: buff
        character(len=:), pointer  :: c
        c => buff(1:)
        write(c,fmt='(f0.4,a,f0.4)') this%v,':',this%d(1)
        do i=2,this%n-1
            c => buff(len_trim(buff):)
            write(c,fmt='(a,f0.4)') ',',this%d(i)
        end do
        c => buff(len_trim(buff):)
        write(c,fmt='(a,f0.4)') ',',this%d(size(this%d))
        write(*,'(a)') str//trim(buff)
    end subroutine print_gav_s

!    pure function generic_f_3(av, fidx) result(r)
!        type(g_auto_var(*)) :: r
!        type(g_auto_var(*)), intent(in) :: av
!        integer, intent(in)  :: fidx
!        procedure(f_t), pointer :: f
!
!        !        f => null()
!        !
!        !        a = D3()
!        !
!        !
!        !    inline ad_var<3> generic_f(const ad_var<3>& av, f_t f)
!        !    {
!        !    const auto& frow = ftable.at(f);
!        !
!        !    ad_var<3> a(generic_f(ad_var<2>(av),f));
!        !
!        !    double x = av.get_var();
!        !    auto f1 = frow.at(1)(x);
!        !    auto f2 = frow.at(2)(x);
!        !    auto f3 = frow.at(3)(x);
!        !    const auto& g1 = av.get(0);
!        !    const auto& g2 = av.get(1);
!        !    const auto& g3 = av.get(2);
!        !    a.set(2,g3*f1 + 3*g1*g2*f2 + pow(g1,3)*f3);
!        !    return a;
!    end function generic_f_3



end module AG

#ifdef UTEST

program utest

    use AG, Dg => g_auto_var

    type(Dg(2)) :: s, u, v, w, ref
    type(Dg(3)) :: s3, r3
!    type(D2) :: s, u, v, w, ref
!    type(D3) :: s3, r3, ref3
    real(8)  :: t
!
    t = 0.5d0
    call s%set(t)
    call s%print
    u = 3*s
    u = -s
    u = s - 1
    v = 3.mult.s - 1
    w = 1 .plus. (3.mult.s)
    u = u + v
!
    ref = Dg(2)(t,[1,0])
    call ref%print

    call s3%set(t)
    r3 = 3*s3
    r3 = s3 + 1
    r3 = 1 .plus. (3.mult.s3)
    r3 = (3.mult.s3) .plus. 1

    r3 = (1 .plus. s3) .mult. (1 .minus. (3.mult.s3)) .mult. (1 .plus. (3.mult.s3))
!    ref3 = D3((3*t + 1)*(1 + t)*(1 - t),[3-2*t-9*t**2,-18*t-2,-18.0d0])
!    call testResult('(3*s3 + 1)*(1 + s3)*(1 - s3)',ref3,r3)
!
!    r3 = s3**7
!    ref3 = D3(0.5**7,[7*0.5**6,7*6*0.5**5,7*6*5*0.5**4])
!    call testResult('s3**7',ref3,r3)
!
!    r3 = (1+s3**7)**(-0.5d0)
!    ref3 = D3(0.996116,[-0.0540528,-0.639835,-6.17195])
!    call testResult('(1+s3**7)**(-0.5d0)',ref3,r3)
!
!    ! 2nd derivatives
!    !D2 u(sin(s));
!    !D2 v(cos(s));
!    !    D2 w = u*v; // w = sin(x)*cos(x) == 0.5*sin(2x) :: D(w) = cos(2x) ; DD(w) = -2*sin(2x)
!    !    D2 ref({0.5*sin(1),cos(1),-2*sin(1)});
!    !    cout << "\tsin(x)*cos(x) ... ";
!    !    if (check_result(w,ref)) {
!    !        cout << "passed" << endl;
!    !    } else {
!    !        cout << "FAILED" << endl;
!    !    }
!
!    contains
!        subroutine testResult(str,ref,res)
!            character(len=*), intent(in) :: str
!            class(auto_var_u), intent(in) :: res
!            class(auto_var_u), intent(in) :: ref
!            if (res == ref) then
!                write(*,'(a)') 'passed'//' :: '//str
!                return
!            end if
!            write(*,'(a)') 'FAILED :: '//str//' => '//res%as_string()//' != '//ref%as_string()
!        end subroutine testResult
!
end program utest
#endif