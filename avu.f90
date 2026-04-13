module AV

    integer, parameter, private :: max_derivative = 4;

    type auto_var_u
        real(8), public      :: x = 0
        real(8), allocatable :: d(:)
    contains
        procedure, public :: set => set_avu
        procedure, public :: as_string
        procedure, public :: print_avu
        procedure, public :: print_avu_s
        generic, public   :: print => print_avu, print_avu_s
        procedure, public :: get => get_avu
        procedure, public :: set_from_avu
        generic, public   :: assignment(=) => set_from_avu
        procedure, public :: avu_times_avu
        procedure, public :: avu_times_r
        generic, public   :: operator(*) => avu_times_avu, avu_times_r
        !procedure, public :: avu_power_r
        !generic, public   :: operator(**) => avu_power_r
        procedure, public :: avu_equals_avu
        generic, public   :: operator(==) => avu_equals_avu
        procedure, public :: avu_plus_avu
        generic, public   :: operator(+) => avu_plus_avu
        procedure, public :: avu_minus_avu
        generic, public   :: operator(-) => avu_minus_avu
    end type auto_var_u

    type, extends(auto_var_u) :: D1
        integer :: n = 1
    contains
        procedure, public :: avu_times_avu_1
        generic, public   :: operator(*) => avu_times_avu_1
        procedure, public :: set => set_avu_1
    end type D1

    type, extends(auto_var_u) :: D2
        integer :: n = 2
    contains
        procedure, public :: avu_times_avu_2
        generic, public   :: operator(*) => avu_times_avu_2
        procedure, public :: set => set_avu_2
    end type D2
 
    type, extends(auto_var_u) :: D3
        integer :: n = 3
    contains
        procedure           :: avu_times_avu_3
        procedure           :: avu_times_3i
        procedure, pass(ob) :: i_times_D3
        procedure, pass(ob) :: r_times_D3
        generic, public   :: operator(*) => avu_times_avu_3, avu_times_3i, i_times_D3, r_times_D3
        procedure, public :: avu_power_3i
        procedure, public :: avu_power_3r
        generic, public   :: operator(**) => avu_power_3i, avu_power_3r
        procedure, public :: avu_plus_avu_3
        procedure, public :: avu_plus_3i
        generic, public   :: operator(+) => avu_plus_avu_3, avu_plus_3i
        procedure, public :: avu_minus_3r
        procedure, public :: avu_minus_3i
        generic, public   :: operator(-) => avu_minus_3r, avu_minus_3i
        procedure, public :: set => set_avu_3
    end type D3

    type, extends(auto_var_u) :: D4
        integer :: n = 4
    contains
        procedure, public :: avu_times_avu_4
        generic, public   :: operator(*) => avu_times_avu_4
        procedure, public :: set => set_avu_4
    end type D4
   
    interface operator(*)
    !    module procedure i_times_avu
    !    !module procedure avu_times_avu_1
    !    !module procedure avu_times_avu_2
        !    !module procedure avu_times_avu_4
    end interface

    interface operator(+)
        module procedure r_plus_avu
    !    !module procedure avu_plus_avu_1
    !    !module procedure avu_plus_avu_2
         module procedure i_plus_D3
    !    !module procedure avu_plus_avu_4
    end interface

    interface operator(-)
    !    module procedure r_minus_avu
    !    !module procedure avu_plus_avu_1
    !    !module procedure avu_plus_avu_2
        module procedure i_minus_D3
        module procedure r_minus_D3
    !    !module procedure avu_plus_avu_4
    end interface

contains


    pure function avu_equals_avu(this, a) result(r)
        class(auto_var_u), intent(in) :: this
        class(auto_var_u), intent(in)  :: a
        logical :: r
        r = (abs(this%x - a%x) < 1.0e-6 .and. any(abs(this%d - a%d) < 1.0e-6))
    end function avu_equals_avu

    ! === Plus  ===================================================================================

    pure function i_plus_D3(a, ob) result(r)
        type(D3) :: r
        integer, intent(in)  :: a
        type(D3), intent(in) :: ob
        r = ob + a
    end function i_plus_D3

    pure function avu_plus_avu(this, a) result(r)
        class(auto_var_u), intent(in) :: this
        type(auto_var_u), intent(in) :: a
        type(auto_var_u) :: r
        r%x = this%x + a%x
        r%d = this%d + a%d
    end function avu_plus_avu
 
    pure function avu_plus_r(this, a) result(r)
        class(auto_var_u), intent(in) :: this
        real(8), intent(in) :: a
        type(auto_var_u) :: r
        r%x = this%x + a
        r%d = this%d
    end function avu_plus_r

    pure function avu_plus_avu_3(this,a) result(r)
        class(D3), intent(in) :: this
        real(8), intent(in)   :: a
        type(D3) :: r
        r%x = this%x + a
        r%d = this%d
    end function avu_plus_avu_3

    pure function r_plus_avu(a, ob) result(r)
        class(auto_var_u), intent(in) :: ob
        real(8), intent(in) :: a
        type(auto_var_u) :: r
        r%x = ob%x + a
        r%d = ob%d
    end function r_plus_avu

    pure function r_minus_avu(a, ob) result(r)
        class(auto_var_u), intent(in) :: ob
        real(8), intent(in) :: a
        type(auto_var_u) :: r
        r%x = a - ob%x
        r%d = -ob%d
    end function r_minus_avu

    pure function avu_minus_avu(ob, a) result(r)
        class(auto_var_u), intent(in) :: ob
        class(auto_var_u), intent(in) :: a
        type(auto_var_u) :: r
        r%x = ob%x - a%x
        r%d = ob%d - a%d
    end function avu_minus_avu

    pure function avu_minus_r(ob, a) result(r)
        class(auto_var_u), intent(in) :: ob
        real(8), intent(in) :: a
        type(auto_var_u) :: r
        r%x = ob%x - a
        r%d = ob%d
    end function avu_minus_r

    pure function avu_minus_i(ob, a) result(r)
        class(auto_var_u), intent(in) :: ob
        integer, intent(in) :: a
        type(auto_var_u) :: r
        r%x = ob%x - a
        r%d = ob%d
    end function avu_minus_i

    pure function avu_plus_3i(this,a) result(r)
        class(D3), intent(in) :: this
        integer, intent(in)   :: a
        type(D3) :: r
        r%x = this%x + a
        r%d = this%d
    end function avu_plus_3i

    pure function avu_minus_3i(this,a) result(r)
        class(D3), intent(in) :: this
        integer, intent(in)   :: a
        type(D3) :: r
        r%x = this%x - a
        r%d = this%d
    end function avu_minus_3i

    pure function avu_minus_3r(this,a) result(r)
        class(D3), intent(in) :: this
        real(8), intent(in)   :: a
        type(D3) :: r
        r%x = this%x - a
        r%d = this%d
    end function avu_minus_3r

    pure function i_minus_D3(a, ob) result(r)
        type(D3) :: r
        integer, intent(in)  :: a
        type(D3), intent(in) :: ob
        r%x = a - ob%x
        r%d = -ob%d
    end function i_minus_D3

    pure function r_minus_D3(a, ob) result(r)
        type(D3) :: r
        real(8), intent(in)  :: a
        type(D3), intent(in) :: ob
        r%x = a - ob%x
        r%d = -ob%d
    end function r_minus_D3

    ! === Times ===================================================================================

    pure function avu_times_3i(this,a) result(r)
        class(D3), intent(in) :: this
        integer, intent(in)   :: a
        type(D3) :: r
        r%x = this%x * a
        r%d = this%d * a
    end function avu_times_3i

    pure function avu_times_3r(this,a) result(r)
        class(D3), intent(in) :: this
        real(8), intent(in)   :: a
        type(D3) :: r
        r%x = this%x * a
        r%d = this%d * a
    end function avu_times_3r

    pure function avu_times_avu(this, a) result(r)
        class(auto_var_u), intent(in) :: this
        type(auto_var_u), intent(in) :: a
        type(auto_var_u) :: r
        r%x = this%x*a%x
    end function avu_times_avu

    pure function avu_times_i(this, a) result(r)
        class(auto_var_u), intent(in) :: this
        integer, intent(in)           :: a
        type(auto_var_u) :: r
        r%x = this%x*a
        r%d = this%d*a
    end function avu_times_i

    pure function avu_times_r(this, a) result(r)
        class(auto_var_u), intent(in) :: this
        real(8), intent(in)           :: a
        type(auto_var_u) :: r
        r%x = this%x*a
        r%d = this%d*a
    end function avu_times_r

    pure function r_times_avu(a, ob) result(r)
        class(auto_var_u), intent(in) :: ob
        real(8), intent(in) :: a
        type(auto_var_u) :: r
        r%x = ob%x*a
    end function r_times_avu

!    pure function i_times_avu(a, ob) result(r)
!        class(auto_var_u), intent(in) :: ob
!        integer, intent(in)           :: a
!        type(auto_var_u) :: r
!        r%x = ob%x*a
!        r%d = ob%d*a
!    end function i_times_avu

    pure function i_times_D3(a, ob) result(r)
        class(D3), intent(in) :: ob
        integer, intent(in)   :: a
        type(D3)              :: r
        r%x = ob%x*a
        r%d = ob%d*a
    end function i_times_D3

    pure function r_times_D3(a, ob) result(r)
        class(D3), intent(in) :: ob
        real(8), intent(in)   :: a
        type(D3)              :: r
        r%x = ob%x*a
        r%d = ob%d*a
    end function r_times_D3

    pure function avu_times_avu_1(this, a) result(r)
        class(D1), intent(in) :: this
        type(D1), intent(in) :: a
        type(D1) :: r
        r%x = this%x * a%x
        allocate(r%d(1))
        associate (u0 => this%x, u => this%d, v => a%d, v0 => a%x, f => r%d)
            f(1) = u0*v(1) + v(1)*v0
        end associate
    end function avu_times_avu_1
 
    pure function avu_times_avu_2(this, a) result(r)
        class(D2), intent(in) :: this
        type(D2), intent(in) :: a
        type(D2) :: r
        r%x = this%x * a%x
        allocate(r%d(this%n))
        associate (u0 => this%x, u => this%d, v => a%d, v0 => a%x, f => r%d)
            f(1) = u0*v(1) + v(1)*v0
            f(2) = u0*v(2) + 2*u(1)*v(1) + u(2)*v0
        end associate
    end function avu_times_avu_2
  
    pure function avu_times_avu_3(this, a) result(r)
        class(D3), intent(in) :: this
        type(D3), intent(in) :: a
        type(D3) :: r
        r%x = this%x * a%x
        allocate(r%d(this%n))
        associate (u0 => this%x, u => this%d, v => a%d, v0 => a%x, f => r%d)
            f(1) = u0*v(1) + u(1)*v0
            f(2) = u0*v(2) + 2*u(1)*v(1) + u(2)*v0
            f(3) = u0*v(3) + 3*u(1)*v(2) + 3*u(2)*v(1) + u(3)*v0
        end associate
    end function avu_times_avu_3
 
    pure function avu_times_avu_4(this, a) result(r)
        class(D4), intent(in) :: this
        type(D4), intent(in) :: a
        type(D4) :: r
        r%x = this%x * a%x
        allocate(r%d(this%n))
        associate (u0 => this%x, u => this%d, v => a%d, v0 => a%x, f => r%d)
            f(1) = u0*v(1) + u(1)*v0
            f(2) = u0*v(2) + 2*u(1)*v(1) + u(2)*v0
            f(3) = u0*v(3) + 3*u(1)*v(2) + 3*u(2)*v(1) + u(3)*v0
            f(3) = u0*v(4) + 4*u(1)*v(3) + 6*u(2)*v(2) + 3*u(3)*v(1) + u(4)*v0
        end associate
    end function avu_times_avu_4

    ! === Power  ===================================================================================

    pure function avu_power_r(this, a) result(r)
        class(auto_var_u), intent(in) :: this
        real(8), intent(in) :: a
        type(auto_var_u) :: r
        r%x = this%x**a
        r%d = this%d
        r%d(1) = a*r%d(1)**(a-1)
        r%d(2) = a*(a-1)*r%d(2)**(a-2)
        r%d(3) = a*(a-1)*(a-2)*r%d(3)**(a-2)
    end function avu_power_r

    pure function avu_power_3i(this, a) result(r)
        class(D3), intent(in) :: this
        integer, intent(in)   :: a
        type(D3) :: r
        real(8)  :: f1, f2, f3
        associate(v => this%x, g1 => this%d(1), g2 => this%d(2), g3 => this%d(3))
            r%x = v**a
            allocate(r%d(3))
            f1 = a*v**(a-1)
            f2 = a*(a-1)*v**(a-2)
            f3 = a*(a-1)*(a-2)*v**(a-3)
            r%d(1) = g1*f1
            r%d(2) = g2*f1+g1*g1*f2
            r%d(3) = g3*f1 + 3*g1*g2*f2 + (g1**3)*f3
        end associate
    end function avu_power_3i

    pure function avu_power_3r(this, a) result(r)
        class(D3), intent(in) :: this
        real(8), intent(in)   :: a
        type(D3) :: r
        real(8)  :: f1, f2, f3
        associate(v => this%x, g1 => this%d(1), g2 => this%d(2), g3 => this%d(3))
            r%x = v**a
            allocate(r%d(3))
            f1 = a*v**(a-1)
            f2 = a*(a-1)*v**(a-2)
            f3 = a*(a-1)*(a-2)*v**(a-3)
            r%d(1) = g1*f1
            r%d(2) = g2*f1+g1*g1*f2
            r%d(3) = g3*f1 + 3*g1*g2*f2 + (g1**3)*f3
        end associate
    end function avu_power_3r

    ! === Get / Set ================================================================================

    pure function get_avu(this) result(r)
        real(8) :: r
        class(auto_var_u), intent(in) :: this
        r = this%x
    end function get_avu
    
    subroutine set_from_avu(this, v)
        class(auto_var_u), intent(inout) :: this
        class(D2), intent(in)    :: v
        this%x = v%x
        this%d = v%d
    end subroutine set_from_avu

    subroutine set_avu(this, v)
        class(auto_var_u), intent(inout) :: this
        real(8), intent(in) :: v
        this%x = v
    end subroutine set_avu

    subroutine set_avu_1(this, v)
        class(D1), intent(inout) :: this
        real(8), intent(in) :: v
        this%d = [1]
        call this%auto_var_u%set(v)
    end subroutine set_avu_1
 
    subroutine set_avu_2(this, v)
        class(D2), intent(inout) :: this
        real(8), intent(in) :: v
        this%d = [1, 0]
        call this%auto_var_u%set(v)
    end subroutine set_avu_2
  
    subroutine set_avu_3(this, v)
        class(D3), intent(inout) :: this
        real(8), intent(in) :: v
        this%d = [1, 0, 0]
        call this%auto_var_u%set(v)
    end subroutine set_avu_3
 
    subroutine set_avu_4(this, v)
        class(D4), intent(inout) :: this
        real(8), intent(in) :: v
        this%d = [1, 0, 0, 0]
        call this%auto_var_u%set(v)
    end subroutine set_avu_4

    subroutine print_avu(this)
        class(auto_var_u), intent(in) :: this
        integer :: i
        character(len=200), target :: buff
        character(len=:), pointer  :: c
        c => buff(1:)
        write(c,fmt='(f0.4,a,f0.4)') this%x,':',this%d(1)
        do i=2,size(this%d)-1
            c => buff(len_trim(buff):)
            write(c,fmt='(a,f0.4)') ',',this%d(i)
        end do
        c => buff(len_trim(buff):)
        write(c,fmt='(a,f0.4)') ',',this%d(size(this%d))
        write(*,'(a)') trim(buff)
    end subroutine print_avu

    subroutine print_avu_s(this, str)
        class(auto_var_u), intent(in) :: this
        character(len=*), intent(in)  :: str
        integer :: i
        character(len=200), target :: buff
        character(len=:), pointer  :: c
        c => buff(1:)
        write(c,fmt='(f0.4,a,f0.4)') this%x,':',this%d(1)
        do i=2,size(this%d)-1
            c => buff(len_trim(buff):)
            write(c,fmt='(a,f0.4)') ',',this%d(i)
        end do
        c => buff(len_trim(buff):)
        write(c,fmt='(a,f0.4)') ',',this%d(size(this%d))
        write(*,'(a)') str//trim(buff)
    end subroutine print_avu_s

    function as_string(this) result(r)
        class(auto_var_u), intent(in) :: this
        character(len=:), allocatable :: r
        integer :: i
        character(len=200), target :: buff
        character(len=:), pointer  :: c
        c => buff(1:)
        write(c,fmt='(2(a,f0.4))') '[',this%x,':',this%d(1)
        do i=2,size(this%d)-1
            c => buff(len_trim(buff):)
            write(c,fmt='(a,f0.4)') ',',this%d(i)
        end do
        c => buff(len_trim(buff):)
        write(c,fmt='(a,f0.4,a)') ',',this%d(size(this%d)),']'
        r = trim(buff)
    end function as_string

end module AV