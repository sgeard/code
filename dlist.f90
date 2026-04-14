module dlist
    public

    type dlist_t
        private
        type(dlist_node_t), pointer :: begin => null()
        type(dlist_node_t), pointer :: end   => null()
        integer                     :: num_of_elements = 0
    contains
        procedure, public :: iterate         => iterate_ll
        procedure, public :: reverse_iterate => reverse_iterate_ll
        procedure, public :: print => print_ll
        procedure, public :: append => append_ll
        procedure, public :: insert => insert_ll
        procedure, public :: remove => remove_ll
        procedure, public :: size => size_ll
        procedure, public :: clear => clear_ll
    end type dlist_t

    ! Data type from which to inherit to create a data node.
    ! No methods required — storage uses polymorphic assignment (=).
    type :: dlist_node_data_t
    end type dlist_node_data_t

    type, extends(dlist_node_data_t) :: dlist_node_integer
        integer :: data = 0
    end type dlist_node_integer

    type, extends(dlist_node_data_t) :: dlist_node_real
        real(8) :: data = 0.0d0
    end type dlist_node_real

    type, extends(dlist_node_data_t) :: dlist_node_real_a
        real(8), allocatable :: data(:)
    end type dlist_node_real_a

    type, extends(dlist_node_data_t) :: dlist_node_real_m
        real(8), allocatable :: data(:,:)
    end type dlist_node_real_m

    type, extends(dlist_node_data_t) :: dlist_node_char
        character(len=:), allocatable :: data
    end type dlist_node_char
    
    interface int_node
        module procedure make_int_node
    end interface int_node

    interface real_node
        module procedure make_real_node
    end interface real_node

    interface real_a_node
        module procedure make_real_a_node
    end interface real_a_node

    interface real_m_node
        module procedure make_real_m_node
    end interface real_m_node

    interface char_node
        module procedure make_char_node
    end interface char_node

    interface make_node
        module procedure make_int_node
        module procedure make_real_node
        module procedure make_real_a_node
        module procedure make_real_m_node
        module procedure make_char_node
    end interface
    
    interface
        module function make_int_node(v) result(n)
            integer, intent(in)          :: v
            type(dlist_node_integer)     :: n
        end function make_int_node

        module function make_real_node(v) result(n)
            real(8), intent(in)          :: v
            type(dlist_node_real)        :: n
        end function make_real_node

        module function make_real_a_node(v) result(n)
            real(8), intent(in)     :: v(:)
            type(dlist_node_real_a) :: n
        end function make_real_a_node

        module function make_real_m_node(v) result(n)
            real(8), intent(in)     :: v(:,:)
            type(dlist_node_real_m) :: n
        end function make_real_m_node

        module function make_char_node(v) result(n)
            character(len=*), intent(in) :: v
            type(dlist_node_char)        :: n
        end function make_char_node
    end interface

    type dlist_node_t
        private
        class(dlist_node_data_t), allocatable :: data
        type(dlist_node_t), pointer :: next     => null()
        type(dlist_node_t), pointer :: previous => null()
    end type dlist_node_t

    ! Interface for functions being applied to each list element in turn
    ! when iterating
    abstract interface
        subroutine command_fun(command, ok)
            import dlist_node_data_t
            class(dlist_node_data_t), intent(in) :: command
            logical, intent(out)                 :: ok ! Exit the loop if not true
        end subroutine command_fun
    end interface

    interface
        module function iterate_ll(this, f) result(r)
            class(dlist_t), intent(inout), target :: this
            procedure(command_fun)                :: f
            logical :: r
        end function iterate_ll

        module subroutine insert_ll(lst, idx, data)
            class(dlist_t), intent(inout)        :: lst
            integer, intent(in)                  :: idx
            class(dlist_node_data_t), intent(in) :: data
        end subroutine insert_ll
        
        module subroutine remove_ll(lst, idx)
            class(dlist_t), intent(inout) :: lst
            integer, intent(in)           :: idx
        end subroutine remove_ll
        
        module function reverse_iterate_ll(this, f) result(r)
            class(dlist_t), intent(inout), target :: this
            procedure(command_fun)                :: f
            logical :: r
        end function reverse_iterate_ll

        module subroutine print_ll(lst)
            class(dlist_t), intent(in) :: lst
        end subroutine print_ll

        module subroutine append_ll(lst, data)
            class(dlist_t), intent(inout)        :: lst
            class(dlist_node_data_t), intent(in) :: data
        end subroutine append_ll

        module integer function size_ll(lst)
            class(dlist_t), intent(inout) :: lst
        end function size_ll

        module subroutine clear_ll(lst)
            class(dlist_t), intent(inout) :: lst
        end subroutine clear_ll

    end interface

end module dlist
