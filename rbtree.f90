!
! Translated from the C version

module rb_tree

    integer, parameter :: RED = 1
    integer, parameter :: BLACK = 0
    integer, parameter :: INDENT_STEP = 4
    
    type rbtree_t
        type(rbtree_node_t), pointer :: root
        integer                      :: count = 0
    contains
        procedure :: dump_tree => dump_tree_t
    end type rbtree_t
    
    type rbtree_node_t
        class(*), allocatable :: key
        class(*), allocatable :: value
        type(rbtree_node_t), pointer :: left => null()
        type(rbtree_node_t), pointer :: right => null()
        type(rbtree_node_t), pointer :: parent => null()
        integer :: colour = -1
        integer :: id = -1
    end type rbtree_node_t
    
    abstract interface
        function compare_func(left, right)
            integer :: compare_func
            class(*), intent(in) :: left, right
        end function compare_func
    end interface


    contains
        integer function get_id()
            integer, save :: id = 0
            id = id + 1
            get_id = id
        end function get_id
            
        subroutine dump_tree_t(this)
            class(rbtree_t), intent(in) :: this
            call print_tree_helper(this%root, 0)
            write(*,*)
        end subroutine dump_tree_t
    
        recursive subroutine print_tree_helper(n, indent)
            type(rbtree_node_t), pointer, intent(in) :: n
            integer, intent(in) :: indent
            if (.not. associated(n)) then
                write(*,'(a)') '<empty tree>'
                return
            end if
            if (associated(n%right)) then
                call print_tree_helper(n%right, indent + INDENT_STEP);
            end if
            write(*,fmt='(a)',advance='no') repeat(' ',indent)
            if (node_colour(n) == BLACK) then
                write(*,'(i0,a,i0)') get_key(n),':',n%id
            else
                write(*,'(2(a,i0),a)') '<',get_key(n),':',n%id,'>'
            end if
            if (associated(n%left)) then
                call print_tree_helper(n%left, indent + INDENT_STEP);
            end if
        
        end subroutine print_tree_helper
              
        integer function compare_ints(i, j)
            class(*), intent(in) :: i, j
            select type(i)
                type is (integer)
                select type(j)
                    type is (integer)
                        if (i < j) then
                            compare_ints = -1
                            return
                        else if (i == j) then
                            compare_ints = 0
                            return
                        else
                            compare_ints = 1
                            return
                        end if
                end select
            end select
            stop "***compare_ints"
        end function compare_ints
        
        integer function get_value(n)
            type(rbtree_node_t), pointer, intent(in) :: n
            if (.not. associated(n)) stop '***get_value'
            select type(v => n%value)
            type is (integer)
                get_value = v
            class default
                get_value = -1
            end select
        end function get_value
        
        integer function get_key(n)
            type(rbtree_node_t), pointer, intent(in) :: n
            if (.not. associated(n)) then
                stop '***get_key'
            end if
            select type(v => n%key)
            type is (integer)
                get_key = v
            class default
                get_key = -3
            end select
        end function get_key
    
       subroutine check_node(n, str)
            type(rbtree_node_t), pointer :: n
            character(len=*), optional, intent(in) :: str
            integer :: p
            if (.not. associated(n)) then
                write(*,'(a)') 'node not allocated'
                return
            end if
            
#ifdef TRACE
            if (present(str)) then
                write(*,fmt='(a)',advance='no') str
            end if
                
            ! Parent
            if (.not. associated(n%parent)) then
                write(*,'(a,i0,a)') 'node ',get_key(n),' is the root node'
                return
            end if
            p = get_key(n%parent)
            if (node_colour(n) == BLACK) then
                write(*,'(a,i0,a)') 'node ',get_key(n),' is BLACK'
            else
                write(*,'(a,i0,a)') 'node ',get_key(n),' is RED'
            end if

#endif
       end subroutine check_node
       
       function grandparent(n)
            type(rbtree_node_t), pointer :: grandparent
            type(rbtree_node_t), pointer, intent(in) :: n
            if (.not. associated(n%parent)) stop '***grandparent: not the root node'
            if (.not. associated(n%parent%parent)) stop '***grandparent: not child of root'
            grandparent => n%parent%parent
            if (associated(grandparent,n)) stop "***grandparent: node is it's own grandparent"
       end function grandparent
       
        function sibling(n)
            type(rbtree_node_t), pointer :: sibling
            type(rbtree_node_t), pointer, intent(in) :: n
            if (.not. associated(n%parent)) stop 'sibling: node has no parent'
            if (associated(n,n%parent%left)) then
                sibling => n%parent%right
            else if (associated(n,n%parent%right)) then
                sibling => n%parent%left
            else
                stop '***sibling: node has no siblings'
            end if
        end function
 
        function uncle(n)
            type(rbtree_node_t), pointer :: uncle
            type(rbtree_node_t), pointer, intent(in) :: n
            if (.not. associated(n%parent)) stop '***uncle: root node has no uncle'
            if (.not. associated(n%parent%parent)) stop '***uncle: children of root have no uncle'
            uncle => sibling(n%parent)
        end function

        logical function verify_properties(t)
            type(rbtree_t), intent(in) :: t
            call verify_property_1(t%root)
            call verify_property_2(t%root)
            call verify_property_4(t%root)
            call verify_property_5(t%root)
            verify_properties = .true.
        end function verify_properties
        
        recursive subroutine verify_property_1(n)
            type(rbtree_node_t), pointer, intent(in) :: n
            if (node_colour(n) /= RED .and. node_colour(n) /= BLACK) stop 'verify_property_1: node colour not set'
            if (.not. associated(n)) return
            if (associated(n,n%parent)) stop 'verify_property_1: invalid parent'
            if (associated(n,n%left)) stop 'verify_property_1: invalid left'
            if (associated(n,n%right)) stop 'verify_property_1: invalid right'
            call verify_property_1(n%left)
            call verify_property_1(n%right)
        end subroutine verify_property_1
        
        subroutine verify_property_2(root)
            type(rbtree_node_t), pointer, intent(in) :: root
            if (node_colour(root) /= BLACK) stop 'verify_property_2'
        end subroutine verify_property_2
   
    integer function node_colour(n)
        type(rbtree_node_t), pointer :: n
        if (associated(n)) then
            node_colour = n%colour
        else
            node_colour = BLACK
        end if
    end function node_colour
    
    recursive subroutine verify_property_4(n)
        type(rbtree_node_t), pointer, intent(in) :: n
        if (node_colour(n) == RED) then
            if (node_colour(n%left) /= BLACK) stop 'verify_property_4 - left'
            if (node_colour(n%right) /= BLACK) stop 'verify_property_4 - right'
            if (node_colour(n%parent) /= BLACK) stop 'verify_property_4 - parent'
        end if
        if (.not. associated(n)) return
        call verify_property_4(n%left)
        call verify_property_4(n%right)
    end subroutine verify_property_4

    subroutine verify_property_5(root)
        type(rbtree_node_t), pointer, intent(in) :: root
        integer :: path_black_count, black_count
        path_black_count = -1
        black_count = 0
        call verify_property_5_helper(root, black_count, path_black_count)
    contains
        recursive subroutine verify_property_5_helper(n, black_count, path_black_count)
            type(rbtree_node_t), pointer, intent(in) :: n
            integer, value :: black_count
            integer, intent(inout) :: path_black_count
            
            if (node_colour(n) == BLACK) black_count = black_count + 1
            
            if (.not. associated(n)) then
                if (path_black_count == -1) then
                    path_black_count = black_count
                else
                    if (black_count /= path_black_count) stop 'verify_property_5'
                end if
                return
            end if
            
            call verify_property_5_helper(n%left, black_count, path_black_count)
            call verify_property_5_helper(n%right, black_count, path_black_count)
        end subroutine verify_property_5_helper
    end subroutine verify_property_5

    subroutine rbtree_create(t)
        type(rbtree_t), intent(inout) :: t
        logical :: tok
        call set_colour(t%root, BLACK)
        tok = verify_properties(t)
    end subroutine rbtree_create

    function new_node(key, value, colour, left, right) result(n)
        type(rbtree_node_t), pointer :: n
        integer, intent(in)          :: key
        class(*)                     :: value
        integer, intent(in)          :: colour
        type(rbtree_node_t), pointer :: left, right
        
        allocate(n)
        allocate(n%key, source=key)
        allocate(n%value, source=value)
        n%colour = colour
        n%left => left
        n%right => right
        n%id = get_id()
        if (associated(left)) left%parent => n
        if (associated(right)) right%parent => n
     end function new_node

    function lookup_node(t, key, compare) result(n)
        type(rbtree_node_t), pointer  :: n
        type(rbtree_t)                :: t
        class(*), pointer, intent(in) :: key
        procedure(compare_func)       :: compare
        integer :: comp_result
        n => t%root
        do while(associated(n))
            comp_result = compare(key, n%key)
            if (comp_result == 0) then
                return
            else if (comp_result < 0) then
                n => n%left
            else
                n => n%right
            end if
        end do
        
    end function lookup_node
    
    function rbtree_lookup(t, key, compare) result(v)
      class(*), pointer :: v
        type(rbtree_node_t), pointer :: n
        type(rbtree_t)          :: t
        class(*), pointer, intent(in)     :: key
        procedure(compare_func) :: compare
        n => lookup_node(t, key, compare)
        if (associated(n)) then
            v => n%value
        else
            v => null()
        end if
    end function rbtree_lookup
    
    subroutine rotate_left(t, n_arg)
        type(rbtree_t) :: t
        type(rbtree_node_t), pointer :: n_arg
        type(rbtree_node_t), pointer :: r
        type(rbtree_node_t), pointer :: n
        
        n => n_arg
        
        !!write(*,'(a)') 'rotate_left<in>'
        call check_node(n,'   n = ')
        r => n%right
        call check_node(r,'   r = ')
        call replace_node(t, n, r)
        call check_node(n,'   n = ')
        n%right => r%left
        call check_node(n,'   n = ')
        if (associated(r%left)) then
            r%left%parent => n
        end if
        r%left => n
        n%parent => r
        call check_node(n,'   n = ')
    end subroutine rotate_left
    
    subroutine rotate_right(t, n_arg)
        type(rbtree_t) :: t
        type(rbtree_node_t), pointer :: n_arg
        type(rbtree_node_t), pointer :: l
        type(rbtree_node_t), pointer :: n
        
        n => n_arg
        l => n%left
        call replace_node(t, n, l)
        n%left => l%right
        if (associated(l%right)) then
            l%right%parent => n
        end if
        l%right => n
        n%parent => l
    end subroutine rotate_right

    subroutine replace_node(t, oldn_arg, newn_arg)
        type(rbtree_t) :: t
        type(rbtree_node_t), pointer :: oldn_arg
        type(rbtree_node_t), pointer :: newn_arg
        type(rbtree_node_t), pointer :: oldn
        type(rbtree_node_t), pointer :: newn
        integer :: k
        
        oldn => oldn_arg
        newn => newn_arg
        
        call check_node(oldn,'   oldn = ') 
        if (associated(oldn%parent)) then
            call check_node(oldn%parent, '   oldn%parent = ')
            if (associated(oldn, oldn%parent%left)) then
#ifdef TRACE
                write(*,'(a,i0,a)') '   setting left node ',get_key(oldn%parent%left),' to new node'
#endif
                oldn%parent%left => newn
            else if (associated(oldn, oldn%parent%right)) then
#ifdef TRACE
                write(*,'(a,i0,a)') '   setting right node ',get_key(oldn%parent%right),' to new node'
#endif
                oldn%parent%right => newn
            else
                stop '***replace_node: replace_node failed'
            end if
            call check_node(oldn%parent, '   oldn%parent = ')
            call check_node(oldn, '   oldn = ')
        else
            t%root => newn
        end if
        if (associated(newn)) then
            k = get_key(oldn)
            !!write(*,'(a,i0)') '   >>> setting newn parent to ',get_key(oldn%parent)
            newn%parent => oldn%parent
            if (k /= get_key(oldn)) stop '*** replace_node: oldn corrupted'
        end if
        call check_node(oldn,'   oldn = ')
        call check_node(newn,'   newn = ')
    end subroutine replace_node

    subroutine rbtree_insert(t, key, value, compare)
        type(rbtree_t)       :: t
        integer, intent(in)  :: key
        class(*), intent(in) :: value
        procedure(compare_func) :: compare
        type(rbtree_node_t), pointer :: inserted_node, n
        integer :: comp_result
        
        inserted_node => new_node(key, value, RED, null(), null())

        if (associated(t%root)) then
            n => t%root
            do
                comp_result = compare_ints(key, n%key)
                if (comp_result == 0) then
                    n%value = value
                    return
                else if (comp_result < 0) then
                    if (.not. associated(n%left)) then
                        n%left => inserted_node
                        exit
                    else
                        n => n%left
                    end if
                else
                    if (.not. associated(n%right)) then
                        n%right => inserted_node
                        exit
                    else
                        n => n%right
                    end if
                end if
            end do
            inserted_node%parent => n
        else
            t%root => inserted_node
        end if
        call insert_case1(t, inserted_node)

        t%count = t%count + 1
    end subroutine rbtree_insert
    
    subroutine set_colour(n, c)
        type(rbtree_node_t), pointer :: n
        integer, intent(in) :: c
        n%colour = c
    end subroutine set_colour
    
    recursive subroutine insert_case1(t, n)
        type(rbtree_t) :: t
        type(rbtree_node_t), pointer :: n

        if (associated(n%parent)) then
            call insert_case2(t, n)
        else
            n%colour = BLACK
        endif
    end subroutine insert_case1

    recursive subroutine insert_case2(t, n)
        type(rbtree_t) :: t
        type(rbtree_node_t), pointer :: n

        if (node_colour(n%parent) == BLACK) then
            return ! Tree is still valid
        else
            call insert_case3(t, n)
        endif
    end subroutine insert_case2
    
    recursive subroutine insert_case3(t, n)
        type(rbtree_t) :: t
        type(rbtree_node_t), pointer :: n

        if (node_colour(uncle(n)) == RED) then
            call set_colour(n%parent, BLACK)
            call set_colour(uncle(n), BLACK)
            call set_colour(grandparent(n), RED)
            call insert_case1(t, grandparent(n))
        else
            call insert_case4(t, n)
        end if
        
    end subroutine insert_case3
    
    recursive subroutine insert_case4(t, n)
        type(rbtree_t) :: t
        type(rbtree_node_t), pointer :: n
        type(rbtree_node_t), pointer :: gp

        gp => grandparent(n)
        if (associated(n, n%parent%right) .and. associated(n%parent, gp%left)) then
            call rotate_left(t, n%parent)
            n => n%left
        else if (associated(n, n%parent%left) .and. associated(n%parent, gp%right)) then
            call rotate_right(t, n%parent)
            n => n%right
        end if
        call insert_case5(t, n)
    end subroutine insert_case4    

    recursive subroutine insert_case5(t, n)
        type(rbtree_t) :: t
        type(rbtree_node_t), pointer :: n
        type(rbtree_node_t), pointer :: gp
        
        gp => grandparent(n)
        call set_colour(n%parent, BLACK)
        call set_colour(gp, RED)
        if (associated(n, n%parent%left) .and. associated(n%parent, gp%left)) then
            call rotate_right(t, gp);
        else if (associated(n, n%parent%right) .and. associated(n%parent, gp%right)) then
            call rotate_left(t, gp)
        else
            stop 'insert_case5'
        end if

    end subroutine insert_case5
    
    function maximum_node(np) result(nr)
        type(rbtree_node_t), pointer :: nr
        type(rbtree_node_t), pointer :: np
        if (.not. associated(np)) stop '***maximum node: n not associated'
        nr => np
        do
            if (.not. associated(nr%right)) exit
            nr => nr%right
        end do
    end function maximum_node
    
    subroutine rbtree_delete(t, key, cf)
        type(rbtree_t)          :: t
        class(*), pointer       :: key
        logical                 :: tok
        procedure(compare_func) :: cf
        
        type(rbtree_node_t), pointer :: n, pred, child
        n => lookup_node(t, key, cf)

        if (.not. associated(n)) return ! Key not found, do nothing
        
        if (associated(n%left) .and. associated(n%right)) then
            ! Copy key/value from predecessor and then delete it instead
            pred => maximum_node(n%left)
            n%key = pred%key
            n%value = pred%value
            n => pred
        end if

        if (associated(n%left) .and. associated(n%right)) stop '***rbtree_delete'
        
        if (associated(n%right)) then
            child => n%right
        else
            child => n%left
        end if

        if (node_colour(n) == BLACK) then
            call set_colour(n, node_colour(child))
            call delete_case1(t, n)
        end if
        
        call replace_node(t, n, child)
        if (.not. associated(n%parent) .and. associated(child)) then ! root should be black
            call set_colour(child, BLACK)
        end if
        deallocate(n)
        tok = verify_properties(t)
        
    end subroutine rbtree_delete
        
    recursive subroutine delete_case1(t, n)
        type(rbtree_t)               :: t
        type(rbtree_node_t), pointer :: n
        if (associated(n%parent)) then
            call delete_case2(t, n)
        end if
    end subroutine delete_case1

    recursive subroutine delete_case2(t, n)
        type(rbtree_t)               :: t
        type(rbtree_node_t), pointer :: n

        if (node_colour(sibling(n)) == RED) then
            call set_colour(n%parent, RED)
            call set_colour(sibling(n),BLACK)
            if (associated(n,n%parent%left)) then
                call rotate_left(t, n%parent)
            else
                call rotate_right(t, n%parent)
            end if
        end if 
        call delete_case3(t, n)
    end subroutine delete_case2

    recursive subroutine delete_case3(t, n)
        type(rbtree_t)               :: t
        type(rbtree_node_t), pointer :: n
        type(rbtree_node_t), pointer :: s
        
        s => sibling(n)
        if (node_colour(n%parent) == BLACK .and. &
            node_colour(s) == BLACK .and.        &
            node_colour(s%left) == BLACK .and.   &
            node_colour(s%right) == BLACK) then
            call set_colour(s,RED)
            call delete_case1(t, n%parent)
        else
            call delete_case4(t, n)
        end if
    end subroutine delete_case3
    
    subroutine delete_case4(t, n)
        type(rbtree_t)               :: t
        type(rbtree_node_t), pointer :: n
        type(rbtree_node_t), pointer :: s
        s => sibling(n)
        if (node_colour(n%parent) == RED .and. &
            node_colour(s) == BLACK .and.      &
            node_colour(s%left) == BLACK .and. &
            node_colour(s%right) == BLACK) then
            call set_colour(s,RED)
            call set_colour(n%parent,BLACK)
        else
            call delete_case5(t, n)
        end if
    end subroutine delete_case4
    
    subroutine delete_case5(t, n)
        type(rbtree_t)               :: t
        type(rbtree_node_t), pointer :: n
        type(rbtree_node_t), pointer :: s
        s => sibling(n)
        if (associated(n, n%parent%left) .and. &
            node_colour(s) == BLACK .and.      &
            node_colour(s%left) == RED .and.   &
            node_colour(s%right) == BLACK) then
            
            call set_colour(s, RED)
            call set_colour(s%left, BLACK)
            call rotate_right(t, s)
            
        elseif (associated(n, n%parent%right) .and. &
            node_colour(s) == BLACK .and.           &
            node_colour(s%right) == RED .and.       &
            node_colour(s%left) == BLACK) then
                     
            call set_colour(s, RED)
            call set_colour(s%right, BLACK)
            call rotate_left(t, s)
        end if
        call delete_case6(t, n)

    end subroutine delete_case5
    
    subroutine delete_case6(t, n)
        type(rbtree_t)               :: t
        type(rbtree_node_t), pointer :: n
        type(rbtree_node_t), pointer :: s
        s => sibling(n)
        call set_colour(s, node_colour(n%parent))
        call set_colour(n%parent, BLACK)
        if (associated(n, n%parent%left)) then
            if (.not. node_colour(s%right) == RED) stop '***delete_case6a'
            call set_colour(s%right, BLACK)
            call rotate_left(t, n%parent)
        else
            if (.not. node_colour(s%left) == RED) stop '***delete_case6b'
            call set_colour(s%left, BLACK)
            call rotate_right(t, n%parent)
        end if
    end subroutine delete_case6
    
    end module rb_tree
    
    
    program test_rbtree
    
        use rb_tree
        
        type(rbtree_t) :: t
        real, target   :: h
        integer, target   :: hi
        class(*), pointer :: p
        integer :: i, j
        integer, target :: input(8) = [11, 1, 2, 5, 7, 8, 14, 15]
        logical :: ok
        
      !read(5,*)
        do i=1,size(input)
            print *,'Adding ',input(i)
            call rbtree_insert(t, input(i), i-1, compare_ints)
            call t%dump_tree
            print *,'===================================================='
        end do
        ok = verify_properties(t)
        print *, 'RB tree state = ',ok
        
        do i=1,size(input)
            print *,'Deleting ',input(i)
            p => input(i)
            call rbtree_delete(t, p, compare_ints)
            call t%dump_tree
            print *,'===================================================='
        end do
        
        do i=0,10000
            j = i/10
            call random_number(h)
            call rbtree_insert(t, int(10000*h), i-1, compare_ints)
            !call t%dump_tree
        end do
        print *,t%count
        call t%dump_tree
        
        do i=0,60000
            call random_number(h)
            hi = int(1000*h)
            p => hi
            call rbtree_delete(t, p, compare_ints)
        end do
    end program test_rbtree
    
