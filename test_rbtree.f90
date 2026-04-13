! Test suite for the rb_tree module.
! Written by Claude (claude-sonnet-4-6).

program test_rbtree

    use rb_tree

    implicit none

    integer :: n_pass = 0, n_fail = 0

    call test_empty_tree()
    call test_insert_sorted()
    call test_insert_unsorted()
    call test_delete()
    call test_update()
    call test_count()
    call test_lookup()
    call test_equal()
    call test_large()

    write(*,'(/,a,i0,a,i0,a)') 'Results: ', n_pass, ' passed, ', n_fail, ' failed'

contains

    ! -----------------------------------------------------------------------
    ! Assertion helper
    ! -----------------------------------------------------------------------

    subroutine check(condition, message)
        logical,          intent(in) :: condition
        character(len=*), intent(in) :: message
        if (condition) then
            write(*,'(a,a)') '  PASS: ', message
            n_pass = n_pass + 1
        else
            write(*,'(a,a)') '  FAIL: ', message
            n_fail = n_fail + 1
        end if
    end subroutine check

    ! -----------------------------------------------------------------------
    ! Format function for integer key / integer value pairs
    ! -----------------------------------------------------------------------

    function fmt_ii(key, value) result(s)
        class(*), intent(in)          :: key, value
        character(len=:), allocatable :: s
        character(len=32) :: buf
        select type(key)
        type is (integer)
            select type(value)
            type is (integer)
                write(buf, '(i0,":",i0)') key, value
                s = trim(buf)
                return
            end select
        end select
        s = '?:?'
    end function fmt_ii

    ! -----------------------------------------------------------------------
    ! Lookup helper: returns value for integer key, or -huge(0) if not found
    ! -----------------------------------------------------------------------

    integer function ilookup(t, key)
        type(rbtree_t) :: t
        integer, intent(in), target :: key
        class(*), pointer :: kp, vp
        kp => key
        vp => rbtree_lookup(t, kp, compare_ints)
        if (.not. associated(vp)) then
            ilookup = -huge(0)
            return
        end if
        select type(vp)
        type is (integer)
            ilookup = vp
        class default
            ilookup = -huge(0)
        end select
    end function ilookup

    ! -----------------------------------------------------------------------

    subroutine test_empty_tree()
        type(rbtree_t) :: t
        write(*,'(/,a)') 'test_empty_tree'
        call check(t%count == 0,             'count is zero')
        call check(rbtree_serialize(t, fmt_ii) == '', 'serialize empty tree = ""')
        call check(verify_properties(t),     'properties hold on empty tree')
    end subroutine test_empty_tree

    ! -----------------------------------------------------------------------

    subroutine test_insert_sorted()
        type(rbtree_t) :: t
        integer :: i
        write(*,'(/,a)') 'test_insert_sorted'
        do i = 1, 5
            call rbtree_insert(t, i, i*10, compare_ints)
        end do
        call check(verify_properties(t), 'properties hold after sorted inserts')
        call check(rbtree_serialize(t, fmt_ii) == '1:10|2:20|3:30|4:40|5:50', &
                   'serialize after sorted inserts')
    end subroutine test_insert_sorted

    ! -----------------------------------------------------------------------

    subroutine test_insert_unsorted()
        type(rbtree_t) :: t
        write(*,'(/,a)') 'test_insert_unsorted'
        ! Same keys as test_insert_sorted but in a different order
        call rbtree_insert(t, 3, 30, compare_ints)
        call rbtree_insert(t, 1, 10, compare_ints)
        call rbtree_insert(t, 5, 50, compare_ints)
        call rbtree_insert(t, 2, 20, compare_ints)
        call rbtree_insert(t, 4, 40, compare_ints)
        call check(verify_properties(t), 'properties hold after unsorted inserts')
        call check(rbtree_serialize(t, fmt_ii) == '1:10|2:20|3:30|4:40|5:50', &
                   'serialize after unsorted inserts matches sorted order')
    end subroutine test_insert_unsorted

    ! -----------------------------------------------------------------------

    subroutine test_delete()
        type(rbtree_t) :: t
        integer, target :: k
        class(*), pointer :: kp
        write(*,'(/,a)') 'test_delete'
        call rbtree_insert(t, 1, 10, compare_ints)
        call rbtree_insert(t, 2, 20, compare_ints)
        call rbtree_insert(t, 3, 30, compare_ints)
        call rbtree_insert(t, 4, 40, compare_ints)
        call rbtree_insert(t, 5, 50, compare_ints)

        ! Delete a leaf
        k = 5; kp => k
        call rbtree_delete(t, kp, compare_ints)
        call check(verify_properties(t), 'properties hold after deleting leaf')
        call check(rbtree_serialize(t, fmt_ii) == '1:10|2:20|3:30|4:40', &
                   'serialize correct after deleting leaf (5)')

        ! Delete an internal node
        k = 2; kp => k
        call rbtree_delete(t, kp, compare_ints)
        call check(verify_properties(t), 'properties hold after deleting internal node')
        call check(rbtree_serialize(t, fmt_ii) == '1:10|3:30|4:40', &
                   'serialize correct after deleting internal node (2)')

        ! Delete root
        k = 3; kp => k
        call rbtree_delete(t, kp, compare_ints)
        call check(verify_properties(t), 'properties hold after deleting root')
        call check(rbtree_serialize(t, fmt_ii) == '1:10|4:40', &
                   'serialize correct after deleting root (3)')

        ! Delete all remaining
        k = 1; kp => k; call rbtree_delete(t, kp, compare_ints)
        k = 4; kp => k; call rbtree_delete(t, kp, compare_ints)
        call check(verify_properties(t), 'properties hold on empty tree after all deletions')
        call check(rbtree_serialize(t, fmt_ii) == '', 'tree empty after all deletions')
    end subroutine test_delete

    ! -----------------------------------------------------------------------

    subroutine test_update()
        type(rbtree_t) :: t
        write(*,'(/,a)') 'test_update'
        call rbtree_insert(t, 1, 10, compare_ints)
        call rbtree_insert(t, 2, 20, compare_ints)
        call rbtree_insert(t, 2, 99, compare_ints)   ! update existing key
        call check(t%count == 2, 'count unchanged after update of existing key')
        call check(rbtree_serialize(t, fmt_ii) == '1:10|2:99', &
                   'updated value reflected in serialize')
    end subroutine test_update

    ! -----------------------------------------------------------------------

    subroutine test_count()
        type(rbtree_t) :: t
        integer, target :: k
        class(*), pointer :: kp
        integer :: i
        write(*,'(/,a)') 'test_count'
        do i = 1, 8
            call rbtree_insert(t, i, i, compare_ints)
        end do
        call check(t%count == 8, 'count = 8 after 8 inserts')
        k = 3; kp => k; call rbtree_delete(t, kp, compare_ints)
        k = 7; kp => k; call rbtree_delete(t, kp, compare_ints)
        call check(t%count == 6, 'count = 6 after 2 deletes')
        ! Deleting a key not in the tree should not change count
        k = 99; kp => k; call rbtree_delete(t, kp, compare_ints)
        call check(t%count == 6, 'count unchanged after deleting absent key')
    end subroutine test_count

    ! -----------------------------------------------------------------------

    subroutine test_lookup()
        type(rbtree_t) :: t
        integer, target :: k
        class(*), pointer :: kp
        write(*,'(/,a)') 'test_lookup'
        call rbtree_insert(t, 10, 100, compare_ints)
        call rbtree_insert(t, 20, 200, compare_ints)
        call rbtree_insert(t, 30, 300, compare_ints)
        call check(ilookup(t, 10) == 100, 'lookup key 10 returns 100')
        call check(ilookup(t, 20) == 200, 'lookup key 20 returns 200')
        call check(ilookup(t, 30) == 300, 'lookup key 30 returns 300')
        call check(ilookup(t, 99) == -huge(0), 'lookup absent key returns sentinel')
        ! Update and re-lookup
        call rbtree_insert(t, 20, 999, compare_ints)
        call check(ilookup(t, 20) == 999, 'lookup after update returns new value')
        ! Lookup after deletion
        k = 10; kp => k; call rbtree_delete(t, kp, compare_ints)
        call check(ilookup(t, 10) == -huge(0), 'lookup deleted key returns sentinel')
    end subroutine test_lookup

    ! -----------------------------------------------------------------------

    subroutine test_equal()
        type(rbtree_t) :: t1, t2, t3
        integer :: i
        write(*,'(/,a)') 'test_equal'
        do i = 1, 5
            call rbtree_insert(t1, i, i*10, compare_ints)
            call rbtree_insert(t2, i, i*10, compare_ints)
            call rbtree_insert(t3, i, i*10, compare_ints)
        end do
        call check(rbtree_equal(t1, t2, compare_ints), &
                   'identical trees are equal')
        ! t3 gets an extra element — structurally different
        call rbtree_insert(t3, 6, 60, compare_ints)
        call check(.not. rbtree_equal(t1, t3, compare_ints), &
                   'trees with different sizes are not equal')
        ! Empty trees are equal
        call check(rbtree_equal(rbtree_t(), rbtree_t(), compare_ints), &
                   'two empty trees are equal')
    end subroutine test_equal

    ! -----------------------------------------------------------------------

    subroutine test_large()
        type(rbtree_t) :: t
        real    :: h
        integer :: i, target_count
        integer, target  :: k
        class(*), pointer :: kp
        write(*,'(/,a)') 'test_large'
        call random_seed()
        do i = 1, 10000
            call random_number(h)
            call rbtree_insert(t, int(10000*h), i, compare_ints)
        end do
        call check(verify_properties(t), 'properties hold after 10000 random inserts')
        target_count = t%count
        do i = 1, 5000
            call random_number(h)
            k = int(10000*h); kp => k
            call rbtree_delete(t, kp, compare_ints)
        end do
        call check(verify_properties(t), 'properties hold after 5000 random deletes')
        call check(t%count <= target_count, 'count did not increase after deletes')
    end subroutine test_large

end program test_rbtree
