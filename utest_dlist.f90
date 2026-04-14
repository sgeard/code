! Unit test for dlist code

! Accumulator state for test callbacks — in a module so that contained
! callback procedures access these via USE rather than non-local capture,
! working around a gfortran ICE in nested-function lowering.
module utest_state
    implicit none
    integer :: s   = 0
    real(8) :: acc(2,2) = 0.0d0
end module utest_state

program utest_dlist
    use dlist
    use utest_state
    implicit none

    type(dlist_t) :: my_dlist


    int_sum: block
        integer :: i, total
        logical :: ok

        total = 0
        do i=1,10
            total = total + i
            call my_dlist%append(int_node(i))
        end do

        ! Iterate and sum; result should equal total
        s = 0
        ok = my_dlist%iterate(adder)
        print '(a,l1)', 'int_sum pass: ', s == total

    end block int_sum

    ! Use matrix multiplication to verify traversal order.
    ! Forward product M1*M2*M3*M4 differs from reverse M4*M3*M2*M1
    ! for non-commuting matrices.
    mat: block
        real(8) :: fwd(2,2), rev(2,2)
        logical :: ok

        call my_dlist%clear()
        call my_dlist%append(make_node(reshape([1d0,0d0,1d0,1d0],[2,2])))  ! m1
        call my_dlist%append(make_node(reshape([2d0,0d0,0d0,1d0],[2,2])))  ! m2
        call my_dlist%append(make_node(reshape([1d0,1d0,0d0,1d0],[2,2])))  ! m3
        call my_dlist%append(make_node(reshape([0d0,1d0,1d0,0d0],[2,2])))  ! m4

        ! Forward: acc = m1*m2*m3*m4
        acc = reshape([1d0,0d0,0d0,1d0],[2,2])   ! identity
        ok = my_dlist%iterate(mat_mul)
        fwd = acc

        ! Reverse: acc = m4*m3*m2*m1
        acc = reshape([1d0,0d0,0d0,1d0],[2,2])   ! identity
        ok = my_dlist%reverse_iterate(mat_mul)
        rev = acc

        print '(a,l1)', 'mat_order pass: ', any(fwd /= rev)

    end block mat

    ! Test insert and remove
    ! Build list 2,3,4 then insert 1 at front and 5 at back; expect 1,2,3,4,5
    ! Then remove element 3 (value 3); expect 1,2,4,5
    insert_remove: block
        integer :: i
        logical :: ok

        call my_dlist%clear()
        do i=2,4
            call my_dlist%append(int_node(i))
        end do
        call my_dlist%insert(0, int_node(1))                    ! prepend
        call my_dlist%insert(my_dlist%size(), int_node(5))      ! append

        s = 0
        ok = my_dlist%iterate(adder)
        print '(a,l1)', 'insert pass:  ', s == 15 .and. my_dlist%size() == 5

        call my_dlist%remove(3)   ! remove value 3 (3rd element)
        s = 0
        ok = my_dlist%iterate(adder)
        print '(a,l1)', 'remove pass:  ', s == 12 .and. my_dlist%size() == 4

    end block insert_remove

contains

    subroutine adder(node, ok)
        use utest_state, only: s
        class(dlist_node_data_t), intent(in) :: node
        logical, intent(out) :: ok
        ok = .true.
        select type(node)
        class is (dlist_node_integer)
            s = s + node%data
        end select
    end subroutine adder

    subroutine mat_mul(node, ok)
        use utest_state, only: acc
        class(dlist_node_data_t), intent(in) :: node
        logical, intent(out) :: ok
        ok = .true.
        select type(node)
        class is (dlist_node_real_m)
            acc = matmul(acc, node%data)
        end select
    end subroutine mat_mul

end program utest_dlist
