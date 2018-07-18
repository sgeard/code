! A bucket
!
! The idea is simple enough. You create a bucket of a particular size and capacity,
! with a filename then keep adding to it. When it's full the contents are automatically
! added to the specified file. The contents are a 1-d real(8).
! If the capacity is < 0 then a virtual bucket is created which just keeps growing until
! 'empty' is called at which point the internal high-water mark is reset to 0. Virtual buckets
! do not have to be associated with a file.
!
! Example
! =======
!       use bucket
!       type(bucket_t) :: bkt
!       real(8)        :: x(5)
!       integer        :: i
!       call bkt%create(capacity=1000, row_size=3, fname='my_results.txt')
!       x = 1
!       do i=1,10000
!           call bkt%add(x(1:3))  ! Every 1000 calls the data are added to the file and the bucket emptied
!       end do
!       call bkt%empty            ! Ensure anything left is flushed

submodule (bucket) bucket_sm
    
contains

    module subroutine delete_bucket(this)
        class(bucket_t), intent(inout) :: this
        logical :: file_exists
        integer :: u
        integer :: bucket_shape(2)
        if (this%is_real_bucket) then
            inquire(file=this%file_name, exist=file_exists)
            if (file_exists) then
                open(file=this%file_name,newunit=u)
                close(u,status='delete')
            end if
        end if
        bucket_shape = shape(this%contents)
        deallocate(this%contents)
        this%high_water = 0
        this%contents_written = .false.
    end subroutine delete_bucket

    module pure integer function number_of_items_bucket(this)
        class(bucket_t), intent(in)  :: this
        number_of_items_bucket = this%high_water
    end function number_of_items_bucket

    module function get_contents_ref_bucket(this) result(r)
        class(bucket_t), target, intent(in) :: this
        real(8), pointer :: r(:,:)
        r => this%contents(:,1:this%high_water)
    end function get_contents_ref_bucket
    
    module pure logical function have_contents_bucket(this)
        class(bucket_t), intent(in)  :: this
        have_contents_bucket = this%contents_written
    end function have_contents_bucket
    
    module subroutine initialize_bucket(this, capacity, row_size, fname, no_delete)
        class(bucket_t), intent(inout)         :: this
        integer, intent(in)                    :: capacity
        integer, intent(in)                    :: row_size
        character(len=*), intent(in), optional :: fname
        logical, intent(in), optional          :: no_delete
        
        if (capacity == 0) then
            stop '***Error: bucket capacity must be non-zero'
        end if
        if (row_size <= 0) then
            stop '***Error: row_size must be a positive integer'
        end if
        this%is_real_bucket = (capacity > 0)
        allocate(this%contents(row_size, abs(capacity)))
        if (present(fname)) then
            this%file_name = fname
        else
            if (capacity > 0) then
                stop '***Error: real bucket (capacity > 0) must have a file name'
            end if
        end if
        if (present(no_delete)) then
            if (no_delete) then
                this%contents_written = .true.
            end if
        end if
    end subroutine initialize_bucket
    
    module subroutine add_bucket(this,item)
        class(bucket_t), intent(inout) :: this
        real(8), intent(in)            :: item(:)
 
        if (this%is_real_bucket .and. len(this%file_name) == 0) then
            stop '***Error: bucket not created'
        end if

        if (size(item) /= size(this%contents,1)) then
            write(*,'(2(a,i0))') 'item size = ',size(item) , ' c.f. bucket row_size = ',size(this%contents,1)
            stop '***Error: cannot add item to bucket'
        end if
        
        if (this%high_water == size(this%contents,2)) then
            if (this%is_real_bucket) then
                ! A real bucket so empty it
                call this%empty
            else
                ! A virtual bucket so just double the size if the limit has been reached
                block
                    real(8), allocatable :: tmp(:,:)
                    allocate(tmp(size(this%contents,1),2*size(this%contents,2)))
                    tmp(:,1:this%high_water) = this%contents
                    call move_alloc(tmp,this%contents)
                end block
            end if
        end if
            
        this%high_water = this%high_water + 1
        this%contents(:,this%high_water) = item
    end subroutine add_bucket

    module subroutine empty_bucket(this)
        class(bucket_t), intent(inout)  :: this
        integer :: i, k, u
        ! This does nothing if the bucket is virtual
        if (this%is_real_bucket .and. this%high_water > 0) then
            if (this%contents_written) then
                open(newunit=u, file=this%file_name, status='old', position='append')
            else
                open(newunit=u, file=this%file_name, status='replace')
            end if
            do k=1,this%high_water
                write(u,'(*(es13.5))') (this%contents(i,k),i=1,size(this%contents,1))
            end do
            close(u,status='keep')
            this%contents_written = .true.
        end if
        this%high_water = 0
        return
    end subroutine empty_bucket
    
end submodule bucket_sm

