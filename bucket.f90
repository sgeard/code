! A bucket
!
! The idea is simple enough. You create a bucket of a particular size and capacity,
! with a filename then keep adding to it. When it's full the contents are automatically
! added to the specified file. The contents are a 1-d real(8).
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

module bucket
    implicit none
    
    public bucket_t
    private
    
    type bucket_t
        private
        integer                       :: high_water = 0
        character(len=:), allocatable :: file_name
        character(len=7)              :: file_status = 'replace'
        real(8), allocatable          :: contents(:,:)
        logical                       :: contents_written = .false.
    contains
        procedure :: add => add_bucket
        procedure :: empty => empty_bucket
        procedure :: create => initialize_bucket
        procedure :: have_contents => have_contents_bucket
    end type bucket_t
    
contains

    pure logical function have_contents_bucket(this)
        class(bucket_t), intent(in)  :: this
        have_contents_bucket = this%contents_written
    end function have_contents_bucket
    
    pure subroutine initialize_bucket(this, capacity, row_size, fname, no_delete)
        class(bucket_t), intent(inout)  :: this
        character(len=*), intent(in)    :: fname
        integer, intent(in)             :: capacity
        integer, intent(in)             :: row_size
        logical, intent(in), optional   :: no_delete
        
        allocate(this%contents(row_size, capacity))
        this%file_name = fname
        if (present(no_delete)) then
            if (no_delete) then
                this%file_status = 'old'
            end if
        end if
    end subroutine initialize_bucket
    
    subroutine add_bucket(this,item)
        class(bucket_t), intent(inout) :: this
        real(8), intent(in)            :: item(:)
 
        if (this%high_water == size(this%contents,2)) then
            call this%empty
        else
            this%high_water = this%high_water + 1
            this%contents(:,this%high_water) = item
        end if
    end subroutine add_bucket

    subroutine empty_bucket(this)
        class(bucket_t), intent(inout)  :: this
        integer :: i, k, u
        if (this%high_water > 0) then
            open(newunit=u, file=this%file_name, status=this%file_status)
            do k=1,this%high_water
                write(u,'(*(es13.5))') (this%contents(i,k),i=1,size(this%contents,1))
            end do
            this%high_water = 0
            if (this%file_status == 'replace') then
                this%file_status = 'old'
            end if
            close(u)
            this%contents_written = .true.
        end if
    end subroutine empty_bucket
    
end module bucket

