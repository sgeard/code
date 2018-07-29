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

module bucket
    implicit none
    
    public bucket_t
    private
    
    type bucket_t
        private
        integer                       :: high_water = 0
        character(len=:), allocatable :: file_name
        real(8), allocatable          :: contents(:,:)
        logical                       :: contents_written = .false.
        logical                       :: is_real_bucket = .true.
    contains
        procedure :: add => add_bucket
        procedure :: delete => delete_bucket
        procedure :: empty => empty_bucket
        procedure :: create => initialize_bucket
        procedure :: have_contents => have_contents_bucket
        procedure :: get_all_contents_ref_bucket
        procedure :: get_layer_contents_ref_bucket
        procedure :: number_of_items => number_of_items_bucket
        procedure :: apply_linear_transform => apply_linear_transform_bucket
        generic, public :: get_contents_ref => get_all_contents_ref_bucket, get_layer_contents_ref_bucket
    end type bucket_t

    interface
        
        ! Apply a*contents + b
        module subroutine apply_linear_transform_bucket(this, n, a, b)
            class(bucket_t), intent(inout) :: this
            integer, intent(in)            :: n
            real(8), intent(in), optional  :: a, b
        end subroutine apply_linear_transform_bucket
        
        module subroutine delete_bucket(this)
            class(bucket_t), intent(inout) :: this
        end subroutine delete_bucket
    
        module pure integer function number_of_items_bucket(this)
            class(bucket_t), intent(in)  :: this
        end function number_of_items_bucket
    
        module function get_all_contents_ref_bucket(this) result(r)
            class(bucket_t), target, intent(in) :: this
            real(8), pointer :: r(:,:)
        end function get_all_contents_ref_bucket
    
        module function get_layer_contents_ref_bucket(this, n) result(r)
            class(bucket_t), target, intent(in) :: this
            integer, intent(in)                 :: n
            real(8), pointer :: r(:)
        end function get_layer_contents_ref_bucket
   
        module pure logical function have_contents_bucket(this)
            class(bucket_t), intent(in)  :: this
        end function have_contents_bucket
        
        module subroutine initialize_bucket(this, capacity, row_size, fname, no_delete)
            class(bucket_t), intent(inout)         :: this
            integer, intent(in)                    :: capacity
            integer, intent(in)                    :: row_size
            character(len=*), intent(in), optional :: fname
            logical, intent(in), optional          :: no_delete
        end subroutine initialize_bucket

        module subroutine add_bucket(this,item)
            class(bucket_t), intent(inout) :: this
            real(8), intent(in)            :: item(:)
        end subroutine add_bucket
        
        module subroutine empty_bucket(this)
            class(bucket_t), intent(inout)  :: this
        end subroutine empty_bucket
    end interface
       
end module bucket

