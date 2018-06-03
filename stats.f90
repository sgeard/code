module stats

    ! ---   Initialization random extraction   ---
    public
    
    type histogram
        integer              :: nbins = 10
        real(8), allocatable :: frequency(:)
        real(8)              :: bin_min
        real(8)              :: bin_max
        logical              :: int_bin_size = .false.
    contains
        generic   :: create => create_hist1, create_hist2, create_hist3
        procedure :: create_hist1
        procedure :: create_hist2
        procedure :: create_hist3
        procedure :: write => write_histogram
        procedure :: get_bin_size
    end type histogram
    
    interface
        module real(8) function get_bin_size(this)
            class(histogram),  intent(in) :: this
        end function get_bin_size
        
        module subroutine create_hist1(this, col, mask, nbins)
            class(histogram), intent(out) :: this
            real(8), intent(in)           :: col(:)
            logical, intent(in), optional :: mask(:)
            integer, intent(in), optional :: nbins
        end subroutine create_hist1
        
        module subroutine create_hist2(this, col, bin_size, bin_range, mask)
            class(histogram), intent(out) :: this
            real(8), intent(in)           :: col(:)
            real(8), intent(in)           :: bin_size
            real(8), intent(in)           :: bin_range(2)
            logical, intent(in), optional :: mask(:)
        end subroutine create_hist2
        
        module subroutine create_hist3(this, col, bin_size, bin_range, mask)
            class(histogram), intent(out) :: this
            real(8), intent(in)           :: col(:)
            integer, intent(in)           :: bin_size
            integer, intent(in)           :: bin_range(2)
            logical, intent(in), optional :: mask(:)
        end subroutine create_hist3
        
        module subroutine write_histogram(this, u)
            class(histogram), intent(in) :: this
            integer, intent(in)          :: u
        end subroutine write_histogram

        module subroutine calculate_stats(col, mean, mask, standard_deviation, skewness)
            real(8), intent(in)            :: col(:)
            real(8), intent(out)           :: mean
            logical, intent(in), optional  :: mask(:)
            real(8), intent(out), optional :: standard_deviation
            real(8), intent(out), optional :: skewness
        end subroutine calculate_stats
    
        module function random() result(ran)
            real(8) :: ran
        end function random
        
        module function gasdev()
            real(8) :: gasdev
        end function gasdev
        
    end interface
    
end module stats

#ifdef TEST_STATS
program test_stats
    use stats
    implicit none
    real(8), allocatable :: d(:)
    real(8) :: mean, standard_deviation, skewness
    logical, allocatable :: mask(:)
    integer :: i
    
    d = [1, 2, 3, 4, 5]
    allocate(mask(size(d)))
    
    ! All entries
    mask = .true.
    call calculate_stats(d, mean, mask, standard_deviation, skewness)
    if (mean /= 3.0d0 .or. standard_deviation /= sqrt(2.0d0)) stop 1
    
    ! Even entries
    mask = [(mod(i,2)==0,i=1,5)]
    call calculate_stats(d, mean, mask, standard_deviation)
    if (mean /= 3 .or. standard_deviation /= 1) stop 1
    
    ! Odd entries
    mask = [(mod(i,2)==1,i=1,5)]
    call calculate_stats(d, mean, mask, standard_deviation)
    if (abs(mean-3.0d0)>1.0e-6 .or. abs(standard_deviation-sqrt(8.0d0/3))>1.0e-6) stop 1
    
    ! Calculate normal distribution mean=0, stddev=1
    deallocate(mask)
    deallocate(d)
    allocate(d(1000))
    allocate(mask(size(d)))
    d = [(gasdev(),i=1,size(d))]
    
    ! Choose positive values only, should give a +ve skew
    mask = (d>0)
    call calculate_stats(d, mean, mask, standard_deviation, skewness)
    if (skewness < 0.50d0) stop 1
    
    ! Choose negative values only, should give a -ve skew
    mask = (d<0)
    call calculate_stats(d, mean, mask, standard_deviation, skewness)
    if (skewness > -0.50d0) stop 1

end program test_stats
#endif

