submodule (stats) stats_sm

    
contains

    module real(8) function get_bin_size(this)
        class(histogram),  intent(in) :: this
        get_bin_size = (this%bin_max-this%bin_min)/this%nbins
    end function get_bin_size
        
    ! Create a histogram given the number of bins
    module subroutine create_hist1(this, col, mask, nbins)
        class(histogram), intent(out) :: this
        real(8), intent(in)           :: col(:)
        logical, intent(in), optional :: mask(:)
        integer, intent(in), optional :: nbins
        logical, allocatable :: hist_mask(:)      ! Mask selecting those entries in col that are in the current interval
        real(8)              :: min_col, max_col, bin_size
        real(8), allocatable :: col_data(:)
        integer              :: i
        
        if (present(nbins)) then
            this%nbins = nbins
        end if
        
        ! Create local contiguous copy of the data being processed
        if (present(mask)) then
            col_data = pack(col,mask)
        else
            col_data = col
        end if
        
        ! Initialize the frequency
        allocate(this%frequency(0:this%nbins-1))
        
        ! Calculate the range of data in 'col' for each bin
        max_col = maxval(col_data)
        min_col = minval(col_data)
        if (max_col == min_col) then
            max_col = max_col + 0.5d0
            min_col = min_col - 0.5d0
        end if
        bin_size = (max_col-min_col)/this%nbins
        do i=0, this%nbins-1
            hist_mask  = (min_col + i*bin_size) < col_data .and. col_data <= (min_col + (i+1)*bin_size)
            this%frequency(i) = count(hist_mask)/bin_size
        end do
        this%bin_max = max_col
        this%bin_min = min_col
    end subroutine create_hist1

    ! Create a histogram given the bin size and range
    module subroutine create_hist2(this, col, bin_size, bin_range, mask)
        class(histogram), intent(out) :: this
        real(8), intent(in)           :: col(:)
        real(8), intent(in)           :: bin_size
        real(8), intent(in)           :: bin_range(2)
        logical, intent(in), optional :: mask(:)
        real(8), allocatable :: col_data(:)
        logical, allocatable :: hist_mask(:)      ! Mask selecting those entries in col that are in the current interval
        integer              :: i
        
        ! Create local contiguous copy of the data being processed
        if (present(mask)) then
            col_data = pack(col,mask)
        else
            col_data = col
        end if

        ! Initialize the frequency
        this%bin_max = bin_range(2)
        this%bin_min = bin_range(1)
        this%nbins = nint((this%bin_max-this%bin_min)/bin_size)
        allocate(this%frequency(0:this%nbins-1))
        
        ! Calculate the range of data in 'col' for each bin
        do i=0, this%nbins-1
            hist_mask  = (this%bin_min + i*bin_size) < col_data .and. &
                          col_data <= (this%bin_min + (i+1)*bin_size)
            this%frequency(i) = count(hist_mask)/bin_size
        end do
    end subroutine create_hist2

    ! Create a histogram given the bin size and range
    module subroutine create_hist3(this, col, bin_size, bin_range, mask)
        class(histogram), intent(out) :: this
        real(8), intent(in)           :: col(:)
        integer, intent(in)           :: bin_size
        integer, intent(in)           :: bin_range(2)
        logical, intent(in), optional :: mask(:)
        real(8)          :: r_bin_range(2)

        ! Check the range
        if (mod(bin_range(2)-bin_range(1),bin_size) /= 0) then
            stop '***Error[create_hist3]: inconsistent bin definition'
        end if
        
        r_bin_range = real(bin_range,8)
        call this%create(col, real(bin_size,8), r_bin_range, mask)
        this%int_bin_size = .true.
    end subroutine create_hist3
    
    ! Write histogram
    module subroutine write_histogram(this, u)
        class(histogram), intent(in) :: this
        integer, intent(in)          :: u
        integer :: i
        real(8) :: bin_size
        
        bin_size = (this%bin_max - this%bin_min)/this%nbins
        associate (bin_start=>this%bin_min, freq=>this%frequency)
            if (this%int_bin_size) then
                do i=0, this%nbins-1
                    write(u,'(2(a,i0),a,f0.2,a,f0.2)') '(',nint(bin_start+i*bin_size), &
                    '->',nint(bin_start+(i+1)*bin_size),'],',bin_start+i*bin_size+bin_size/2,',',freq(i)
                end do
            else
                do i=0, this%nbins-1
                    write(u,'(2(a,es9.2),a,es9.2,a,es9.2)') '(',bin_start+i*bin_size,'->', &
                    bin_start+(i+1)*bin_size,'],',bin_start+i*bin_size+bin_size/2,',',freq(i)
                end do
            end if
        end associate
    end subroutine write_histogram
    
    ! Calculate mean and standard deviation
    module subroutine calculate_stats(col, mean, mask, standard_deviation, skewness)
        real(8), intent(in)            :: col(:)
        real(8), intent(out)           :: mean
        logical, intent(in), optional  :: mask(:)
        real(8), intent(out), optional :: standard_deviation
        real(8), intent(out), optional :: skewness
        real(8) :: mv(3), ssdev
        integer :: i, n
        
        mv = 0
        if (present(mask)) then
            n = count(mask)
            if (n == 0) return
            
            !$omp parallel do shared(col, mask) reduction(+:mv)
            do i=1,size(col)
                if (mask(i)) then
                    mv(1) = mv(1) + col(i)
                    if (present(standard_deviation) .or. present(skewness)) then
                        mv(2) = mv(2) + col(i)**2
                        if (present(skewness)) then
                            mv(3)= mv(3)+ col(i)**3
                        end if
                    end if
                end if
            end do
            !$omp end parallel do
        
        else
            n = size(col)
            !$omp parallel do shared(col) reduction(+:mv)
            do i=1,n
                mv(1) = mv(1) + col(i)
                if (present(standard_deviation) .or. present(skewness)) then
                    mv(2) = mv(2) + col(i)**2
                    if (present(skewness)) then
                        mv(3)= mv(3)+ col(i)**3
                    end if
                end if
            end do
            !$omp end parallel do
        
        end if
        
        ! Mean and standard deviation
        mv = mv/n
        mean = mv(1)
        if (present(standard_deviation) .or. present(skewness)) then
            ssdev = sqrt(abs(mv(2) - mv(1)**2))  ! Rounding could make it < 0 so use abs to be safe
            if (present(skewness)) then
                skewness = (mv(3) - mean*(3*ssdev**2 + mean**2))/ssdev**3
            end if
            if (present(standard_deviation)) then
                standard_deviation = ssdev
            end if
        end if
            
    end subroutine calculate_stats

    module function random() result(ran)
!=======================================================================
!
!    This is an adapted version of subroutine RANECU written by
!    F. James (Comput. Phys. Commun. (1990) 60, 329-344), which 
!    has been modified to give a single random number at each call.
!    The seeds iseed1 and iseed2 must be initialized in the main 
!    program and transferred through the named common block /REED/.
!
!=======================================================================
    
        real(8) :: ran

        ! ---   Declaracion de variables   ---
        integer :: i1, i2, iz
        
        ! Saved data. Note that inline initialization implies 'save'
        integer :: iseed1 = 860934
        integer :: iseed2 = 542039

        ! ---   Parametros   ---
        real(8), parameter :: uscale=1.0d0/2.0d0**31
 
!     ---   Calculo del numero aleatorio   ---

        i1 = iseed1/53668
        iseed1 = 40014*(iseed1-i1*53668)-i1*12211

        if (iseed1 < 0) then
            iseed2 = iseed1+2147483563
        end if
 
        i2 = iseed2/52774
        iseed2 = 40692*(iseed2-i2*52774)-i2*3791

        if (iseed2 < 0) then
            iseed2 = iseed2+2147483399
        end if
 
        iz = iseed1-iseed2

        if (iz < 1) then
            iz = iz+2147483562
        end if

        ran = iz*uscale
        return

    end function random

!***********************************************************************

    module function gasdev()
!=======================================================================
!
!     Returns a normally distributed deviate with zero mean and unit
!     variance.
!
!=======================================================================

        real(8) :: gasdev
          
        ! ---   Variable declaration   ---
        real(8)       :: fac, r
        real(8)       :: v1, v2

        ! ---   Use static memory for persistence   ---
        real(8), save :: gset = 0
        logical, save :: have_extra_deviate = .false.

!       ---   We don't have an extra deviate handy, so ...   ---
        if (.not. have_extra_deviate) then

            uniform_square: do

                ! ---   pick two uniform numbers in the square (-1,+1)
 
                v1 = 2*random()-1
                v2 = 2*random()-1

                ! ---   See if they are in the unit circle   ---

                r = v1*v1+v2*v2

                ! ---   and if they are not, try again   ---
        
                if (r < 1 .and. r > 0) exit uniform_square

            end do uniform_square

!           ---  Make Box-Muller polar transformation to get 2 normal deviates   ---

            fac = sqrt(-2*log(r)/r)

!           ---   return one and save the other for the next time   ---

            gset = v1*fac
            gasdev = v2*fac
            have_extra_deviate = .true.

        else

!           ---   We have an extra deviate so return it and unset flag   ---

            gasdev = gset
            have_extra_deviate = .false.

        end if

        return

    end function gasdev

!***********************************************************************

end submodule stats_sm
