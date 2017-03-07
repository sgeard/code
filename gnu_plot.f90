! Basic Fortran interface for gnuplot
!
! No tests here but it has been tested in anger!

module gnu_plot

    integer, parameter :: plot_type_points  = 1    
    integer, parameter :: plot_type_lines  = 2

    type, abstract :: gplot_t
        character(len=:), allocatable :: fstem
        character(len=:), allocatable :: title
        character(len=:), allocatable :: xlabel
        character(len=:), allocatable :: ylabel
    contains
    end type gplot_t
    
    type, extends(gplot_t) :: histogram
    
    contains
        procedure :: write => write_gpl_hist    
    end type histogram
    
    type, extends(gplot_t) :: scatter
    
    contains
        procedure :: write => write_gpl_scat   
    end type scatter

    type, extends(gplot_t) :: line
        integer :: plot_type = plot_type_lines
    contains
        procedure :: write => write_gpl_line
    end type line

contains

    subroutine create_plot(fname)
        character(len=*), intent(in) :: fname
        character(len=64) :: cmsg
        integer           :: cstat
        call execute_command_line('gnuplot '//fname,cmdstat=cstat,cmdmsg=cmsg)
        if (cstat /= 0) then
            write(*,*) cmsg
            stop 1
        end if    
    end subroutine create_plot

    subroutine append_plot(fname, plot_command)
        character(len=*), intent(in) :: fname, plot_command
        character(len=64)  :: cmsg
        character(len=120) :: line
        integer            :: cstat
        integer            :: u
        
        open(newunit=u,file=fname//'gpl',status='old',position='append',action='readwrite')
        backspace(u)
        read(u,'(a120)') line
        backspace(u)
        write(u,fmt='(a)',advance='no') trim(line)//', '//plot_command
        close(u)
        call execute_command_line('gnuplot '//fname//'gpl',cmdstat=cstat,cmdmsg=cmsg)
        if (cstat /= 0) then
            write(*,*) cmsg
            stop 1
        end if    
    end subroutine append_plot
    
    ! Write gnuplot file for histograms
    subroutine write_gpl_hist(this, data_file, ymax)
        class(histogram), intent(in)   :: this
        character(len=*), intent(in)   :: data_file
        real(8), optional,  intent(in) :: ymax
        integer           :: u
        character(len=:), allocatable :: fgpl
        fgpl = this%fstem//'gpl'
        open (newunit=u,file=fgpl,access='sequential',status='replace')
        write (u,'(a)') 'set xlabel "'//this%xlabel//'"'
        write (u,'(a)') 'set ylabel "Frequency"'
        write (u,'(a)') 'set title "'//this%title//'"'
        write (u,'(a)') 'set boxwidth 1 relative'
        write (u,'(a)') 'set term png truecolor'
        write (u,'(a)') 'set output "'//this%fstem//'png'
        write (u,'(a)') 'set style fill transparent solid 0.5 noborder'
        write (u,'(a)') 'set xtics rotate 90'
        if (present(ymax)) then
            write(u,'(a,f0.2,a)') 'set yrange [0:',ymax,']'
        end if    
        write (u,'(a)') 'set datafile separator ","'
        write (u,fmt='(a)',advance='no') 'plot "'//data_file//'" using 2:3:xticlabels(1) w boxes lc rgb"red" notitle'
        close(u)
        call create_plot(fgpl)
    end subroutine write_gpl_hist
    
    ! Write gnuplot file for scatter plots
    subroutine write_gpl_scat(this,data_file)
        class(scatter), intent(in)   :: this
        character(len=*), intent(in) :: data_file
        integer           :: u
        character(len=:), allocatable :: fgpl
        fgpl = this%fstem//'gpl'
        open (newunit=u,file=fgpl,access='sequential',status='replace')
        if (allocated(this%xlabel)) then
            write (u,'(a)') 'set xlabel "'//this%xlabel//'"'
        end if
        if (allocated(this%ylabel)) then
            write (u,'(a)') 'set ylabel "'//this%ylabel//'"'
        end if
        if (allocated(this%title)) then
            write (u,'(a)') 'set title "'//this%title//'"'
        end if
        write (u,'(a)') 'set term png truecolor'
        write (u,'(a)') 'set output "'//this%fstem//'png'
        write (u,fmt='(a)',advance='no') 'plot "'//data_file//'"'
        close(u)
        call create_plot(fgpl)
    end subroutine write_gpl_scat
    
    ! Write gnuplot file for line plots
    subroutine write_gpl_line(this,data_file,columns)
        class(line), intent(in)       :: this
        character(len=*), intent(in)  :: data_file
        integer, optional, intent(in) :: columns(2)
        
        integer                       :: u
        character(len=:), allocatable :: fgpl, ptype
        character(len=20)             :: col_sel
        
        fgpl = this%fstem//'gpl'
        open (newunit=u,file=fgpl,access='sequential',status='replace')
        if (allocated(this%xlabel)) then
            write (u,'(a)') 'set xlabel "'//this%xlabel//'"'
        end if
        if (allocated(this%ylabel)) then
            write (u,'(a)') 'set ylabel "'//this%ylabel//'"'
        end if
        if (allocated(this%title)) then
            write (u,'(a)') 'set title "'//this%title//'"'
        end if
        write (u,'(a)') 'set term png truecolor'
        write (u,'(a)') 'set output "'//this%fstem//'png'
        select case(this%plot_type)
        case (plot_type_lines)
            ptype = ' with lines'
        case (plot_type_points)
            ptype = ' with points'
        case default
            ptype = ' with linespoints'
        end select
        ! Apply column selector if given
        if (present(columns)) then
            col_sel = 'using x:y'
            write(col_sel(7:7),'(i1)') columns(1)
            write(col_sel(9:9),'(i1)') columns(2)
            write (u,fmt='(a)',advance='no') 'plot "'//data_file//'" '//trim(col_sel)//' '//ptype//' notitle'
        else
            write (u,fmt='(a)',advance='no') 'plot "'//data_file//'"'//ptype//' notitle'
        end if
        close(u)
        call create_plot(fgpl)
    end subroutine write_gpl_line

end module gnu_plot