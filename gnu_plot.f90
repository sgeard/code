! Basic Fortran interface for gnuplot
! ===================================
!
! This module implements various plotting options for gnuplot.
!
! Currently all data are plotted from a file.
!
! The output is a .png file.
!
! Examples
! ========

! Lines
! -----

!     Declare an instance of the line type
!     use gnu_plot
!     type(line) :: gpl
!
!     Set the output file name
!     gpl%gfile = 'assign3.gpl'
      
!     Set the title and axes labels
!     gpl%xlabel = '"x"'
!     gpl%ylabel = '"y" rotate by 360'
!     gpl%title = 'Temperature Profile at time t'
      
!     Set the position of the legend
!     gpl%pos = 'bottom left vertical inside'

!     Create plots from data_files
!     call gpl%append(data_file1,force_create=.true.)
!     call gpl%append(data_file2)

!     Run the command, this will create the .png file
!     call  gpl%create()


module gnu_plot

    integer, parameter :: plot_type_points = 1    
    integer, parameter :: plot_type_lines  = 2

    type, abstract :: gplot_t
        character(len=:), allocatable :: gfile
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
        integer                       :: plot_type = plot_type_lines
        logical                       :: plot_is_square = .false.
        character(len=:), allocatable :: legend
        character(len=:), allocatable :: pos
    contains
        procedure :: write => write_gpl_line
        procedure :: append => append_gpl_line
        procedure :: create => create_gpl_line
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
        character(len=64)   :: cmsg
        character(len=1000) :: line
        integer             :: cstat
        integer             :: u
        
        open(newunit=u,file=fname//'gpl',status='old',position='append',action='readwrite')
        backspace(u)
        read(u,'(a1000)') line
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
        character(len=:), allocatable  :: fstem
        
        open (newunit=u,file=this%gfile,access='sequential',status='replace')
        write (u,'(a)') 'set xlabel "'//this%xlabel//'"'
        write (u,'(a)') 'set ylabel "Frequency"'
        write (u,'(a)') 'set title "'//this%title//'"'
        write (u,'(a)') 'set boxwidth 1 relative'
        write (u,'(a)') 'set term png truecolor'
        fstem = this%gfile(1:len(this%gfile)-3)
        write (u,'(a)') 'set output "'//fstem//'png'
        write (u,'(a)') 'set style fill transparent solid 0.5 noborder'
        write (u,'(a)') 'set xtics rotate 90'
        if (present(ymax)) then
            write(u,'(a,f0.2,a)') 'set yrange [0:',ymax,']'
        end if
        write (u,'(a)') 'set datafile separator ","'
        write (u,fmt='(a)',advance='no') 'plot "'//data_file//'" using 2:3:xticlabels(1) w boxes lc rgb"red" notitle'
        close(u)
        call create_plot(this%gfile)
    end subroutine write_gpl_hist
    
    ! Write gnuplot file for scatter plots
    subroutine write_gpl_scat(this,data_file)
        class(scatter), intent(in)   :: this
        character(len=*), intent(in) :: data_file
        integer                       :: u
        character(len=:), allocatable :: fstem
       
        open (newunit=u,file=this%gfile,access='sequential',status='replace')
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
        fstem = this%gfile(1:len(this%gfile)-3)
        write (u,'(a)') 'set output "'//fstem//'png"'
        write (u,fmt='(a)',advance='no') 'plot "'//data_file//'"'
        close(u)
        call create_plot(this%gfile)
    end subroutine write_gpl_scat
    
    ! Write gnuplot file for line plots
    subroutine write_gpl_line(this,data_file,columns)
        class(line), intent(in)       :: this
        character(len=*), intent(in)  :: data_file
        integer, optional, intent(in) :: columns(2)
        
        integer                       :: u
        character(len=:), allocatable :: fgpl, ptype
        character(len=20)             :: col_sel
        character(len=:), allocatable :: plot_command
        character(len=:), allocatable :: fstem
        
        open (newunit=u,file=this%gfile,access='sequential',status='replace')
        if (allocated(this%xlabel)) then
            write (u,'(a)') 'set xlabel '//this%xlabel
        end if
        if (allocated(this%ylabel)) then
            write (u,'(a)') 'set ylabel '//this%ylabel
        end if
        if (allocated(this%title)) then
            write (u,'(a)') 'set title "'//this%title//'"'
        end if
        if (allocated(this%pos)) then
            write (u,'(a)') 'set key '//this%pos
        end if
        write (u,'(a)') 'set term png truecolor'
        fstem = this%gfile(1:len(this%gfile)-3)
        write (u,'(a)') 'set output "'//fstem//'png"'
        select case(this%plot_type)
        case (plot_type_lines)
            ptype = ' with lines'
        case (plot_type_points)
            ptype = ' with points'
        case default
            ptype = ' with linespoints'
        end select
        if (this%plot_is_square) then
            write (u,'(a)') 'set size square'
        end if
        
        ! Apply column selector if given
        if (present(columns)) then
            col_sel = 'using x:y'
            write(col_sel(7:7),'(i1)') columns(1)
            write(col_sel(9:9),'(i1)') columns(2)
            plot_command = 'plot "'//data_file//'" '//trim(col_sel)//' '//ptype
        else
            plot_command = 'plot "'//data_file//'"'//ptype
        end if
        if (allocated(this%legend)) then
            plot_command = plot_command//' title "'//this%legend//'"'
        end if
        write(u,'(a)') plot_command
        close(u)
        call create_plot(this%gfile)
    end subroutine write_gpl_line
    
    ! Write gnuplot file for line plots
    subroutine append_gpl_line(this,data_file,columns,force_create)
        class(line), intent(in)       :: this
        character(len=*), intent(in)  :: data_file
        integer, optional, intent(in) :: columns(2)
        logical, optional, intent(in) :: force_create
        
        integer                       :: u
        character(len=1000)           :: line
        character(len=20)             :: col_sel
        character(len=:), allocatable :: plot_command, ptype
        logical                       :: gfile_exists
        logical                       :: create_new
        
        inquire(file=this%gfile,exist=gfile_exists)
        create_new = (.not. gfile_exists)
        if (.not. create_new .and. present(force_create)) then
            create_new = force_create
        end if
        if (force_create) then
            call this%write(data_file, columns)
            return
        end if
        open(newunit=u,file=this%gfile,status='old',position='append',action='readwrite')
        backspace(u)
        read(u,'(a1000)') line
        backspace(u)
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
            plot_command = '"'//data_file//'" '//trim(col_sel)//' '//ptype
        else
            plot_command = '"'//data_file//'"'//ptype
        end if
        if (allocated(this%legend)) then
            plot_command = plot_command//' title "'//this%legend//'"'
        end if
        write(u,fmt='(a)',advance='no') trim(line)//', '//plot_command
        close(u)

    end subroutine append_gpl_line

    subroutine create_gpl_line(this)
        class(line), intent(in) :: this
        call create_plot(this%gfile)
    end subroutine create_gpl_line

end module gnu_plot