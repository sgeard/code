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

    type :: gplot_t
        character(len=:), allocatable :: gfile
        character(len=:), allocatable :: title
        character(len=:), allocatable :: xlabel
        character(len=:), allocatable :: ylabel
        logical                       :: ignore_first_row = .false.
        logical                       :: show_title = .true.
        logical                       :: keep_plot_files = .false.
        logical                       :: hide_x_labels = .false.
        logical                       :: hide_y_labels = .false.
    contains
        procedure                     :: create_plot
    end type gplot_t
    
    type, extends(gplot_t) :: contour_plot_t
        character(len=:), allocatable :: contour_function
    contains
        procedure :: write => write_contour_plot  
    end type contour_plot_t
    
    type, extends(gplot_t) :: histogram
    contains
        procedure :: write => write_gpl_hist    
    end type histogram
    
    type, extends(gplot_t) :: scatter
        logical   :: plot_with_dots = .false.
    contains
        procedure :: write => write_gpl_scat   
    end type scatter

    type, extends(gplot_t) :: line_plot_t
        integer                       :: plot_type = plot_type_lines
        logical                       :: plot_is_square = .false.
        character(len=:), allocatable :: legend
        character(len=:), allocatable :: pos
    contains
        procedure :: write => write_gpl_line
        procedure :: append => append_gpl_line
        procedure :: create => create_gpl_line
    end type line_plot_t

    type, extends(gplot_t) :: bar_plot_t
    contains
        procedure :: write => write_bar_plot
    end type bar_plot_t
    
    
    interface
    
        module subroutine write_contour_plot(this)
            class(contour_plot_t), intent(inout) :: this
        end subroutine write_contour_plot
    
        module subroutine write_bar_plot(this, data_file, columns)
            class(bar_plot_t), intent(inout) :: this
            character(len=*), intent(in)     :: data_file
            integer, optional, intent(in)    :: columns(2)
        end subroutine write_bar_plot
        
        module subroutine create_plot(this, fname)
            class(gplot_t), intent(in)               :: this
            character(len=*), intent(in) :: fname
        end subroutine create_plot
        
        module subroutine write_gpl_hist(this, data_file, ymax)
            class(histogram), intent(in)   :: this
            character(len=*), intent(in)   :: data_file
            real(8), optional,  intent(in) :: ymax
        end subroutine write_gpl_hist
        
        module subroutine write_gpl_scat(this,data_file)
            class(scatter), intent(in)   :: this
            character(len=*), intent(in) :: data_file
        end subroutine write_gpl_scat
        
        module subroutine write_gpl_line(this,data_file,columns)
            class(line_plot_t), intent(in)       :: this
            character(len=*), intent(in)  :: data_file
            integer, optional, intent(in) :: columns(2)
        end subroutine write_gpl_line
        
        module subroutine append_gpl_line(this,data_file,columns,force_create)
            class(line_plot_t), intent(in) :: this
            character(len=*), intent(in)   :: data_file
            integer, optional, intent(in)  :: columns(2)
            logical, optional, intent(in)  :: force_create
        end subroutine append_gpl_line
        
        module subroutine create_gpl_line(this)
            class(line_plot_t), intent(in) :: this
        end subroutine create_gpl_line

    end interface
    
end module gnu_plot
