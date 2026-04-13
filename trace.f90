! Copyright 2011 Simon Geard. All rights reserved.



module trace
  implicit none

  type trace_atom
     character(len=:), allocatable :: proc_name
  end type trace_atom

  type tracing
      integer           :: level = 0
      character(len=3)  :: indent = '   '
      type(trace_atom), pointer :: ta => null()
  contains
     procedure, public :: in
     procedure, public :: out
     procedure, public :: message
     procedure, public :: get_current_indent
     procedure, public :: print_current_indent
  end type tracing

  type(tracing), public :: tr
  logical, private      :: s_trace_is_on = .false.

contains

  subroutine set_trace_on
    s_trace_is_on = .true.
  end subroutine set_trace_on

  subroutine set_trace_off
    s_trace_is_on = .false.
  end subroutine set_trace_off

  logical function trace_is_on()
    trace_is_on = s_trace_is_on
  end function trace_is_on

  subroutine in(this, name)
      implicit none
      class(tracing), intent(inout) :: this
      character(len=*), intent(in) :: name
      type(trace_atom), pointer :: ta

      if (s_trace_is_on) then
          allocate(ta)
          ta%proc_name = name
          write(*,'(a)') repeat(this%indent,this%level)//name//'<in>'
      end if
      this%level = this%level + 1
  end subroutine in

  subroutine out(this, no_message)
    implicit none
    class(tracing), intent(inout) :: this
    logical, optional             :: no_message ! = .true.
    logical :: output_mess
    this%level = this%level - 1
    if (s_trace_is_on) then
       output_mess = merge(no_message, .true., present(no_message))
       if (output_mess) write(*,'(a)') repeat(this%indent,this%level)//'<out>'
    end if
  end subroutine out

  subroutine message(this, mess)
    class(tracing), intent(in)   :: this
    character(len=*), intent(in) :: mess
    if (s_trace_is_on) then
       write(*,'(a)') repeat(this%indent,this%level)//mess
    end if
  end subroutine message

  subroutine print_current_indent(this)
    class(tracing), intent(in)    :: this
    if (this%level > 0) then
       write(*,advance='no',fmt = '(a)') repeat(this%indent,this%level)
    end if
  end subroutine print_current_indent

  subroutine get_current_indent(this, indent, nindent)
    class(tracing), intent(in)    :: this
    character(len=*), intent(out) :: indent
    integer, intent(out)          :: nindent
    indent = repeat(this%indent,this%level)
    nindent = len(this%indent)*this%level
  end subroutine get_current_indent
end module trace
