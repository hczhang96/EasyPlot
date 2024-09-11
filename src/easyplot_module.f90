module easyplot_module
  use iso_fortran_env
  use strings_module, only: split
  implicit none
  
  private

  integer, parameter, public :: wp = real64 !! kind of real.name.
  character(len=*), parameter :: python_exe = 'python' !! The python executable name.
  character(len=*), parameter :: int_fmt = '(I10)' !! Integer format string.
  character(len=*), parameter :: real_fmt_default = '(E30.16)' !! default real number format string
  integer, parameter          :: max_int_len      = 10         !! max string length for integers
  integer, parameter          :: max_real_len     = 60         !! max string length for reals

  type, public :: easyplot 
    !!> @brief The easyplot class
    private

    character(len=15), public :: path_plot = './visuals/' !! path to save the plot
    character(len=50), public :: file_name  !! file name
    character(len=100),public :: file_path  !! path to save the plot
    character(len=:), allocatable :: header !! header string buffer
    character(len=:), allocatable :: body   !! body string buffer
    character(len=:), allocatable :: rest   !! rest string buffer
    character(len=1) :: raw_str_token = ' ' !! will be 'r' if using raw strings
    character(len=:), allocatable :: xlabels !! x-axis labels
    character(len=:), allocatable :: ylabels !! y-axis labels
    character(len=:), allocatable :: zlabels !! z-axis labels

    integer :: istatus = 1 !! status of the plot (0 means not problems)
    real(wp) :: pause_time = 0.0_wp !! pause time for the plot

    logical :: show_legend = .false.     !! show legend into plot
    logical :: show_grad   = .false.     !! show grid into plot
    logical :: hold_state  = .false.     !! hold the plot state
    logical :: use_numpy   = .true.      !! use numpy python module
    logical :: use_oo_api  = .false.     !! use OO interface of matplotlib (incopatible with showfig subroutine)
    logical :: mplot3d     = .false.     !! it is a 3d plot
    logical :: polar       = .false.     !! it is a polar plot
    logical :: axis_equal  = .false.     !! equal scale on each axis
    logical :: axisbelow   = .true.      !! axis below other chart elements
    logical :: tight_layout = .false.    !! tight layout option
    logical :: usetex      = .false.     !! enable LaTeX

    character(len=:),allocatable :: xaxis_date_fmt  !! date format for the x-axis. Example: `"%m/%d/%y %H:%M:%S"`
    character(len=:),allocatable :: yaxis_date_fmt  !! date format for the y-axis. Example: `"%m/%d/%y %H:%M:%S"`
    character(len=:),allocatable :: real_fmt  !! real number formatting

  contains

    procedure, public :: initialize        !! Initialize the pyplot handler
    procedure :: initialize_file           !! Initialize the file name
    procedure :: initialize_figure         !! Initialize the figure
    procedure :: initialize_plot_directory !! Initialize the variable directory

    procedure, public :: legend => add_legend   !! add legend to the plot
    procedure, public :: labels => add_labels   !! add labels to the plot
    procedure, public :: hold   => set_hold     !! hold the plot state
    procedure, public :: grad   => add_grad     !! add grid to the plot
    procedure, public :: plot   => add_plot     !! add a plot to the figure
    procedure, public :: show   => show_figure  !! show the figure in a window
    procedure, public :: save   => save_figure  !! save the figure to a .png file 
    procedure :: add_legend     
    procedure :: add_grad    
    procedure :: add_labels
    procedure :: set_hold
    procedure :: add_plot
    procedure :: show_figure     !! show the figure
    procedure :: save_figure     !! save the figure

    procedure, public :: pause => set_pause_time !! set the pause time
    procedure :: set_pause_time 

    procedure :: add_header      !! add a string to the header buffer
    procedure :: add_body        !! add a string to the 
    procedure :: add_rest        !! add a string to the rest buffer
    procedure :: destroy         !! destroy pyplot instance
    procedure :: execute         !! execute the python script
    procedure :: finish_ops               !! Some final things to add before saving or showing the figure
    procedure :: write_to_file

  end type easyplot

contains

  subroutine destroy(me)

    class(easyplot),intent(inout) :: me !! pyplot handler

    if (allocated(me%header))   deallocate(me%header)
    me%header = '' !! reset the header buffer
    if (allocated(me%body))     deallocate(me%body)
    me%body = '' !! reset the body buffer
    if (allocated(me%rest))     deallocate(me%rest)
    me%rest = '' !! reset the rest buffer
    if (allocated(me%real_fmt)) deallocate(me%real_fmt)
    me%real_fmt = real_fmt_default
    if (allocated(me%xlabels))  deallocate(me%xlabels)
    if (allocated(me%ylabels))  deallocate(me%ylabels)
    if (allocated(me%zlabels))  deallocate(me%zlabels)

    me%raw_str_token = ' '

  end subroutine destroy

  !> @brief Add a string to header buffer.
  subroutine add_header(me, string)

    class(easyplot), intent(inout) :: me  !! pyplot handler
    character(len=*), intent(in)  :: string !! string to be added to pyplot handler buffer

    integer :: n_old !! current `me%header` length
    integer :: n_str !! length of input `string`
    character(len=:),allocatable :: tmp !! tmp string for building the result

    if (len(string)==0) return

    if (allocated(me%header)) then
        n_old = len(me%header)
        n_str = len(string)
        allocate(character(len=n_old+n_str+1) :: tmp)
        tmp(1:n_old) = me%header
        tmp(n_old+1:) = string//new_line(' ')
        call move_alloc(tmp, me%header)
    else
        allocate(me%header, source = string//new_line(' '))
    end if

  end subroutine add_header

  !> @brief Add a string to the plot body buffer.
  subroutine add_body(me, string)

    class(easyplot), intent(inout) :: me  !! pyplot handler
    character(len=*), intent(in)  :: string !! string to be added to pyplot handler buffer

    integer :: n_old !! current `me%body` length
    integer :: n_str !! length of input `string`
    character(len=:),allocatable :: tmp !! tmp string for building the result

    ! original
    !me%body = me%body//string//new_line(' ')

    if (len(string)==0) return

    ! the above can sometimes cause a stack overflow in the
    ! intel Fortran compiler, so we replace with this:
    if (allocated(me%body)) then
        n_old = len(me%body)
        n_str = len(string)
        allocate(character(len=n_old+n_str+1) :: tmp)
        tmp(1:n_old) = me%body
        tmp(n_old+1:) = string//new_line(' ')
        call move_alloc(tmp, me%body)
    else
        allocate(me%body, source = string//new_line(' '))
    end if

  end subroutine add_body

  !> @brief Add a string to the rest buffer.
  subroutine add_rest(me, string)

    class(easyplot), intent(inout) :: me  !! pyplot handler
    character(len=*), intent(in)  :: string !! string to be added to pyplot handler buffer

    integer :: n_old !! current `me%rest` length
    integer :: n_str !! length of input `string`
    character(len=:),allocatable :: tmp !! tmp string for building the result

    if (len(string)==0) return

    if (allocated(me%rest)) then
        n_old = len(me%rest)
        n_str = len(string)
        allocate(character(len=n_old+n_str+1) :: tmp)
        tmp(1:n_old) = me%rest
        tmp(n_old+1:) = string//new_line(' ')
        call move_alloc(tmp, me%rest)
    else
        allocate(me%rest, source = string//new_line(' '))
    end if

  end subroutine add_rest


  !> @brief Initialize the plot storage directory.
  subroutine initialize_plot_directory(me)
    implicit none
    class(easyplot) :: me
    logical :: isExist

    inquire(file=trim(me%path_plot), exist=isExist)
    if (.not.isExist) then  ! if the directory does not exist
      call system('mkdir "'//trim(me%path_plot)//'"') ! create the directory
      open(101, file=trim(me%path_plot)//'README.txt', status='unknown')
      write(101,*) '!============================================!'
      write(101,*) '! This directory is used to save the figure. !'
      write(101,*) '!                                            !'
      write(101,*) '!                    -- Haicheng Zhang, ECNU !'
      write(101,*) '!============================================!'
      close(101)
    end if

  end subroutine initialize_plot_directory

  !> @brief Initialize the pyplot handler.
  subroutine initialize( me, filename, figsize, font_size, axis_equal, &
                         legend, grid, hold, &
                         axes_labelsize, xtick_labelsize,ytick_labelsize,  &
                         ztick_labelsize )
    implicit none
    class(easyplot), intent(inout) :: me
    character(len=*), optional, intent(in) :: filename !! file name is must be provided
    integer, intent(in), optional :: figsize(2)
    integer, intent(in), optional :: font_size
    logical, intent(in), optional :: axis_equal
    logical, intent(in), optional :: legend
    logical, intent(in), optional :: grid
    logical, intent(in), optional :: hold
    integer, intent(in), optional :: axes_labelsize
    integer, intent(in), optional :: xtick_labelsize
    integer, intent(in), optional :: ytick_labelsize
    integer, intent(in), optional :: ztick_labelsize

    ! Initialize the variable directory
    call me%initialize_plot_directory()
    ! Initialize the file name
    call me%initialize_file(filename)
    ! Initialize the figure
    call me%initialize_figure( figsize=figsize, axis_equal=axis_equal, &
                               font_size=font_size, axes_labelsize=axes_labelsize, &
                               xtick_labelsize=xtick_labelsize, &
                               ytick_labelsize=ytick_labelsize, &
                              ztick_labelsize=ztick_labelsize )
    ! set the initial status
    me%istatus = 0    ! 0 is no problems, initialization successful
    if ( present(legend) ) call me%legend(legend) ! add legend
    if ( present(grid) ) call me%grad(grid)       ! add grid
    if ( present(hold) ) call me%hold(hold)       ! hold the plot state

  end subroutine initialize
  
  !> @brief Initialize the file name.
  subroutine initialize_file( me, filename )
    implicit none

    class(easyplot), intent(inout) :: me
    character(len=*), optional, intent(in) :: filename
    ! local variables
    character(len=50) :: name_pre, datetime
    character(len=8) :: date, timer

    ! set the file path
    if ( present(filename) .and. filename /= '' ) then
      me%file_path = trim(adjustl(me%path_plot))//trim(adjustl(filename))
      me%file_name = trim(adjustl(filename))
    else
      call date_and_time(date, timer)
      name_pre = 'figure_'
      datetime = trim(date(5:6)//date(7:8)//timer(1:6))
      me%file_name = trim(name_pre)//trim(datetime)
      me%file_path = trim(adjustl(me%path_plot))//trim(adjustl(me%file_name))
    end if
    
  end subroutine initialize_file

  !> @brief Initialize the figure.
  subroutine initialize_figure( me, figsize, font_size, axis_equal, &
                                axes_labelsize, xtick_labelsize, &
                                ytick_labelsize, ztick_labelsize )
    implicit none

    class(easyplot), intent(inout) :: me
    integer, intent(in), optional :: figsize(2)
    integer, intent(in), optional :: font_size
    logical, intent(in), optional :: axis_equal
    integer, intent(in), optional :: axes_labelsize
    integer, intent(in), optional :: xtick_labelsize
    integer, intent(in), optional :: ytick_labelsize
    integer, intent(in), optional :: ztick_labelsize
    ! local variables    
    character(len=max_int_len)  :: width_str             !! figure width dummy string
    character(len=max_int_len)  :: height_str            !! figure height dummy string
    character(len=max_int_len)  :: font_size_str         !! font size dummy string
    character(len=max_int_len)  :: axes_labelsize_str    !! size of axis labels dummy string
    character(len=max_int_len)  :: xtick_labelsize_str   !! size of x axis tick labels dummy string
    character(len=max_int_len)  :: ytick_labelsize_str   !! size of x axis tick labels dummy string
    character(len=max_int_len)  :: ztick_labelsize_str   !! size of z axis tick labels dummy string
    character(len=:),allocatable :: python_fig_func      !! Python's function for creating a new Figure instance
    character(len=*), parameter :: default_font_size_str = '10' !! the default font size for plots

    call me%destroy()
    ! set figure size
    if (present(figsize)) then
      call integer_to_string(figsize(1), width_str)
      call integer_to_string(figsize(2), height_str)
    end if
    ! set equal axis
    if (present(axis_equal)) then
      me%axis_equal = axis_equal
    else
      me%axis_equal = .false.
    end if

    call optional_int_to_string(font_size, font_size_str, default_font_size_str)
    call optional_int_to_string(axes_labelsize, axes_labelsize_str, default_font_size_str)
    call optional_int_to_string(xtick_labelsize, xtick_labelsize_str, default_font_size_str)
    call optional_int_to_string(ytick_labelsize, ytick_labelsize_str, default_font_size_str)
    call optional_int_to_string(ztick_labelsize, ztick_labelsize_str, default_font_size_str)

    me%header = ''

    call me%add_header('#!/usr/bin/env python')
    call me%add_header('')

    call me%add_header('import matplotlib')
    call me%add_header('import matplotlib.pyplot as plt')
    call me%add_header('import numpy as np')
    call me%add_header('')

    call me%add_header('matplotlib.rcParams["font.family"] = "Serif"')
    call me%add_header('matplotlib.rcParams["font.size"] = '//trim(font_size_str))
    call me%add_header('matplotlib.rcParams["axes.labelsize"] = '//trim(axes_labelsize_str))
    call me%add_header('matplotlib.rcParams["xtick.labelsize"] = '//trim(xtick_labelsize_str))
    call me%add_header('matplotlib.rcParams["ytick.labelsize"] = '//trim(ytick_labelsize_str))
    if (me%usetex) call me%add_header('matplotlib.rcParams["text.usetex"] = True')

    call me%add_header('')

    python_fig_func = 'plt.figure'
    if (present(figsize)) then  !if specifying the figure size
      call me%add_header('fig = '//python_fig_func//'(figsize=('//trim(width_str)//','//trim(height_str)//'),facecolor="white")')
    else
      call me%add_header('fig = '//python_fig_func//'(facecolor="white")')
    end if

    call me%add_header('ax = fig.add_subplot(1, 1, 1)')
    call me%add_header('ax.set_axisbelow(True)')
    call me%add_header('')
    ! call me%add_header('plt.ion()')   ! turn on interactive mode
  
  end subroutine initialize_figure

  !> @brief Add a plot to the figure.
  subroutine add_plot( me, x, y, title, label, linestyle, markersize, linewidth, &
                       xlim, ylim, xscale, yscale, color )
    implicit none
    
    class(easyplot), intent(inout) :: me
    real(wp), dimension(:), intent(in) :: x, y
    character(len=*), intent(in) :: label
    character(len=*), intent(in) :: linestyle
    integer, intent(in), optional :: markersize
    integer, intent(in), optional :: linewidth
    real(wp), intent(in), optional :: xlim(2)
    real(wp), intent(in), optional :: ylim(2)
    character(len=*), optional, intent(in) :: title
    character(len=*), intent(in), optional :: xscale
    character(len=*), intent(in), optional :: yscale
    real(wp), dimension(:), intent(in), optional :: color
    ! local variables
    character(len=:), allocatable :: tmp_str
    character(len=:), allocatable :: arg_str      !! the arguments to pass to `plot`
    character(len=:), allocatable :: xstr         !! x values stringified
    character(len=:), allocatable :: ystr         !! y values stringified
    character(len=:), allocatable :: xlimstr      !! xlim values stringified
    character(len=:), allocatable :: ylimstr      !! ylim values stringified
    character(len=:), allocatable :: color_str    !! color values stringified
    character(len=max_int_len)    :: imark        !! actual markers size
    character(len=max_int_len)    :: iline        !! actual line width
    character(len=*), parameter   :: xname = 'x'  !! x variable name for script
    character(len=*), parameter   :: yname = 'y'  !! y variable name for script

    if (allocated(me%body)) then
      !axis limits (optional):
      if (present(xlim)) call vec_to_string(xlim, me%real_fmt, xlimstr, me%use_numpy)
      if (present(ylim)) call vec_to_string(ylim, me%real_fmt, ylimstr, me%use_numpy)

      !convert the arrays to strings:
      call vec_to_string(x, me%real_fmt, xstr, me%use_numpy)
      call vec_to_string(y, me%real_fmt, ystr, me%use_numpy)

      !get optional inputs (if not present, set default value):
      call optional_int_to_string(markersize, imark, '3')
      call optional_int_to_string(linewidth, iline, '3')

      !write the arrays:
      call me%add_body(trim(xname)//' = '//xstr)
      call me%add_body(trim(yname)//' = '//ystr)
      call me%add_body(' ')

      !main arguments for plot:
      arg_str = trim(xname)//','//&
                trim(yname)//','//&
                trim(me%raw_str_token)//'"'//trim(linestyle)//'",'//&
                'linewidth='//trim(adjustl(iline))//','//&
                'markersize='//trim(adjustl(imark))//','//&
                'label='//trim(me%raw_str_token)//'"'//trim(label)//'"'

      ! optional arguments:
      if (present(color)) then
          if (size(color)<=3) then
              call vec_to_string(color(1:3), '*', color_str, use_numpy=.false., is_tuple=.true.)
              arg_str = arg_str//',color='//trim(color_str)
          end if
      end if

      if ( me%hold_state .neqv. .true. ) call me%add_body('ax.cla()')
      ! if ( me%hold_state .neqv. .true. ) call me%add_body('plt.clf()')
      ! write the plot statement:
      call me%add_body('ax.plot('//arg_str//')')
      ! title 
      if ( present(title) ) call me%add_body('ax.set_title("'//trim(title)//'",loc="center")')
      ! legend
      if ( me%show_legend ) call me%add_body('ax.legend(loc="upper right")')
      ! grid
      call logical_to_string(me%show_grad, tmp_str)
      if ( me%show_grad ) call me%add_body('ax.grid("'//trim(tmp_str)//'")')
      ! labels
      if (allocated(me%xlabels)) call me%add_body('ax.set_xlabel("'//trim(me%xlabels)//'")')
      if (allocated(me%ylabels)) call me%add_body('ax.set_ylabel("'//trim(me%ylabels)//'")')
      if (allocated(me%zlabels)) call me%add_body('ax.set_zlabel("'//trim(me%zlabels)//'")')
      ! pause time
      if (me%pause_time > 0.0_wp) then
        call real_to_string(me%pause_time, me%real_fmt, arg_str)
        call me%add_body('plt.pause('//arg_str//')')
      end if
      ! axis limits:
      if (allocated(xlimstr)) call me%add_body('ax.set_xlim('//xlimstr//')')
      if (allocated(ylimstr)) call me%add_body('ax.set_ylim('//ylimstr//')')

      ! axis scales:
      if (present(xscale)) call me%add_body('ax.set_xscale('//trim(me%raw_str_token)//'"'//xscale//'")')
      if (present(yscale)) call me%add_body('ax.set_yscale('//trim(me%raw_str_token)//'"'//yscale//'")')

      call me%add_body(' ')

    else
      me%istatus = 1    ! initialization failed
      write(error_unit,'(A)') 'Error in add_plot: pyplot class not properly initialized.'
    end if

  end subroutine add_plot

  !> @brief Write the buffer to a file, and then execute it with Python.
  subroutine write_to_file( me )
    implicit none
    
    class(easyplot), intent(inout) :: me
    ! local variables
    character(len=100) :: filename
    integer :: iostat, unit_in

    filename = trim(me%file_path)//'.py'     
    !open the file:
    open(newunit=unit_in, file=filename, status='REPLACE', iostat=iostat)
    if (iostat/=0) then
      write(error_unit,'(A)') 'Error opening file: '//filename
      return
    end if
    
    !write to the file:
    write(unit_in, '(A)') me%header
    write(unit_in, '(A)') me%body
    write(unit_in, '(A)') me%rest

    close(unit_in, iostat=iostat)
    ! call execute_command_line('python'//' "'//filename//'"')

  end subroutine write_to_file

  !> @brief Show the figure.
  subroutine show_figure( me )
    implicit none

    class(easyplot), intent(inout) :: me

    if ( me%istatus /= 0 ) then
      write(error_unit,'(A)') 'Error in show_figure: pyplot class not properly initialized.'
      return
    end if
    
    call me%add_header('plt.ion()')   ! turn on interactive mode
    me%rest = ''
    call me%finish_ops()              ! add some final operations
    call me%add_rest('plt.ioff()')    ! turn off interactive mode
    call me%add_rest('print(" Note: Close the figure to continue ...")')
    call me%add_rest('plt.show()')    ! show the figure
    call me%write_to_file()
    call me%execute()

  end subroutine show_figure

  !> @brief Save the figure.
  subroutine save_figure( me, filename )
    implicit none

    class(easyplot), intent(inout) :: me
    character(len=*), intent(in), optional :: filename

    ! local variables
    character(len=:), dimension(:), allocatable :: array
    character(len=:),allocatable :: tmp_str
    if ( me%istatus /= 0 ) then
      write(error_unit,'(A)') 'Error in save_figure: pyplot class not properly initialized.'
      return
    end if

    if ( present(filename) ) then
      call split(filename, array, delimiters='.')
      tmp_str = array(size(array)-1)
      me%file_path = trim(adjustl(me%path_plot))//trim(adjustl(tmp_str))
    end if
    me%rest = ''
    call me%finish_ops()          ! add some final operations
    ! call me%add_rest('fig.savefig("'//trim(me%file_path)//'.png")')
    call me%add_rest('fig.savefig("'//trim(me%file_path)//'")') ! save the figure  
    call me%add_rest('print(" Saved figure to: '//trim(me%path_plot)//'" )') 
    call me%write_to_file()
    call me%execute()

  end subroutine save_figure

  
  !> @brief Some final things to add before saving or showing the figure.
  subroutine finish_ops( me )

    class(easyplot),intent(inout) :: me  !! pyplot handler
    
    if (me%axis_equal) then
      if (me%mplot3d) then
        call me%add_rest('ax.set_aspect("auto")')
        call me%add_rest('')

        call me%add_rest('def set_axes_equal(ax):')
        call me%add_rest('    x_limits = ax.get_xlim3d()')
        call me%add_rest('    y_limits = ax.get_ylim3d()')
        call me%add_rest('    z_limits = ax.get_zlim3d()')
        call me%add_rest('    x_range = abs(x_limits[1] - x_limits[0])')
        call me%add_rest('    x_middle = np.mean(x_limits)')
        call me%add_rest('    y_range = abs(y_limits[1] - y_limits[0])')
        call me%add_rest('    y_middle = np.mean(y_limits)')
        call me%add_rest('    z_range = abs(z_limits[1] - z_limits[0])')
        call me%add_rest('    z_middle = np.mean(z_limits)')
        call me%add_rest('    plot_radius = 0.5*max([x_range, y_range, z_range])')
        call me%add_rest('    ax.set_xlim3d([x_middle - plot_radius, x_middle + plot_radius])')
        call me%add_rest('    ax.set_ylim3d([y_middle - plot_radius, y_middle + plot_radius])')
        call me%add_rest('    ax.set_zlim3d([z_middle - plot_radius, z_middle + plot_radius])')
        call me%add_rest('set_axes_equal(ax)')

      else
        call me%add_rest('ax.axis("equal")')
      end if
      call me%add_rest('')
    end if
    if (allocated(me%xaxis_date_fmt) .or. allocated(me%yaxis_date_fmt)) then
      call me%add_rest('from matplotlib.dates import DateFormatter')
      if (allocated(me%xaxis_date_fmt)) &
        call me%add_rest('ax.xaxis.set_major_formatter(DateFormatter("'//trim(me%xaxis_date_fmt)//'"))')
      if (allocated(me%yaxis_date_fmt)) &
        call me%add_rest('ax.yaxis.set_major_formatter(DateFormatter("'//trim(me%yaxis_date_fmt)//'"))')
      call me%add_rest('')
    end if
    if (me%tight_layout) then
      call me%add_rest('fig.tight_layout()')
      call me%add_rest('')
    end if

  end subroutine finish_ops

  !> @brief set the hold state.
  subroutine set_hold( me, hold_state )
    implicit none

    class(easyplot), intent(inout) :: me
    logical, optional, intent(in) :: hold_state

    if ( me%istatus /= 0 ) then
      write(error_unit,'(A)') 'Error in set_hold: pyplot class not properly initialized.'
      return
    end if
    if (present(hold_state)) then
      me%hold_state = hold_state
    else
      me%hold_state = .true.
    end if
    
  end subroutine set_hold
  
  !> @brief add legend to the plot.
  subroutine add_legend( me, legend )
    implicit none

    class(easyplot), intent(inout) :: me    
    logical, intent(in), optional :: legend  

    if ( me%istatus /= 0 ) then
      write(error_unit,'(A)') 'Error in add_legend: pyplot class not properly initialized.'
      return
    end if
    if (present(legend)) then
      me%show_legend = legend
    else
      me%show_legend = .true.
    end if

  end subroutine add_legend

  !> @brief add grid to the plot.
  subroutine add_grad( me, grad )
    implicit none

    class(easyplot), intent(inout) :: me    
    logical, intent(in), optional :: grad  

    if ( me%istatus /= 0 ) then
      write(error_unit,'(A)') 'Error in add_grad: pyplot class not properly initialized.'
      return
    end if
    
    if (present(grad)) then
        me%show_grad = grad
    else
      me%show_grad = .true.
    end if

  end subroutine add_grad

  !> @brief add labels to the plot.
  subroutine add_labels( me, xlabel, ylabel, zlabel )
    implicit none
    
    class(easyplot), intent(inout) :: me
    character(len=*), optional, intent(in) :: xlabel, ylabel, zlabel

    if ( me%istatus /= 0 ) then
      write(error_unit,'(A)') 'Error in add_labels: pyplot class not properly initialized.'
      return
    end if
    if ( present(xlabel) ) me%xlabels = xlabel
    if ( present(ylabel) ) me%ylabels = ylabel
    if ( present(zlabel) ) me%zlabels = zlabel 

  end subroutine add_labels

  !> @brief set the pause time.
  subroutine set_pause_time( me, pause_time )
    implicit none

    class(easyplot), intent(inout) :: me
    real(wp), intent(in), optional :: pause_time

    if ( me%istatus /= 0 ) then
      write(error_unit,'(A)') 'Error in set_pause_time: pyplot class not properly initialized.'
      return
    end if
    if (present(pause_time)) then
      me%pause_time = pause_time
    else
      me%pause_time = 0.1_wp
    end if

  end subroutine set_pause_time


!*****************************************************************************************
!> author: Jacob Williams
!  date: 8/16/2015
!
!  Write the buffer to a file, and then execute it with Python.
!
!  If user specifies a Python file name, then the file is kept, otherwise
!  a temporary filename is used, and the file is deleted after it is used.
  subroutine execute(me, pyfile, python)

    class(easyplot),   intent(inout)         :: me     !! pytplot handler
    character(len=*), intent(in),  optional :: pyfile !! name of the python script to generate
    ! integer,          intent (out),optional :: istat  !! status output (0 means no problems)
    character(len=*), intent(in),optional   :: python !! python executable to use. (by default, this is 'python')

    integer                       :: iunit   !! IO unit
    character(len=:), allocatable :: file    !! file name
    logical                       :: scratch !! if a scratch file is to be used
    integer                       :: iostat  !! open/close status code
    character(len=:), allocatable :: python_ !! python executable to use

    scratch = (.not. present(pyfile))
    file = trim(me%file_path)//'.py'  !file name to use
    if ( me%istatus /= 0 ) then
      write(error_unit,'(A)') 'Error in execute: pyplot class not properly initialized.'
      return
    end if
    
    !open the file:
    open(newunit=iunit, file=file, status='OLD', iostat=iostat)
    if (iostat /= 0) then
      write(error_unit,'(A)') 'Error opening file: '//trim(file)
      return
    end if

    !write to the file:
    ! write(iunit, '(A)') me%string

    !to ensure that the file is there for the next
    !command line call, we have to close it here.
    close(iunit, iostat=iostat)
    if (iostat /= 0) then
      write(error_unit,'(A)') 'Error closing file: '//trim(file)
    else

      if (present(python)) then
        python_ = trim(python)
      else
        python_ = python_exe
      end if

      !run the file using python:
      if (file(1:1)/='"') then
        ! if not already in quotes, should enclose in quotes
        call execute_command_line(python_//' "'//file//'"')
      else
        call execute_command_line(python_//' '//file)
      end if

      if (scratch) then
        !delete the file (have to reopen it because
        !Fortran has no file delete function)
        open(newunit=iunit, file=file, status='OLD', iostat=iostat)
        ! if (iostat==0) close(iunit, status='DELETE', iostat=iostat)
      end if
      if (iostat /= 0) then
        write(error_unit,'(A)') 'Error closing file.'
      end if

    end if

    !cleanup:
    if (allocated(file)) deallocate(file)  

  end subroutine execute
!*****************************************************************************************
    
!*****************************************************************************************
!> author: Jacob Williams
!
! Integer to string, specifying the default value if
! the optional argument is not present.

  subroutine optional_int_to_string(int_value, string_value, default_value)

    integer,          intent(in), optional :: int_value      !! integer value
    character(len=*), intent(out)          :: string_value   !! integer value stringified
    character(len=*), intent(in)           :: default_value  !! default integer value

    if (present(int_value)) then
        call integer_to_string(int_value, string_value)
    else
        string_value = default_value
    end if

  end subroutine optional_int_to_string
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
! Logical to string, specifying the default value if
! the optional argument is not present.

  subroutine optional_logical_to_string(logical_value, string_value, default_value)

    logical,intent(in),optional              :: logical_value
    character(len=:),allocatable,intent(out) :: string_value   !! integer value stringified
    character(len=*),intent(in)              :: default_value  !! default integer value

    if (present(logical_value)) then
        if (logical_value) then
            string_value = 'True'
        else
            string_value = 'False'
        end if
    else
        string_value = default_value
    end if

  end subroutine optional_logical_to_string
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
! Integer to string conversion.

  subroutine integer_to_string(i, s)

    integer,          intent(in), optional  :: i     !! integer value
    character(len=*), intent(out)           :: s     !! integer value stringified

    integer :: istat !! IO status

    write(s, int_fmt, iostat=istat) i

    if (istat/=0) then
        write(error_unit,'(A)') 'Error converting integer to string'
        s = '****'
    else
        s = adjustl(s)
    end if

  end subroutine integer_to_string
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
! Real scalar to string.

  subroutine real_to_string(v, fmt, string)

    real(wp),                      intent(in)  :: v         !! real values
    character(len=*),              intent(in)  :: fmt       !! real format string
    character(len=:), allocatable, intent(out) :: string       !! real values stringified

    integer                     :: istat     !! IO status
    character(len=max_real_len) :: tmp       !! dummy string

    if (fmt=='*') then
        write(tmp, *, iostat=istat) v
    else
        write(tmp, fmt, iostat=istat) v
    end if
    if (istat/=0) then
        write(error_unit,'(A)') 'Error in real_to_string'
        string = '****'
    else
        string = trim(adjustl(tmp))
    end if

  end subroutine real_to_string
!*****************************************************************************************

  subroutine logical_to_string( l, string )
    implicit none
    
    logical, intent(in) :: l                          !! logical value
    character(len=:), allocatable, intent(out) :: string !! logical value stringified

    if (l) then
        string = 'True'
    else
        string = 'False'
    end if

  end subroutine logical_to_string

!*****************************************************************************************
!> author: Jacob Williams
!
! Real vector to string.

  subroutine vec_to_string(v, fmt, string, use_numpy, is_tuple)

    real(wp), dimension(:),        intent(in)  :: v         !! real values
    character(len=*),              intent(in)  :: fmt       !! real format string
    character(len=:), allocatable, intent(out) :: string       !! real values stringified
    logical,                       intent(in)  :: use_numpy !! activate numpy python module usage
    logical,intent(in),optional                :: is_tuple  !! if true [default], use '()', if false use '[]'

    integer                     :: i         !! counter
    integer                     :: istat     !! IO status
    character(len=max_real_len) :: tmp       !! dummy string
    logical                     :: tuple

    if (present(is_tuple)) then
        tuple = is_tuple
    else
        tuple = .false.
    end if

    if (tuple) then
        string = '('
    else
        string = '['
    end if

    do i=1, size(v)
        if (fmt=='*') then
            write(tmp, *, iostat=istat) v(i)
        else
            write(tmp, fmt, iostat=istat) v(i)
        end if
        if (istat/=0) then
            write(error_unit,'(A)') 'Error in vec_to_string'
            string = '****'
            return
        end if
        string = string//trim(adjustl(tmp))
        if (i<size(v)) string = string // ','
    end do

    if (tuple) then
        string = string // ')'
    else
        string = string // ']'
    end if

    !convert to numpy array if necessary:
    if (use_numpy) string = 'np.array('//string//')'

  end subroutine vec_to_string
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
! Real matrix (rank 2) to string.

  subroutine matrix_to_string(v, fmt, string, use_numpy)

    real(wp), dimension(:,:),      intent(in)  :: v         !! real values
    character(len=*),              intent(in)  :: fmt       !! real format string
    character(len=:), allocatable, intent(out) :: string       !! real values stringified
    logical,                       intent(in)  :: use_numpy !! activate numpy python module usage

    integer                      :: i         !! counter
    character(len=:),allocatable :: tmp       !! dummy string

    string = '['
    do i=1, size(v,1)  !rows
        call vec_to_string(v(i,:), fmt, tmp, use_numpy)  !one row at a time
        string = string//trim(adjustl(tmp))
        if (i<size(v,1)) string = string // ','
    end do
    string = string // ']'

    !convert to numpy array if necessary:
    if (use_numpy) string = 'np.array('//string//')'

  end subroutine matrix_to_string




  
end module easyplot_module