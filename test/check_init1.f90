program check_init1  
  use easyplot_module
  implicit none

  integer,parameter :: n = 100
  real(wp), dimension(:),allocatable :: x     !! x values
  real(wp), dimension(:),allocatable :: y     !! y values
  real(wp), dimension(:),allocatable :: sx    !! sin(x) values
  real(wp), dimension(:),allocatable :: cx    !! cos(x) values
  real(wp), dimension(:),allocatable :: tx    !! sin(x)*cos(x) values
  type(mpyplot) :: mplt   !! pytplot handler
  integer       :: i      !! counter

  ! size arrays:
  allocate(x(n))
  allocate(y(n))
  allocate(sx(n))
  allocate(cx(n))
  allocate(tx(n))
  
  !generate some data:
  x  = [(real(i,wp), i=0,size(x)-1)]/5.0_wp
  sx = sin(x)
  cx = cos(x)
  tx = sx * cx
  
  ! initialize the plot, if not present the file name (figure name), the default is 'figure_date_time.png'
  ! the parameters can be set in the initialize function
  call mplt%initialize(grid=.true., legend=.false., hold=.false.)
  ! if pause is not called, the plot will not be shown until the function 'show()' is called
  ! if we want to save the figure in the end, the pause function is not necessary or set to 0
  ! if we want to observate the process of the plot, the pause function can be set to a positive value, e.g. 1.d0
  call mplt%pause(1.d0)
  ! if the status of hold is not set, the labels must be set before the plot
  call mplt%labels(xlabel='xxxx', ylabel='y~~~')
  call mplt%plot(x,sx,label='$\sin (x)$',title='$\sin (x)$',linestyle='b-o',markersize=5,linewidth=2)
  ! if the function show() is called after each plot, the program will not continue until the figure is closed
  ! call mplt%show()

  call mplt%labels(xlabel='XXXX', ylabel='YYYY')
  call mplt%plot(x,cx,label='$\cos (x)$',title='$\cos (x)$',linestyle='r-o',markersize=5,linewidth=2)
  ! call mplt%show()

  call mplt%labels(xlabel='', ylabel='')
  call mplt%plot(x,tx,label='$\sin (x) \cos (x)$',title='$\sin (x) \cos (x)$',linestyle='g-o',markersize=2,linewidth=1)
  call mplt%show()
  ! call mplt%save('test.png')
        
end program check_init1
