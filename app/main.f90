program main
  use easyplot_module
  implicit none

  integer,parameter :: n = 100
  real(wp), dimension(:),allocatable :: x     !! x values
  real(wp), dimension(:),allocatable :: y     !! y values
  real(wp), dimension(:),allocatable :: sx    !! sin(x) values
  real(wp), dimension(:),allocatable :: cx    !! cos(x) values
  real(wp), dimension(:),allocatable :: tx    !! sin(x)*cos(x) values
  type(easyplot) :: eplt   !! pytplot handler
  integer        :: i      !! counter

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
  call eplt%initialize()
  ! if pause is not called, the plot will not be shown until the function 'show()' is called
  ! if we want to save the figure in the end, the pause function is not necessary or set to 0
  call eplt%pause(0.d0)   
  ! if legend is not called, the legend will not be shown
  call eplt%legend(.false.)
  ! if grad is not called, the grid will not be shown
  call eplt%grad()
  ! if hold is not called, the plot will be cleared after each plot
  call eplt%hold()
  ! if the status of hold is not set, the labels must be set before the plot
  call eplt%labels(xlabel='xxxx', ylabel='y~~~')
  call eplt%plot(x,sx,label='$\sin (x)$',title='$\sin (x)$',linestyle='b-o',markersize=5,linewidth=2)
  ! call eplt%show()

  call eplt%labels(xlabel='XXXX', ylabel='YYYY')
  call eplt%plot(x,cx,label='$\cos (x)$',title='$\cos (x)$',linestyle='r-o',markersize=5,linewidth=2)
  ! call eplt%show()

  call eplt%labels(xlabel='', ylabel='')
  call eplt%plot(x,tx,label='$\sin (x) \cos (x)$',title='$\sin (x) \cos (x)$',linestyle='g-o',markersize=2,linewidth=1)
  ! call eplt%show()
  call eplt%save('test.png')
  
end program main
