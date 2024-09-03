# EasyPlot

### Overview

This module is a Fortran-based plotting library designed to provide functionality similar to MATLAB's `plot` function. Inspired by Jacob Williams' open-source project [pyplot-fortran](https://github.com/jacobwilliams/pyplot-fortran.git "pyplot-fortran") this module aims to offer a simple and efficient way to create plots directly from Fortran programs. A primary focus has been placed on enabling dynamic observation of plot changes, which was not fully supported in the original `pyplot-fortran`.

### Current Status

Please note that this module is still under development and testing. As of now, it supports basic plotting functionalities. Future updates will include additional features and improvements.

### Compiling

The module requires a modern Fortran compiler (it uses various Fortran 2003/2008 features such as deferred-length strings). It should work fine with the latest gfortran or ifort compilers.

A `fpm.toml` file is provided for compiling pyplot-fortran with the [Fortran Package Manager](https://github.com/fortran-lang/fpm). For example, to build:

```fortran
fpm build --profile release
```

By default, the library is built with double precision (`real64`) real values. To use  `EasyPlot` within your fpm project, add the following to your `fpm.toml` file:

```fortran
[dependencies]
pyplot-fortran = { git="https://github.com/hczhang96/EasyPlot" }
```

### Example

The following example generates a plot of the sine function:

```fortran
program check_init1
  use mpyplot_module
  implicit none

  integer,parameter :: n = 100
  real(wp), dimension(:),allocatable   :: x     !! x values
  real(wp), dimension(:),allocatable   :: y     !! y values
  real(wp), dimension(:),allocatable   :: yerr  !! error values for bar chart
  real(wp), dimension(:),allocatable   :: sx    !! sin(x) values
  real(wp), dimension(:),allocatable   :: cx    !! cos(x) values
  real(wp), dimension(:),allocatable   :: tx    !! sin(x)*cos(x) values
  type(mpyplot)            :: mplt   !! pytplot handler
  integer                  :: i     !! counter

  real(wp),parameter :: pi = acos(-1.0_wp)
  real(wp),parameter :: deg2rad = pi/180.0_wp

  character(len=*), parameter :: testdir = "test/"

  ! size arrays:
  allocate(x(n))
  allocate(y(n))
  allocate(yerr(n))
  allocate(sx(n))
  
  !generate some data:
  x    = [(real(i,wp), i=0,size(x)-1)]/5.0_wp
  sx   = sin(x)
  cx   = cos(x)
  tx   = sx * cx
  
  ! initialize the plot, if not present the file name (figure name), the default is 'figure_date_time.png'
  call mplt%initialize()
  ! if pause is not called, the plot will not be shown until the function 'show()' is called
  ! if we want to save the figure in the end, the pause function is not necessary or set to 0
  call mplt%pause(0.d0)   
  ! if legend is not called, the legend will not be shown
  call mplt%legend(.false.)
  ! if grad is not called, the grid will not be shown
  call mplt%grad()
  ! if hold is not called, the plot will be cleared after each plot
  call mplt%hold()
  ! if the status of hold is not set, the labels must be set before the plot
  call mplt%labels(xlabel='xxxx', ylabel='y~~~')
  call mplt%plot(x,sx,label='$\sin (x)$',title='$\sin (x)$',linestyle='b-o',markersize=5,linewidth=2)
  ! call mplt%show()

  call mplt%labels(xlabel='XXXX', ylabel='YYYY')
  call mplt%plot(x,cx,label='$\cos (x)$',title='$\cos (x)$',linestyle='r-o',markersize=5,linewidth=2)
  ! call mplt%show()

  call mplt%labels(xlabel='', ylabel='')
  call mplt%plot(x,tx,label='$\sin (x) \cos (x)$',title='$\sin (x) \cos (x)$',linestyle='g-o',markersize=2,linewidth=1)
  ! call mplt%show()
  call mplt%save('test.png')
  
end program check_init1

```

### Contributing

Contributions to the module are welcome. If you have ideas, bug reports, or would like to contribute code, please submit a pull request or create an issue on the project's repository.

### Acknowledgements

* **Jacob Williams:** For the inspiration provided by the `pyplot-fortran` project.
* **ZUO Zhihua:** For the invaluable assistance and guidance in programming throughout this project.

### See also

* [Matplotlib](https://matplotlib.org/)
* [pyplot-fortran](https://github.com/jacobwilliams/pyplot-fortran.git "pyplot-fortran")
