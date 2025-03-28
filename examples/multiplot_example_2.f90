! fplot_multi_2.f90

program example
    use iso_fortran_env
    use fplot_core 
    implicit none

    ! Variables
    integer(int32), parameter :: n = 1000
    real(real64), allocatable, dimension(:) :: x1, y1, x2, y2, x3, y3, x4, y4
    type(multiplot) :: mplt
    type(plot_2d) :: plt1, plt2, plt3, plt4
    type(plot_data_2d) :: d1, d2, d3, d4
    
    ! Build the data sets
    x1 = linspace(0.0d0, 1.0d0, n)
    x2 = linspace(0.0d0, 2.0d0, n)
    x3 = linspace(0.0d0, 3.0d0, n)
    x4 = linspace(0.0d0, 4.0d0, n)
    y1 = sin(20.0d0 * x1)
    y2 = sin(20.0d0 * x2) * cos(50.0d0 * x2)
    y3 = sqrt(x3) * sin(10.0d0 * x3)
    y4 = exp(-0.1d0 * x4) * sin(15.0d0 * x4)

    ! Define the plots
    call mplt%initialize(2, 2)
    call plt1%initialize()
    call plt2%initialize()
    call plt3%initialize()
    call plt4%initialize()

    call d1%define_data(x1, y1)
    call d2%define_data(x2, y2)
    call d3%define_data(x3, y3)
    call d4%define_data(x4, y4)

    call plt1%push(d1)
    call plt1%set_title("Plot 1 (1, 1)")

    call plt2%push(d2)
    call plt2%set_title("Plot 2 (2, 1)")

    call plt3%push(d3)
    call plt3%set_title("Plot 3 (1, 2)")

    call plt4%push(d4)
    call plt4%set_title("Plot 4 (2, 2)")

    call mplt%set(1, 1, plt1)
    call mplt%set(2, 1, plt2)
    call mplt%set(1, 2, plt3)
    call mplt%set(2, 2, plt4)
    call mplt%draw()
end program