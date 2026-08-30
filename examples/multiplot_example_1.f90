program example
    use iso_fortran_env
    use fplot_core
    implicit none

    ! Variables
    integer(int32), parameter :: n = 1000
    real(real64), allocatable, dimension(:) :: x1, y1, x2, y2
    type(multiplot) :: mplt
    type(plot_2d) :: plt1, plt2

    ! Build the data sets
    x1 = linspace(0.0d0, 5.0d0, n)
    x2 = linspace(0.0d0, 10.0d0, n)
    y1 = exp(-0.1d0 * x1) * sin(20.0d0 * x1)
    y2 = exp(-0.2d0 * x2) * sin(15.0d0 * x2) + 0.1d0 * sin(75.0d0 * x2)

    ! Define the plots
    call mplt%initialize(2, 1)
    call plt1%initialize()
    call plt2%initialize()

    call plt1%set_x_axis_title("X1")
    call plt1%set_y_axis_title("Y1")

    call plt2%set_x_axis_title("X2")
    call plt2%set_y_axis_title("Y2")
    
    call plt1%push(x1, y1, name = "Data Set 1")
    call plt2%push(x2, y2, name = "Data Set 2")

    call mplt%set(1, 1, plt1)
    call mplt%set(2, 1, plt2)
    call mplt%draw()
end program