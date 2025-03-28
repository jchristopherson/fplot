program example
    use iso_fortran_env
    use fplot_core
    implicit none

    ! Variables
    integer(int32), parameter :: n = 1000
    real(real64), allocatable, dimension(:) :: x1, y1, x2, y2
    type(multiplot) :: mplt
    type(plot_2d) :: plt1, plt2
    type(plot_data_2d) :: d1, d2
    class(plot_axis), pointer :: x1Axis, x2Axis, y1Axis, y2Axis

    ! Build the data sets
    x1 = linspace(0.0d0, 5.0d0, n)
    x2 = linspace(0.0d0, 10.0d0, n)
    y1 = exp(-0.1d0 * x1) * sin(20.0d0 * x1)
    y2 = exp(-0.2d0 * x2) * sin(15.0d0 * x2) + 0.1d0 * sin(75.0d0 * x2)

    ! Define the plots
    call mplt%initialize(2, 1)
    call mplt%set_font_size(14)
    call plt1%initialize()
    call plt2%initialize()

    x1Axis => plt1%get_x_axis()
    y1Axis => plt1%get_y_axis()

    x2Axis => plt2%get_x_axis()
    y2Axis => plt2%get_y_axis()

    call x1Axis%set_title("X1")
    call y1Axis%set_title("Y1")

    call x2Axis%set_title("X2")
    call y2Axis%set_title("Y2")

    call d1%set_name("Data Set 1")
    call d1%set_line_width(2.0)
    call d1%define_data(x1, y1)

    call d2%set_name("Data Set 2")
    call d2%set_line_width(2.0)
    call d2%define_data(x2, y2)

    call plt1%push(d1)
    call plt2%push(d2)

    call mplt%set(1, 1, plt1)
    call mplt%set(2, 1, plt2)
    call mplt%draw()
end program