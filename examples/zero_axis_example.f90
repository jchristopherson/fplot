program example
    use, intrinsic :: iso_fortran_env
    use fplot_core
    implicit none

    ! Parameters
    integer(int32), parameter :: n = 1000

    ! Local Variables
    real(real64), dimension(n) :: x, y1, y2
    type(plot_2d) :: plt
    class(plot_axis), pointer :: xAxis, yAxis
    type(legend), pointer :: lgnd
    
    ! Initialize the plot object
    call plt%initialize()

    ! Define the legend location
    lgnd => plt%get_legend()
    call lgnd%set_is_visible(.true.)
    call lgnd%set_draw_inside_axes(.false.)

    ! Define titles
    call plt%set_title("2D Example Plot 2")

    xAxis => plt%get_x_axis()
    call xAxis%set_title("X Axis")
    call xAxis%set_zero_axis(.true.)

    yAxis => plt%get_y_axis()
    call yAxis%set_title("Y Axis")

    ! Define the data, and then add it to the plot
    x = linspace(0.0d0, 10.d0, n)
    y1 = sin(5.0d0 * x)
    y2 = 2.0d0 * cos(2.0d0 * x)

    call plt%push(x, y1, lw = 1.5, name = "Data Set 1")
    call plt%push(x, y2, lw = 2.5, name = "Data Set 2")

    ! Let GNUPLOT draw the plot
    call plt%draw()
end program