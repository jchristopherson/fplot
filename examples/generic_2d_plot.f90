program example
    use, intrinsic :: iso_fortran_env
    use fplot_core
    implicit none

    ! Parameters
    integer(int32), parameter :: n = 1000

    ! Local Variables
    real(real64), dimension(n) :: x, y1, y2
    type(plot_2d) :: plt
    
    ! Initialize the plot object
    call plt%initialize()
    call plt%show_legend(.true.)
    call plt%set_title("Example Plot")
    call plt%set_x_axis_title("X Axis")
    call plt%set_y_axis_title("Y Axis")

    ! Define the data, and then add it to the plot
    x = linspace(0.0d0, 10.0d0, n)
    y1 = sin(5.0d0 * x)
    y2 = 2.0d0 * cos(2.0d0 * x)

    call plt%push(x, y1, name = "Data Set 1")
    call plt%push(x, y2, ls = LINE_DASHED, name = "Data Set 2")
    
    ! Let GNUPLOT draw the plot
    call plt%draw()
end program