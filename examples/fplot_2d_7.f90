! fplot_2d_7.f90

program example
    use fplot_core
    use iso_fortran_env
    implicit none

    ! Local Variables
    integer(int32), parameter :: npts = 1000
    real(real64), dimension(npts) :: x, y
    type(plot_2d) :: plt
    type(plot_data_2d) :: dataset
    class(plot_axis), pointer :: xAxis, yAxis
    type(legend), pointer :: leg
    type(plot_label) :: lbl

    ! Build a data set
    x = linspace(0.0d0, 10.0d0, npts)
    y = sin(10.0d0 * x) * sin(0.5d0 * x)

    call dataset%define_data(y)

    ! Define the label
    call lbl%set_text("Test Label 1")
    call lbl%set_position([600.0, 0.6, 0.0])

    ! Set up the plot
    call plt%initialize()
    call plt%set_title("Example Plot")
    call plt%push_label(lbl)

    xAxis => plt%get_x_axis()
    call xAxis%set_title("X Axis")

    yAxis => plt%get_y_axis()
    call yAxis%set_title("Y Axis")

    ! Hide the legend
    leg => plt%get_legend()
    call leg%set_is_visible(.false.)

    ! Add the data to the plot
    call plt%push(dataset)

    ! Draw
    call plt%draw()
end program