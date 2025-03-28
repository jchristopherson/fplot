program example
    use iso_fortran_env
    use fplot_core
    implicit none

    ! Local Variables
    type(plot_2d) :: plt
    type(vector_field_plot_data) :: ds1
    class(plot_axis), pointer :: xAxis, yAxis
    type(rainbow_colormap) :: map
    real(real64), allocatable, dimension(:,:,:) :: pts
    real(real64), allocatable, dimension(:,:) :: dx, dy
    real(real64) :: dxdt(2)
    integer(int32) :: i, j

    ! Create a grid of points defining the vector locations
    pts = meshgrid( &
        linspace(-2.0d0, 2.0d0, 20), &
        linspace(-5.0d0, 5.0d0, 20))

    ! Compute the values of each derivative
    allocate(dx(size(pts, 1), size(pts, 2)))
    allocate(dy(size(pts, 1), size(pts, 2)))
    do j = 1, size(pts, 2)
        do i = 1, size(pts, 1)
            call eqn([pts(i,j,1), pts(i,j,2)], dxdt)
            dx(i,j) = dxdt(1)
            dy(i,j) = dxdt(2)
        end do
    end do

    ! Define arrow properties
    call ds1%set_arrow_size(0.1d0)  ! 1.0 by default
    call ds1%set_fill_arrow(.true.) ! .false. by default

    ! Create the plot
    call plt%initialize()
    call plt%set_font_size(14)
    xAxis => plt%get_x_axis()
    yAxis => plt%get_y_axis()

    ! Define axis labels
    call xAxis%set_title("x(t)")
    call yAxis%set_title("dx/dt")

    ! Set plot style information
    call xAxis%set_zero_axis(.true.)
    call yAxis%set_zero_axis(.true.)
    call plt%set_draw_border(.false.)
    call plt%set_show_gridlines(.false.)

    ! Define the colormap
    call plt%set_colormap(map)

    ! Add the data to the plot - color by the magnitude of gradient
    call ds1%define_data(pts(:,:,1), pts(:,:,2), dx, dy, sqrt(dx**2 + dy**2))
    call plt%push(ds1)

    call plt%draw()
contains
    ! Van der Pol Equation
    ! x" - mu * (1 - x^2) * x' + x = 0
    subroutine eqn(x, dxdt)
        real(real64), intent(in) :: x(2)
        real(real64), intent(out) :: dxdt(2)

        real(real64), parameter :: mu = 2.0d0

        dxdt(1) = x(2)
        dxdt(2) = mu * (1.0d0 - x(1)**2) * x(2) - x(1)
    end subroutine
end program
