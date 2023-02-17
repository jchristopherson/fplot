! fplot_d2d_1.f90

program example
    use iso_fortran_env
    use fplot_core
    implicit none

    ! Parameters
    integer(int32), parameter :: npts = 1000
    real(real64), parameter :: pi = 2.0d0 * acos(0.0d0)

    ! Local Variables
    type(delaunay_tri_2d) :: tri
    real(real64) :: x(npts), y(npts), theta(npts), radius(npts)
    type(plot_2d) :: plt
    type(plot_data_tri_2d) :: ds

    ! Initialization
    call random_number(theta)
    theta = 2.0d0 * pi * theta

    call random_number(radius)
    radius = radius + 0.5d0

    x = radius * cos(theta)
    y = radius * sin(theta)

    ! Create a 2D triangulation from the data
    call tri%create(x, y)

    ! Display the number of points and elements
    print '(AI0AI0A)', "The triangulation consists of ", &
        tri%get_point_count(), " points, and ", tri%get_triangle_count(), &
        " triangles."

    ! Plot the triangulation
    call plt%initialize()
    call plt%set_font_size(14)

    call ds%define_data(tri)
    call plt%push(ds)

    call plt%draw()
end program
