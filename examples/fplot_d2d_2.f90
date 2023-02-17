! fplot_d2d_2.f90

program example
    use fplot_core
    use iso_fortran_env
    implicit none

    ! Parameters
    integer(int32), parameter :: npts = 1000
    real(real64), parameter :: pi = 2.0d0 * acos(0.0d0)
    real(real64), parameter :: xpt = 0.75d0
    real(real64), parameter :: ypt = 0.75d0

    ! Local Variables
    type(delaunay_tri_2d) :: tri
    real(real64) :: x(npts), y(npts), theta(npts), radius(npts), &
        xtri(3), ytri(3)
    integer(int32) :: ind, n1, n2, n3
    integer(int32), allocatable, dimension(:,:) :: indices
    type(plot_2d) :: plt
    type(plot_data_tri_2d) :: ds
    type(plot_data_2d) :: dtri, dpt

    ! Initialization
    call random_number(theta)
    theta = 2.0d0 * pi * theta

    call random_number(radius)
    radius = radius + 0.5d0

    x = radius * cos(theta)
    y = radius * sin(theta)

    ! Create a 2D triangulation from the data
    call tri%create(x, y)

    ! Find the index of the triangle containing (xpt, ypt)
    ind = tri%find_triangle(xpt, ypt)
    if (ind == -1) then
        print '(A)', "No triangle was found that included the specified point."
    end if

    ! Get the vertices of this triangle
    indices = tri%get_indices()
    n1 = indices(ind, 1)
    n2 = indices(ind, 2)
    n3 = indices(ind, 3)
    xtri = [x(n1), x(n2), x(n3)]
    ytri = [y(n1), y(n2), y(n3)]

    ! Plot the triangulation, the point of interest, and highlight the triangle
    call plt%initialize()
    call plt%set_font_size(14)

    call ds%define_data(tri)
    call plt%push(ds)

    call dtri%define_data(xtri, ytri)
    call dtri%set_draw_line(.false.)
    call dtri%set_draw_markers(.true.)
    call dtri%set_marker_style(MARKER_FILLED_CIRCLE)
    call dtri%set_marker_scaling(1.2)
    call dtri%set_line_color(CLR_LIME)
    call plt%push(dtri)

    call dpt%define_data([xpt], [ypt])
    call dpt%set_draw_line(.false.)
    call dpt%set_draw_markers(.true.)
    call dpt%set_marker_style(MARKER_X)
    call dpt%set_marker_scaling(3.0)
    call dpt%set_line_width(2.0)
    call plt%push(dpt)

    call plt%draw()
end program
