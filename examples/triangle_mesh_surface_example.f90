program example
    use iso_fortran_env
    use fplot_core
    implicit none

    ! Variables
    integer(int32), parameter :: npts = 15
    real(real64), allocatable :: xc(:,:), yc(:,:), x(:), y(:), z(:), xy(:,:,:)
    real(real64) :: xi, yi, zi
    type(delaunay_tri_surface) :: tri
    type(tri_surface_plot_data) :: ds
    type(plot_data_3d) :: di
    type(surface_plot) :: plt
    integer(int32) :: i

    ! Initialization
    xy = meshgrid(linspace(-5.0d0, 5.0d0, npts), linspace(-5.0d0, 5.0d0, npts))
    xc = xy(:,:,1)
    yc = xy(:,:,2)
    x = reshape(xc, [npts * npts])
    y = reshape(yc, [npts * npts])
    z = sin(x) + sin(y)

    ! Generate the triangulation
    call tri%create(x, y)
    call tri%define_function_values(z)

    ! Interpolate using the triangulation
    xi = 2.0d0
    yi = 2.0d0
    zi = tri%evaluate(xi, yi)

    ! Print the interpolated values
    print '(A)', "Interpolated Value:"
    print '(A, F0.3, A, F0.3, A, F0.3)', achar(9), xi, achar(9), yi, achar(9), zi
    
    print '(A)', "Actual Values:"
    print '(A, F0.3, A, F0.3, A, F0.3)', achar(9), xi, achar(9), yi, achar(9), &
        sin(xi) + sin(yi)

    ! Generate the plot
    call plt%initialize()
    call plt%set_x_axis_title("x")
    call plt%set_y_axis_title("y")
    call plt%set_z_axis_title("f(x,y)")

    call ds%define_data(tri)
    call ds%set_use_wireframe(.true.)

    call di%define_data([xi], [yi], [zi])
    call di%set_draw_line(.false.)
    call di%set_line_width(2.0)
    call di%set_draw_markers(.true.)
    call di%set_marker_style(MARKER_FILLED_CIRCLE)
    call di%set_marker_scaling(3.0)
    call di%set_line_color(CLR_RED)

    call plt%push(ds)
    call plt%push(di)
    call plt%draw()
end program