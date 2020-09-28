! fplot_tri_surf_1.f90

program example
    use iso_fortran_env
    use fplot_core
    implicit none

    ! Variables
    integer(int32), parameter :: npts = 10
    real(real64), allocatable :: xc(:,:), yc(:,:), x(:), y(:), z(:), xy(:,:,:)
    type(delaunay_tri_surface) :: tri
    type(tri_surface_plot_data) :: ds
    type(surface_plot) :: plt

    ! Initialization
    xy = meshgrid(linspace(0.0d0, 5.0d0, npts), linspace(0.0d0, 5.0d0, npts))
    xc = xy(:,:,1)
    yc = xy(:,:,2)
    x = reshape(xc, [npts * npts])
    y = reshape(yc, [npts * npts])
    z = sin(x) + sin(y)

    ! Generate the triangulation
    call tri%create(x, y)
    call tri%define_function_values(z)

    ! Generate the plot
    call plt%initialize()
    call ds%define_data(tri)
    ! call ds%set_use_wireframe(.true.)
    call plt%push(ds)
    call plt%draw()
end program