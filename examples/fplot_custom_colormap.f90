! fplot_surf_2.f90

program example
    use fplot_core
    use iso_fortran_env
    use forcolormap, cmap => Colormap
    use forcolormap, only : colormaps_list
    implicit none

    ! Parameters
    integer(int32), parameter :: m = 50
    integer(int32), parameter :: n = 50

    ! Local Variables
    real(real64), dimension(m, n, 2), target :: xy
    real(real64), pointer, dimension(:,:) :: x, y
    real(real64), dimension(m, n) :: z
    type(surface_plot) :: plt
    type(surface_plot_data) :: d1
    class(plot_axis), pointer :: xAxis, yAxis, zAxis
    type(custom_colormap) :: map
    type(cmap) :: colors

    ! Set up the colormap
    call colors%set("glasgow", -8.0d0, 8.0d0)
    call map%set_colormap(colors)

    ! Define the data
    xy = meshgrid(linspace(-5.0d0, 5.0d0, n), linspace(-5.0d0, 5.0d0, m))
    x => xy(:,:,1)
    y => xy(:,:,2)

    ! Initialize the plot
    call plt%initialize()
    call plt%set_colormap(map)

    ! Establish lighting
    call plt%set_use_lighting(.true.)

    ! Set the orientation of the plot
    call plt%set_elevation(20.0d0)
    call plt%set_azimuth(30.0d0)
    
    ! Define titles
    call plt%set_title("Example Plot")
    
    xAxis => plt%get_x_axis()
    call xAxis%set_title("X Axis")

    yAxis => plt%get_y_axis()
    call yAxis%set_title("Y Axis")

    zAxis => plt%get_z_axis()
    call zAxis%set_title("Z Axis")

    ! Define the function to plot
    z = sqrt(x**2 + y**2) * sin(x**2 + y**2)
    call d1%define_data(x, y, z)
    call plt%push(d1)

    ! Draw the plot
    call plt%draw()
end program