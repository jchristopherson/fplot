program example
    use fplot_core
    use iso_fortran_env
    use forcolormap
    implicit none

    ! Parameters
    integer(int32), parameter :: m = 50
    integer(int32), parameter :: n = 50

    ! Local Variables
    real(real64), dimension(m, n, 2), target :: xy
    real(real64), pointer, dimension(:,:) :: x, y
    real(real64), dimension(m, n) :: z
    type(surface_plot) :: plt
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
    call plt%set_x_axis_title("X Axis")
    call plt%set_y_axis_title("Y Axis")
    call plt%set_z_axis_title("Z Axis")
    call plt%set_title("Custom Colormap")

    ! Establish lighting
    call plt%set_use_lighting(.true.)

    ! Set the orientation of the plot
    call plt%set_elevation(40.0d0)
    call plt%set_azimuth(20.0d0)

    ! Define the function to plot
    z = sqrt(x**2 + y**2) * sin(x**2 + y**2)
    call plt%push(x, y, z)

    ! Draw the plot
    call plt%draw()
end program