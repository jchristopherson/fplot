! fplot_tri_surface_plot_data.f90

module fplot_tri_surface_plot_data
    use iso_fortran_env
    use fplot_plot_data
    use fplot_delaunay_tri_surface
    use strings
    implicit none
    private
    public :: tri_surface_plot_data

    type, extends(plot_data) :: tri_surface_plot_data
        !! Provides a three-dimensional surface plot data set constructed of
        !! triangulated points.
        real(real64), private, allocatable, dimension(:) :: m_x
            !! An array of the x-coordinates of each point.
        real(real64), private, allocatable, dimension(:) :: m_y
            !! An array of the y-coordinates of each point.
        real(real64), private, allocatable, dimension(:) :: m_z
            !! An array of the z-coordinates of each point.
        integer(int32), private, allocatable, dimension(:,:) :: m_indices
            !! A 3-column matrix containing the indices of each triangle's
            !! vertex.
        logical, private :: m_wireframe = .true.
            !! Determines if the surface should be drawn as a wireframe.
    contains
        procedure, public :: get_data_string => tspd_get_data_cmd
        procedure, public :: get_command_string => tspd_get_cmd
        procedure, public :: get_use_wireframe => tspd_get_wireframe
        procedure, public :: set_use_wireframe => tspd_set_wireframe
        procedure, public :: define_data => tspd_define_data
    end type

contains
! ------------------------------------------------------------------------------
    function tspd_get_data_cmd(this) result(x)
        !! Gets the GNUPLOT command string for representing the data.
        class(tri_surface_plot_data), intent(in) :: this
            !! The tri_surface_plot_data object.
        character(len = :), allocatable :: x
            !! The command string.

        ! Local Variables
        type(string_builder) :: str
        integer(int32) :: i, j, n
        character :: delimiter, nl

        ! Initialization
        call str%initialize()
        n = size(this%m_indices, 1)
        delimiter = achar(9)
        nl = new_line(nl)

        ! Process
        ! https://stackoverflow.com/questions/42784369/drawing-triangular-mesh-using-gnuplot
        ! http://www.gnuplot.info/faq/faq.html#x1-530005.10
        do i = 1, n
            ! Line 1-2
            ! Vertex 1
            j = this%m_indices(i, 1)
            call str%append(to_string(this%m_x(j)))
            call str%append(delimiter)
            call str%append(to_string(this%m_y(j)))
            call str%append(delimiter)
            call str%append(to_string(this%m_z(j)))
            call str%append(nl)

            ! Vertex 2
            j = this%m_indices(i, 2)
            call str%append(to_string(this%m_x(j)))
            call str%append(delimiter)
            call str%append(to_string(this%m_y(j)))
            call str%append(delimiter)
            call str%append(to_string(this%m_z(j)))
            call str%append(nl)

            ! Line 2-3
            ! Vertex 2
            call str%append(nl)
            j = this%m_indices(i, 2)
            call str%append(to_string(this%m_x(j)))
            call str%append(delimiter)
            call str%append(to_string(this%m_y(j)))
            call str%append(delimiter)
            call str%append(to_string(this%m_z(j)))
            call str%append(nl)


            ! Vertex 3
            j = this%m_indices(i, 3)
            call str%append(to_string(this%m_x(j)))
            call str%append(delimiter)
            call str%append(to_string(this%m_y(j)))
            call str%append(delimiter)
            call str%append(to_string(this%m_z(j)))
            call str%append(nl)

            ! Line 3-1
            ! Vertex 3
            call str%append(nl)
            j = this%m_indices(i, 3)
            call str%append(to_string(this%m_x(j)))
            call str%append(delimiter)
            call str%append(to_string(this%m_y(j)))
            call str%append(delimiter)
            call str%append(to_string(this%m_z(j)))
            call str%append(nl)

            ! Vertex 1
            j = this%m_indices(i, 1)
            call str%append(to_string(this%m_x(j)))
            call str%append(delimiter)
            call str%append(to_string(this%m_y(j)))
            call str%append(delimiter)
            call str%append(to_string(this%m_z(j)))
            call str%append(nl)

            ! Add in the two blank lines
            if (i /= n) then
                call str%append(nl)
                call str%append(nl)
            end if
        end do

        ! End
        x = char(str%to_string())
    end function

! ------------------------------------------------------------------------------
    function tspd_get_cmd(this) result(x)
        !! Gets the GNUPLOT command string for the object.
        class(tri_surface_plot_data), intent(in) :: this
            !! The tri_surface_plot_data object.
        character(len = :), allocatable :: x
            !! The command string.

        ! Local Variables
        type(string_builder) :: str
        integer(int32) :: n

        ! Initialization
        call str%initialize()

        ! Title
        n = len_trim(this%get_name())
        if (n > 0) then
            call str%append(' "-" title "')
            call str%append(this%get_name())
            call str%append('"')
        else
            call str%append(' "-" notitle')
        end if

        ! PM3D or wireframe?
        if (this%get_use_wireframe()) then
            call str%append(" with lines")
        else
            call str%append(" with pm3d")
        end if

        ! End
        x = char(str%to_string())
    end function

! ------------------------------------------------------------------------------
    pure function tspd_get_wireframe(this) result(rst)
        !! Gets a value determining if a wireframe mesh should be displayed.
        class(tri_surface_plot_data), intent(in) :: this
            !! The tri_surface_plot_data object.
        logical :: rst
            !! Returns true if the plot is to be drawn as a wireframe; else,
            !! false to draw as a surface.
        rst = this%m_wireframe
    end function

! ------------------------------------------------------------------------------
    subroutine tspd_set_wireframe(this, x)
        !! Sets a value determining if a wireframe mesh should be displayed.
        class(tri_surface_plot_data), intent(inout) :: this
            !! The tri_surface_plot_data object.
        logical, intent(in) :: x
            !! Set to true if the plot is to be drawn as a wireframe; else,
            !! false to draw as a surface.
        this%m_wireframe = x
    end subroutine

! ------------------------------------------------------------------------------
    subroutine tspd_define_data(this, tri)
        !! Defines the data to plot.
        class(tri_surface_plot_data), intent(inout) :: this
            !! The tri_surface_plot_data object.
        class(delaunay_tri_surface), intent(in) :: tri
            !! The triangulation to plot.

        ! Process
        if (allocated(this%m_x)) deallocate(this%m_x)
        if (allocated(this%m_y)) deallocate(this%m_y)
        if (allocated(this%m_z)) deallocate(this%m_z)
        if (allocated(this%m_indices)) deallocate(this%m_indices)

        this%m_x = tri%get_points_x()
        this%m_y = tri%get_points_y()
        this%m_z = tri%get_points_z()
        this%m_indices = tri%get_indices()
    end subroutine

! ------------------------------------------------------------------------------
end module
