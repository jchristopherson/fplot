! fplot_plot_3d.f90

module fplot_plot_3d
    use iso_fortran_env
    use fplot_plot
    use fplot_errors
    use fplot_plot_axis
    use fplot_constants
    use fplot_plot_data
    use fplot_legend
    use ferror
    use strings
    implicit none
    private
    public :: plot_3d

    type, extends(plot) :: plot_3d
        !! A plot object defining a 3D plot.
        type(x_axis), private, pointer :: m_xAxis => null()
            !! The x-axis.
        type(y_axis), private, pointer :: m_yAxis => null()
            !! The y-axis.
        type(z_axis), private, pointer :: m_zAxis => null()
            !! The z-axis.
        real(real64), private :: m_elevation = 60.0d0
            !! The elevation angle.
        real(real64), private :: m_azimuth = 30.0d0
            !! The azimuth.
        logical, private :: m_zIntersect = .true.
            !! Z-axis intersect X-Y plane?
        logical, private :: m_setMap = .false.
            !! Set map projection.
        integer(int32), private :: m_csys = COORDINATES_CARTESIAN
            !! Plot coordinate system.
    contains
        final :: p3d_clean_up
        procedure, public :: initialize => p3d_init
        procedure, public :: get_command_string => p3d_get_cmd
        procedure, public :: get_x_axis => p3d_get_x_axis
        procedure, public :: get_y_axis => p3d_get_y_axis
        procedure, public :: get_z_axis => p3d_get_z_axis
        procedure, public :: get_elevation => p3d_get_elevation
        procedure, public :: set_elevation => p3d_set_elevation
        procedure, public :: get_azimuth => p3d_get_azimuth
        procedure, public :: set_azimuth => p3d_set_azimuth
        procedure, public :: get_z_intersect_xy => p3d_get_z_axis_intersect
        procedure, public :: set_z_intersect_xy => p3d_set_z_axis_intersect
        procedure, public :: get_use_map_view => p3d_get_use_map_view
        procedure, public :: set_use_map_view => p3d_set_use_map_view
        procedure, public :: get_coordinate_system => p3d_get_csys
        procedure, public :: set_coordinate_system => p3d_set_csys
    end type

contains
! ------------------------------------------------------------------------------
    subroutine p3d_clean_up(this)
        !! Cleans up resources held by the plot_3d object.
        type(plot_3d), intent(inout) :: this
            !! The plot_3d object.
        call this%free_resources()
        if (associated(this%m_xAxis)) then
            deallocate(this%m_xAxis)
            nullify(this%m_xAxis)
        end if
        if (associated(this%m_yAxis)) then
            deallocate(this%m_yAxis)
            nullify(this%m_yAxis)
        end if
        if (associated(this%m_zAxis)) then
            deallocate(this%m_zAxis)
            nullify(this%m_zAxis)
        end if
    end subroutine

! ------------------------------------------------------------------------------
    subroutine p3d_init(this, term, fname, err)
        !! Initializes the plot_3d object.
        class(plot_3d), intent(inout) :: this
            !! The plot_3d object.
        integer(int32), intent(in), optional :: term
            !! An optional input that is used to define the terminal.
            !!  The default terminal is a WXT terminal.  The acceptable inputs 
            !! are:
            !!
            !!  - GNUPLOT_TERMINAL_PNG
            !!
            !!  - GNUPLOT_TERMINAL_QT
            !!
            !!  - GNUPLOT_TERMINAL_WIN32
            !!
            !!  - GNUPLOT_TERMINAL_WXT
            !!
            !!  - GNUPLOT_TERMINAL_LATEX
        character(len = *), intent(in), optional :: fname
            !! A filename to pass to the terminal in the event the
            !! terminal is a file type (e.g. GNUPLOT_TERMINAL_PNG).
        class(errors), intent(inout), optional, target :: err
            !! An error handling object.

        ! Local Variables
        integer(int32) :: flag
        class(errors), pointer :: errmgr
        type(errors), target :: deferr

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Initialize the base class
        ! call plt_init(this, term, fname, errmgr)
        call this%plot%initialize(term, fname, errmgr)
        if (errmgr%has_error_occurred()) return

        ! Process
        flag = 0
        if (.not.associated(this%m_xAxis)) then
            allocate(this%m_xAxis, stat = flag)
        end if
        if (flag == 0 .and. .not.associated(this%m_yAxis)) then
            allocate(this%m_yAxis, stat = flag)
        end if
        if (flag == 0 .and. .not.associated(this%m_zAxis)) then
            allocate(this%m_zAxis, stat = flag)
        end if

        ! Error Checking
        if (flag /= 0) then
            call report_memory_error(errmgr, "p3d_init", flag)
            return
        end if
    end subroutine

! ------------------------------------------------------------------------------
    function p3d_get_cmd(this) result(x)
        !! Gets the GNUPLOT command string to represent this plot_3d object.
        class(plot_3d), intent(in) :: this
            !! The plot_3d object.
        character(len = :), allocatable :: x
            !! The command string.

        ! Local Variables
        type(string_builder) :: str
        integer(int32) :: i, n
        class(plot_data), pointer :: ptr
        class(plot_axis), pointer :: xAxis, yAxis, zAxis
        type(legend), pointer :: leg
        ! class(plot_label), pointer :: lbl

        ! Initialization
        call str%initialize()

        ! Call the base routine
        call str%append(this%plot%get_command_string())

        ! Grid
        if (this%get_show_gridlines()) then
            call str%append(new_line('a'))
            call str%append("set grid")
        end if

        ! Title
        n = len_trim(this%get_title())
        if (n > 0) then
            call str%append(new_line('a'))
            call str%append('set title "')
            call str%append(this%get_title())
            call str%append('"')
        end if

        ! Axes
        call str%append(new_line('a'))
        xAxis => this%get_x_axis()
        if (associated(xAxis)) call str%append(xAxis%get_command_string())

        call str%append(new_line('a'))
        yAxis => this%get_y_axis()
        if (associated(yAxis)) call str%append(yAxis%get_command_string())

        call str%append(new_line('a'))
        zAxis => this%get_z_axis()
        if (associated(zAxis)) call str%append(zAxis%get_command_string())

        ! Tic Marks
        if (.not.this%get_tics_inward()) then
            call str%append(new_line('a'))
            call str%append("set tics out")
        end if
        if (xAxis%get_zero_axis() .or. yAxis%get_zero_axis() .or. &
                zAxis%get_zero_axis()) then
            call str%append(new_line('a'))
            call str%append("set tics axis")
        end if

        ! Border
        if (this%get_draw_border()) then
            n = 31
        else
            n = 0
            if (.not.xAxis%get_zero_axis()) n = n + 1
            if (.not.yAxis%get_zero_axis()) n = n + 4
            if (.not.zAxis%get_zero_axis()) n = n + 16

            call str%append(new_line('a'))
            call str%append("set xtics nomirror")
            call str%append(new_line('a'))
            call str%append("set ytics nomirror")
            call str%append(new_line('a'))
            call str%append("set ztics nomirror")
        end if
        call str%append(new_line('a'))
        if (n > 0) then
            call str%append("set border ")
            call str%append(to_string(n))
        else
            call str%append("unset border")
        end if

        ! Force the z-axis to move to the x-y plane
        if (this%get_z_intersect_xy()) then
            call str%append(new_line('a'))
            call str%append("set ticslevel 0")
        end if

        ! Scaling
        if (this%get_axis_equal()) then
            call str%append(new_line('a'))
            call str%append("set view equal xyz")
        end if

        ! Legend
        call str%append(new_line('a'))
        leg => this%get_legend()
        if (associated(leg)) call str%append(leg%get_command_string())

        ! ! Labels
        ! do i = 1, this%get_label_count()
        !     lbl => this%get_label(i)
        !     if (.not.associated(lbl)) cycle
        !     call str%append(new_line('a'))
        !     call str%append(lbl%get_command_string())
        ! end do

        ! Orientation
        call str%append(new_line('a'))
        call str%append("set view ")
        if (this%get_use_map_view()) then
            call str%append("map")
        else
            call str%append(to_string(this%get_elevation()))
            call str%append(",")
            call str%append(to_string(this%get_azimuth()))
        end if

        ! Coordinate system
        if (this%get_coordinate_system() == COORDINATES_CYLINDRICAL) then
            call str%append(new_line('a'))
            call str%append("set mapping cylindrical")
        else if (this%get_coordinate_system() == COORDINATES_SPHERICAL) then
            call str%append(new_line('a'))
            call str%append("set mapping spherical")
        end if

        ! Define the plot function and data formatting commands
        n = this%get_count()
        call str%append(new_line('a'))
        call str%append("splot ")
        do i = 1, n
            ptr => this%get(i)
            if (.not.associated(ptr)) cycle
            call str%append(ptr%get_command_string())
            if (i /= n) call str%append(", ")
        end do

        ! Define the data to plot
        do i = 1, n
            ptr => this%get(i)
            if (.not.associated(ptr)) cycle
            call str%append(new_line('a'))
            call str%append(ptr%get_data_string())
            call str%append("e")
            ! if (i /= n) then
            !     call str%append("e")
            ! end if
        end do

        ! End
        x = char(str%to_string())
    end function

! ------------------------------------------------------------------------------
    function p3d_get_x_axis(this) result(ptr)
        !! Gets the x-axis object.
        class(plot_3d), intent(in) :: this
            !! The plot_3d object.
        class(plot_axis), pointer :: ptr
            !! A pointer to the x-axis object.
        ptr => this%m_xAxis
    end function

! ------------------------------------------------------------------------------
    function p3d_get_y_axis(this) result(ptr)
        !! Gets the y-axis object.
        class(plot_3d), intent(in) :: this
            !! The plot_3d object.
        class(plot_axis), pointer :: ptr
            !! A pointer to the y-axis object.
        ptr => this%m_yAxis
    end function

! ------------------------------------------------------------------------------
    function p3d_get_z_axis(this) result(ptr)
        !! Gets the z-axis object.
        class(plot_3d), intent(in) :: this
            !! The plot_3d object.
        class(plot_axis), pointer :: ptr
            !! A pointer to the z-axis object.
        ptr => this%m_zAxis
    end function

! ------------------------------------------------------------------------------
    pure function p3d_get_elevation(this) result(x)
        !! Gets the plot elevation angle.
        class(plot_3d), intent(in) :: this
            !! The plot_3d object.
        real(real64) :: x
            !! The elevation angle, in degrees.
        x = this%m_elevation
    end function

! --------------------
    subroutine p3d_set_elevation(this, x)
        !! Sets the plot elevation angle.
        class(plot_3d), intent(inout) :: this
            !! The plot_3d object.
        real(real64), intent(in) :: x
            !! The elevation angle, in degrees.
        this%m_elevation = x
    end subroutine

! ------------------------------------------------------------------------------
    pure function p3d_get_azimuth(this) result(x)
        !! Gets the plot azimuth angle.
        class(plot_3d), intent(in) :: this
            !! The plot_3d object.
        real(real64) :: x
            !! The azimuth angle, in degrees.
        x = this%m_azimuth
    end function

! --------------------
    subroutine p3d_set_azimuth(this, x)
        !! Sets the plot azimuth angle.
        class(plot_3d), intent(inout) :: this
            !! The plot_3d object.
        real(real64), intent(in) :: x
            !! The azimuth angle, in degrees.
        this%m_azimuth = x
    end subroutine

! ------------------------------------------------------------------------------
    pure function p3d_get_z_axis_intersect(this) result(x)
        !! Gets a value determining if the z-axis should intersect the
        !! x-y plane.
        class(plot_3d), intent(in) :: this
            !! The plot_3d object.
        logical :: x
            !! Returns true if the z-axis should intersect the x-y plane; else,
            !! false to allow the z-axis to float.
        x = this%m_zIntersect
    end function

! --------------------
    subroutine p3d_set_z_axis_intersect(this, x)
        !! Sets a value determining if the z-axis should intersect the
        !! x-y plane.
        class(plot_3d), intent(inout) :: this
            !! The plot_3d object.
        logical, intent(in) :: x
            !! Set to true if the z-axis should intersect the x-y plane; else,
            !! false to allow the z-axis to float.
        this%m_zIntersect = x
    end subroutine

! ADDED March 29, 2023 - JAC
! ------------------------------------------------------------------------------
    pure function p3d_get_use_map_view(this) result(rst)
        !! Gets a value determining if the view should be set to a 2D
        !! map view.  If true, the azimuth and elevation terms are ignored.
        class(plot_3d), intent(in) :: this
            !! The plot_3d object.
        logical :: rst
            !! Returns true if the map view will be used; else, false.
        rst = this%m_setMap
    end function

! --------------------
    subroutine p3d_set_use_map_view(this, x)
        !! Sets a value determining if the view should be set to a 2D
        !! map view.  If true, the azimuth and elevation terms are ignored.
        class(plot_3d), intent(inout) :: this
            !! The plot_3d object.
        logical, intent(in) :: x
            !! Seturns true if the map view will be used; else, false.
        this%m_setMap = x
    end subroutine

! ADDED Sept. 15, 2023 - JAC
! ------------------------------------------------------------------------------
    pure function p3d_get_csys(this) result(rst)
        !! Gets a value determining the coordinate system.
        class(plot_3d), intent(in) :: this
            !! The plot_3d object.
        integer(int32) :: rst
            !! The coordinate system ID, which must be one of the following.
            !!
            !! - COORDINATES_CARTESIAN
            !!
            !! - COORDINATES_CYLINDRICAL
            !!
            !! - COORDINATES_SPHERICAL
        rst = this%m_csys
    end function

! --------------------
    subroutine p3d_set_csys(this, x)
        !! Sets a value determining the coordinate system.
        class(plot_3d), intent(inout) :: this
            !! The plot_3d object.
        integer(int32), intent(in) :: x
            !! The coordinate system ID, which must be one of the following.
            !!
            !! - COORDINATES_CARTESIAN
            !!
            !! - COORDINATES_CYLINDRICAL
            !!
            !! - COORDINATES_SPHERICAL
        if (x /= COORDINATES_CARTESIAN .and. &
            x /= COORDINATES_CYLINDRICAL .and. &
            x /= COORDINATES_SPHERICAL) &
        then
            ! Set to default as the input is nonsensical
            this%m_csys = COORDINATES_CARTESIAN
        else
            this%m_csys = x
        end if
    end subroutine

! ------------------------------------------------------------------------------
end module
