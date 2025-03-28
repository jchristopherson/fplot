! fplot_polar.f90

module fplot_plot_polar
    use iso_fortran_env
    use fplot_plot
    use fplot_terminal
    use fplot_errors
    use fplot_constants
    use fplot_legend
    use fplot_plot_data
    use ferror
    use strings
    implicit none
    private
    public :: plot_polar

    type, extends(plot) :: plot_polar
    private
        !> @brief Allow the plot to autoscale?
        logical :: m_autoscale = .true.
        !> @brief The minimum radius value - only applicable if m_autoscale is
        !!  false.
        real(real64) :: m_minrad = 0.0d0
        !> @brief The maximum radius value - only applicable if m_autoscale is
        !!  false.
        real(real64) :: m_maxrad = 1.0d0
        !> @brief The location for theta = 0
        character(len = :), allocatable :: m_thetaStart 
        !> @brief The direction for theta
        character(len = :), allocatable :: m_thetaDirection
    contains
        final :: plr_clean_up
        procedure, public :: initialize => plr_init
        procedure, public :: get_command_string => plr_get_cmd
        procedure, public :: get_autoscale => plr_get_autoscale
        procedure, public :: set_autoscale => plr_set_autoscale
        procedure, public :: get_radial_limits => plr_get_limits
        procedure, public :: set_radial_limits => plr_set_limits
        procedure, public :: get_theta_start_position => plr_get_theta_start
        procedure, public :: set_theta_start_position => plr_set_theta_start
        procedure, public :: get_theta_direction => plr_get_theta_direction
        procedure, public :: set_theta_direction => plr_set_theta_direction
    end type

contains
! ------------------------------------------------------------------------------
    subroutine plr_clean_up(this)
        !! Cleans up resources held by the plot_polar object.
        type(plot_polar), intent(inout) :: this
            !! The plot_polar object.
        call this%free_resources()
    end subroutine

! ------------------------------------------------------------------------------
    subroutine plr_init(this, term, fname, err)
        !! Initializes the plot_polar object.
        class(plot_polar), intent(inout) :: this
            !! The plot_polar object.
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

        ! Initialize the rest of the object
        this%m_thetaStart = POLAR_THETA_RIGHT
        this%m_thetaDirection = POLAR_THETA_CCW
    end subroutine

! ------------------------------------------------------------------------------
    function plr_get_cmd(this) result(x)
        !! Gets the GNUPLOT command string to represent this plot_polar object.
        class(plot_polar), intent(in) :: this
            !! The plot_polar object.
        character(len = :), allocatable :: x
            !! The command string.

        ! Local Variables
        integer(int32) :: i, n
        type(string_builder) :: str
        type(legend), pointer :: leg
        real(real64) :: lim(2)
        ! class(plot_label), pointer :: lbl
        class(plot_data), pointer :: ptr

        ! Initialization
        call str%initialize()

        ! Call the base routine
        call str%append(this%plot%get_command_string())

        ! Polar-Specific Settings
        call str%append(new_line('a'))
        call str%append("unset border")

        call str%append(new_line('a'))
        call str%append("set polar")

        call str%append(new_line('a'))
        call str%append("set size square")

        call str%append(new_line('a'))
        call str%append("unset xtics")
        call str%append(new_line('a'))
        call str%append("unset ytics")

        call str%append(new_line('a'))
        call str%append('set ttics 0, 30 format "%g".GPVAL_DEGREE_SIGN')

        call str%append(new_line('a'))
        call str%append("set mttics 3")

        call str%append(new_line('a'))
        call str%append("set theta ")
        call str%append(this%get_theta_start_position())
        call str%append(" ")
        call str%append(this%get_theta_direction())

        ! Radial Limits
        if (.not.this%get_autoscale()) then
            lim = this%get_radial_limits()
            call str%append(new_line('a'))
            call str%append("set rrange [")
            call str%append(to_string(lim(1)))
            call str%append(":")
            call str%append(to_string(lim(2)))
            call str%append("]")
        end if

        ! Grid
        if (this%get_show_gridlines()) then
            call str%append(new_line('a'))
            call str%append("set grid r polar")
        end if

        ! Title
        n = len_trim(this%get_title())
        if (n > 0) then
            call str%append(new_line('a'))
            call str%append('set title "')
            call str%append(this%get_title())
            call str%append('"')
        end if

        ! Border
        call str%append(new_line('a'))
        if (this%get_draw_border()) then
            call str%append("set border polar")
        else
            call str%append("set border 0")
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

        ! Define the plot function and data formatting commands
        n = this%get_count()
        call str%append(new_line('a'))
        call str%append("plot ")
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
        end do

        ! End
        x = char(str%to_string())
    end function

! ------------------------------------------------------------------------------
    pure function plr_get_autoscale(this) result(rst)
        !! Gets a logical value determining if the axis should be 
        !! automatically scaled to fit the data.
        class(plot_polar), intent(in) :: this
            !! The plot_polar object.
        logical :: rst
            !! Returns true if the plot will autoscale; else, false.
        rst = this%m_autoscale
    end function

! --------------------
    subroutine plr_set_autoscale(this, x)
        !! Sets a logical value determining if the axis should be 
        !! automatically scaled to fit the data.
        class(plot_polar), intent(inout) :: this
            !! The plot_polar object.
        logical, intent(in) :: x
            !! Set to true if the plot will autoscale; else, false.
        this%m_autoscale = x
    end subroutine

! ------------------------------------------------------------------------------
    pure function plr_get_limits(this) result(rst)
        !! Gets the radial axis limits if autoscaling is inactive.
        class(plot_polar), intent(in) :: this
            !! The plot_polar object.
        real(real64) :: rst(2)
            !! A 2-element array containing the minimum and maximum limit
            !! values in that order.
        rst = [this%m_minrad, this%m_maxrad]
    end function

! --------------------
    subroutine plr_set_limits(this, x)
        !! Sets the radial axis limits if autoscaling is inactive.
        class(plot_polar), intent(inout) :: this
            !! The plot_polar object.
        real(real64), intent(in) :: x(2)
            !! A 2-element array containing the minimum and maximum limit
            !! values in that order.
        this%m_minrad = minval(x)
        this%m_maxrad = maxval(x)
    end subroutine

! ------------------------------------------------------------------------------
    pure function plr_get_theta_start(this) result(rst)
        !! Gets the position for \(\theta = 0\).
        class(plot_polar), intent(in) :: this
            !! The plot_polar object.
        character(len = :), allocatable :: rst
            !! The starting position.  It is one of the following flags.
            !!
            !!  - POLAR_THETA_BOTTOM
            !!
            !!  - POLAR_THETA_TOP
            !!
            !!  - POLAR_THETA_RIGHT
            !!
            !!  - POLAR_THETA_LEFT
        rst = this%m_thetaStart
    end function

! --------------------
    subroutine plr_set_theta_start(this, x)
        !! Sets the position for \(\theta = 0\).
        class(plot_polar), intent(inout) :: this
            !! The plot_polar object.
        character(len = *), intent(in) :: x
            !! The starting position.  It is one of the following flags.
            !!
            !!  - POLAR_THETA_BOTTOM
            !!
            !!  - POLAR_THETA_TOP
            !!
            !!  - POLAR_THETA_RIGHT
            !!
            !!  - POLAR_THETA_LEFT
        if (x /= POLAR_THETA_BOTTOM .and. &
            x /= POLAR_THETA_TOP .and. &
            x /= POLAR_THETA_LEFT .and. &
            x /= POLAR_THETA_RIGHT) &
        then
            ! Reset to default
            this%m_thetaStart = POLAR_THETA_RIGHT
        else
            this%m_thetaStart = x
        end if
    end subroutine

! ------------------------------------------------------------------------------
    pure function plr_get_theta_direction(this) result(rst)
        !! Gets the \(\theta\) direction.
        class(plot_polar), intent(in) :: this
            !! The plot_polar object.
        character(len = :), allocatable :: rst
            !! The direction.  It is one of the following flags.
            !!
            !!  - POLAR_THETA_CCW
            !!
            !!  - POLAR_THETA_CW
        rst = this%m_thetaDirection
    end function

! --------------------
    subroutine plr_set_theta_direction(this, x)
        !! Sets the \(\theta\) direction.
        class(plot_polar), intent(inout) :: this
            !! The plot_polar object.
        character(len = *), intent(in) :: x
            !! The direction.  It is one of the following flags.
            !!
            !!  - POLAR_THETA_CCW
            !!
            !!  - POLAR_THETA_CW
        if (x /= POLAR_THETA_CCW .and. x /= POLAR_THETA_CW) then
            ! Reset to default
            this%m_thetaDirection = POLAR_THETA_CCW
        else
            this%m_thetaDirection = x
        end if
    end subroutine

! ------------------------------------------------------------------------------
end module
