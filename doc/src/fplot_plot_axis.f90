! fplot_plot_axis.f90

module fplot_plot_axis
    use iso_fortran_env
    use fplot_plot_object
    use fplot_constants
    use strings
    implicit none
    private
    public :: plot_axis
    public :: pa_get_string_result
    public :: x_axis
    public :: y_axis
    public :: y2_axis
    public :: z_axis

    type, abstract, extends(plot_object) :: plot_axis
    private
        logical :: m_hasTitle = .false.
            !! Has a title?
        character(len = PLOTDATA_MAX_NAME_LENGTH) :: m_title = ""
            !! Axis title.
        logical :: m_autoscale = .true.
            !! Autoscale?
        real(real64), dimension(2) :: m_limits = [0.0d0, 1.0d0]
            !! Display limits.
        logical :: m_logScale = .false.
            !! Log scaled?
        logical :: m_zeroAxis = .false.
            !! Has a zero axis?
        real(real32) :: m_axisWidth = 1.0
            !! The width, in pixels, of the zero-axis line.
        logical :: m_defaultTicLabels = .true.
            !! Use default tic label format?
        character(len = PLOTDATA_MAX_NAME_LENGTH) :: m_ticLabelFmt = "%g"
            !! The tic lablel format.
    contains
        procedure, public :: get_title => pa_get_title
        procedure, public :: set_title => pa_set_title
        procedure, public :: is_title_defined => pa_has_title
        procedure, public :: get_autoscale => pa_get_autoscale
        procedure, public :: set_autoscale => pa_set_autoscale
        procedure, public :: get_limits => pa_get_axis_limits
        procedure, public :: set_limits => pa_set_axis_limits
        procedure, public :: get_is_log_scaled => pa_get_log_scale
        procedure, public :: set_is_log_scaled => pa_set_log_scale
        procedure, public :: get_command_string => pa_get_cmd_string
        procedure, public :: get_zero_axis => pa_get_zero_axis
        procedure, public :: set_zero_axis => pa_set_zero_axis
        procedure, public :: get_zero_axis_line_width => pa_get_zero_axis_width
        procedure, public :: set_zero_axis_line_width => pa_set_zero_axis_width
        procedure(pa_get_string_result), deferred, public :: get_id_string
        procedure, public :: get_use_default_tic_label_format => &
            pa_get_use_dft_tic_lbl_fmt
        procedure, public :: set_use_default_tic_label_format => &
            pa_set_use_dft_tic_lbl_fmt
        procedure, public :: get_tic_label_format => pa_get_tic_label_fmt
        procedure, public :: set_tic_label_format => pa_set_tic_label_fmt
    end type

    interface
        function pa_get_string_result(this) result(x)
            !! Retrieves a string from a plot_axis.
            import plot_axis
            class(plot_axis), intent(in) :: this
                !! The plot_axis object.
            character(len = :), allocatable :: x
                !! The string.
        end function
    end interface

    type, extends(plot_axis) :: x_axis
        !! Defines an x-axis object.
        character, private :: m_id = "x"
            !! The ID character.
    contains
        procedure, public :: get_id_string => xa_get_id
    end type

    type, extends(plot_axis) :: y_axis
        !! Defines a y-axis object.
        character, private :: m_id = "y"
            !! The ID character.
    contains
        procedure, public :: get_id_string => ya_get_id
    end type

    type, extends(plot_axis) :: y2_axis
        !! Defines a secondary y-axis object.
        character(len = 2), private :: m_id = "y2"
            !! The ID character.
    contains
        procedure, public :: get_id_string => y2a_get_id
    end type

    type, extends(plot_axis) :: z_axis
        !! Defines a z-axis object.
        character, private :: m_id = "z"
            !! The ID character.
    contains
        procedure, public :: get_id_string => za_get_id
    end type

contains
! ------------------------------------------------------------------------------
    function pa_get_title(this) result(txt)
        !! Gets the axis title.
        class(plot_axis), intent(in) :: this
            !! The plot_axis object.
        character(len = :), allocatable :: txt
            !! The title.
        integer(int32) :: n
        n = len_trim(this%m_title)
        allocate(character(len = n) :: txt)
        txt = trim(this%m_title)
    end function

! --------------------
    subroutine pa_set_title(this, txt)
        !! Sets the axis title.
        class(plot_axis), intent(inout) :: this
            !! The plot_axis object.
        character(len = *), intent(in) :: txt
            !! The title.

        ! Local Variables
        integer(int32) :: n

        ! Process
        n = min(len_trim(txt), PLOTDATA_MAX_NAME_LENGTH)
        this%m_title = ""
        if (n /= 0) then
            this%m_title(1:n) = txt(1:n)
            this%m_hasTitle = .true.
        else
            this%m_hasTitle = .false.
        end if
    end subroutine

! ------------------------------------------------------------------------------
    pure function pa_has_title(this) result(x)
        !! Gets a value determining if a title has been defined for this axis.
        class(plot_axis), intent(in) :: this
            !! The plot_axis object.
        logical :: x
            !! Returns true if a title has been defined; else, false.
        x = this%m_hasTitle
    end function

! ------------------------------------------------------------------------------
    pure function pa_get_autoscale(this) result(x)
        !! Gets a value determining if the axis should be automatically scaled
        !! to fit the data.
        class(plot_axis), intent(in) :: this
            !! The plot_axis object.
        logical :: x
            !! Returns true if the axis should be automatically scaled; else, 
            !! false.
        x = this%m_autoscale
    end function

! --------------------
    subroutine pa_set_autoscale(this, x)
        !! Sets a value determining if the axis should be automatically scaled
        !! to fit the data.
        class(plot_axis), intent(inout) :: this
            !! The plot_axis object.
        logical, intent(in) :: x
            !! Set to true if the axis should be automatically scaled; else, 
            !! set to false.
        this%m_autoscale = x
    end subroutine

! ------------------------------------------------------------------------------
    pure function pa_get_axis_limits(this) result(x)
        !! Gets the axis display limits, assuming autoscaling is not
        !! active for this axis.
        class(plot_axis), intent(in) :: this
            !! The plot_axis object.
        real(real64), dimension(2) :: x
            !! A two-element array containing the limits as follows:
            !! [lower, upper].
        x(1) = minval(this%m_limits)
        x(2) = maxval(this%m_limits)
    end function

! --------------------
    subroutine pa_set_axis_limits(this, lower, upper)
        !! Gets the axis display limits, assuming autoscaling is not
        !! active for this axis.
        class(plot_axis), intent(inout) :: this
            !! The plot_axis object.
        real(real64), intent(in) :: lower
            !! The lower display limit.
        real(real64), intent(in) :: upper
            !! The upper display limit.
        this%m_limits(1) = min(lower, upper)
        this%m_limits(2) = max(lower, upper)
    end subroutine

! ------------------------------------------------------------------------------
    pure function pa_get_log_scale(this) result(x)
        !! Gets a logical value defining if the axis should be log scaled.
        class(plot_axis), intent(in) :: this
            !! The plot_axis object.
        logical :: x
            !! Returns true if log scaling is applied to the axis; else, false.
        x = this%m_logScale
    end function

! --------------------
    subroutine pa_set_log_scale(this, x)
        !! Sets a logical value defining if the axis should be log scaled.
        class(plot_axis), intent(inout) :: this
            !! The plot_axis object.
        logical, intent(in) :: x
            !! Set to true if log scaling is applied to the axis; else, false.
        this%m_logScale = x
    end subroutine

! ------------------------------------------------------------------------------
    function pa_get_cmd_string(this) result(txt)
        !! Returns the appropriate GNUPLOT command string to define the
        !! plot_axis properties.
        class(plot_axis), intent(in) :: this
            !! The plot_axis object.
        character(len = :), allocatable :: txt
            !! The GNUPLOT command string.

        ! Local Variables
        type(string_builder) :: str
        character(len = :), allocatable :: axis, fmt
        real(real64) :: lim(2)

        ! Process
        axis = this%get_id_string()
        fmt = this%get_tic_label_format()
        lim = this%get_limits()
        call str%initialize()

        ! Formatting
        if (.not.this%get_use_default_tic_label_format()) then
            call str%append("set format ")
            call str%append(axis)
            call str%append('"')
            call str%append(fmt)
            call str%append('"')
            call str%append(new_line('a'))
        end if

        ! Axis Limits
        if (this%get_autoscale()) then
            call str%append("set ")
            call str%append(axis)
            call str%append("range [*:*]")
        else
            call str%append("set ")
            call str%append(axis)
            call str%append("range [")
            call str%append(to_string(lim(1)))
            call str%append(":")
            call str%append(to_string(lim(2)))
            call str%append("]")
        end if

        ! Titles
        call str%append(new_line('a'))
        if (this%is_title_defined()) then
            call str%append("set ")
            call str%append(axis)
            call str%append("label ")
            call str%append('"')
            call str%append(this%get_title())
            call str%append('"')
        else
            call str%append("set ")
            call str%append(axis)
            call str%append("label ")
            call str%append('""')
        end if

        ! Scaling
        call str%append(new_line('a'))
        if (this%get_is_log_scaled()) then
            call str%append("set log ")
            call str%append(axis)
        else
            call str%append("unset log ")
            call str%append(axis)
        end if

        ! Zero Axis
        if (this%get_zero_axis()) then
            call str%append(new_line('a'))
            call str%append("set ")
            call str%append(this%get_id_string())
            call str%append("zeroaxis linestyle -1 linewidth ")
            call str%append(to_string(this%get_zero_axis_line_width()))
        end if

        ! Output
        txt = char(str%to_string())
    end function

! ------------------------------------------------------------------------------
    pure function pa_get_zero_axis(this) result(x)
        !! Gets a value determining if the axis should be drawn through
        !! zero of opposing axes.
        class(plot_axis), intent(in) :: this
            !! The plot_axis object.
        logical :: x
            !! Returns true to draw as a zero axis; else, set to false.
        x = this%m_zeroAxis
    end function

! --------------------
    subroutine pa_set_zero_axis(this, x)
        !! Sets a value determining if the axis should be drawn through
        !! zero of opposing axes.
        class(plot_axis), intent(inout) :: this
            !! The plot_axis object.
        logical, intent(in) :: x
            !! Set to true to draw as a zero axis; else, set to false.
        this%m_zeroAxis = x
    end subroutine

! ------------------------------------------------------------------------------
    pure function pa_get_zero_axis_width(this) result(x)
        !! Gets the width of the line used to represent the zero axis line, if 
        !! active.
        class(plot_axis), intent(in) :: this
            !! The plot_axis object.
        real(real32) :: x
            !! The width of the line, in pixels.
        x = this%m_axisWidth
    end function

! --------------------
    subroutine pa_set_zero_axis_width(this, x)
        !! Sets the width of the line used to represent the zero axis line, if 
        !! active.
        class(plot_axis), intent(inout) :: this
            !! The plot_axis object.
        real(real32), intent(in) :: x
            !! The width of the line, in pixels.
        this%m_axisWidth = x
    end subroutine

! ADDED March 29, 2023 - JAC
! ------------------------------------------------------------------------------
    pure function pa_get_use_dft_tic_lbl_fmt(this) result(rst)
        !! Gets a value determining if the default tic label format will be 
        !! used.
        class(plot_axis), intent(in) :: this
            !! The plot_axis object.
        logical :: rst
            !! Returns true if the default tic label format will be used; else, 
            !! false.
        rst = this%m_defaultTicLabels
    end function

! --------------------
    subroutine pa_set_use_dft_tic_lbl_fmt(this, x)
        !! Sets a value determining if the default tic label format will be 
        !! used.
        class(plot_axis), intent(inout) :: this
            !! The plot_axis object.
        logical, intent(in) :: x
            !! Set to true if the default tic label format will be used; else, 
            !! false.
        this%m_defaultTicLabels = x
    end subroutine

! ------------------------------------------------------------------------------
    pure function pa_get_tic_label_fmt(this) result(rst)
        !! Gets the tic label format.  The format string can be any format 
        !! string accepted by the C command 'printf.'
        class(plot_axis), intent(in) :: this
            !! The plot_axis object.
        character(len = :), allocatable :: rst
            !! The tic label format string.
        rst = trim(this%m_ticLabelFmt)
    end function

! --------------------
    subroutine pa_set_tic_label_fmt(this, x)
        !! Sets the tic label format.  The format string can be any format 
        !! string accepted by the C command 'printf.'
        class(plot_axis), intent(inout) :: this
            !! The plot_axis object.
        character(len = *), intent(in) :: x
            !! The tic label format string.
        this%m_ticLabelFmt = x
    end subroutine

! ******************************************************************************
! X_AXIS MEMBERS
! ------------------------------------------------------------------------------
    function xa_get_id(this) result(x)
        !! Gets the axis identification string.
        class(x_axis), intent(in) :: this
            !! The x_axis object.
        character(len = :), allocatable :: x
            !! The identification string.
        x = this%m_id
    end function

! ******************************************************************************
! Y_AXIS MEMBERS
! ------------------------------------------------------------------------------
    function ya_get_id(this) result(x)
        !! Gets the axis identification string.
        class(y_axis), intent(in) :: this
            !! The y_axis object.
        character(len = :), allocatable :: x
            !! The identification string.
        x = this%m_id
    end function

! ******************************************************************************
! Y2_AXIS MEMBERS
! ------------------------------------------------------------------------------
    function y2a_get_id(this) result(x)
        !! Gets the axis identification string.
        class(y2_axis), intent(in) :: this
            !! The y2_axis object.
        character(len = :), allocatable :: x
            !! The identification string.
        x = this%m_id
    end function

! ******************************************************************************
! Z_AXIS MEMBERS
! ------------------------------------------------------------------------------
    function za_get_id(this) result(x)
        !! Gets the axis identification string.
        class(z_axis), intent(in) :: this
            !! The z_axis object.
        character(len = :), allocatable :: x
            !! The identification string.
        x = this%m_id
    end function

! ------------------------------------------------------------------------------
end module