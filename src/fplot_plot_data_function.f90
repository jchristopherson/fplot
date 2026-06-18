module fplot_plot_data_function
    use iso_fortran_env
    use fplot_plot_data
    use fplot_colors
    use strings
    implicit none
    private
    public :: plot_data_function

    type, extends(plot_data_colored) :: plot_data_function
        !! Defines a function to plot.
        character(len = :), private, allocatable :: m_function
            !! The function to plot (e.g. sin(x))
        character(len = :), private, allocatable :: m_functionName
            !! The name of the function (e.g. f(x), g(x), etc.).
        real(real64), private :: m_minX = -1.0d0
            !! The minimum X value of the plot range.
        real(real64), private :: m_maxX = 1.0d0
            !! The maximum X value of the plot range.
    contains
        procedure, public :: get_function => pdf_get_function
        procedure, public :: get_function_name => pdf_get_function_name
        procedure, public :: define_data => pdf_define_data
        procedure, public :: get_command_string => pdf_get_cmd
        procedure, public :: get_data_string => pdf_get_data_cmd
        procedure, public :: clear_data => pdf_clear_data
        procedure, public :: get_minimum_x => pdf_get_min_x
        procedure, public :: set_minimum_x => pdf_set_min_x
        procedure, public :: get_maximum_x => pdf_get_max_x
        procedure, public :: set_maximum_x => pdf_set_max_x
    end type

contains
! ------------------------------------------------------------------------------
    pure function pdf_get_function(this) result(rst)
        !! Gets the function to plot.
        class(plot_data_function), intent(in) :: this
            !! The plot_data_function object.
        character(len = :), allocatable :: rst
            !! The function.

        if (allocated(this%m_function)) then
            rst = this%m_function
        else
            rst = ""
        end if
    end function

! ------------------------------------------------------------------------------
    pure function pdf_get_function_name(this) result(rst)
        !! Gets the name of the function to plot (e.g. f(x), g(x), etc.).
        class(plot_data_function), intent(in) :: this
            !! The plot_data_function object.
        character(len = :), allocatable :: rst
            !! The function name.

        if (allocated(this%m_functionName)) then
            rst = this%m_functionName
        else
            rst = ""
        end if
    end function

! ------------------------------------------------------------------------------
    subroutine pdf_define_data(this, f, fname, minX, maxX)
        !! Defines the function to plot.
        class(plot_data_function), intent(inout) :: this
            !! The plot_data_function object.
        character(len = *), intent(in) :: f
            !! The function to plot (e.g. sin(x)).
        character(len = *), intent(in) :: fname
            !! The name of the function to plot (e.g. f(x)).
        real(real64), intent(in), optional :: minX
            !! The minimum value to plot.  If not supplied, this will default
            !! to a value of -1.
        real(real64), intent(in), optional :: maxX
            !! The maximum x value to plot. If not supplied, this will default
            !! to a value of 1.

        this%m_function = f
        this%m_functionName = fname

        if (present(minX)) then
            this%m_minX = minX
        else
            this%m_minX = -1.0d0
        end if

        if (present(maxX)) then
            this%m_maxX = maxX
        else
            this%m_maxX = 1.0d0
        end if
    end subroutine

! ------------------------------------------------------------------------------
    function pdf_get_cmd(this) result(rst)
        !! Gets the GNUPLOT command string for this object.
        class(plot_data_function), intent(in) :: this
            !! The plot_data_function object.
        character(len = :), allocatable :: rst
            !! The command string.

        ! Local Variables
        type(string_builder) :: str
        type(color) :: clr

        ! Build the command string
        call str%initialize()
        call str%append("[")
        call str%append(to_string(this%get_minimum_x()))
        call str%append(":")
        call str%append(to_string(this%get_maximum_x()))
        call str%append("] ")
        call str%append(this%get_function_name())

        ! Line Color
        clr = this%get_line_color()
        call str%append(' lc rgb "#')
        call str%append(clr%to_hex_string())
        call str%append('"')

        rst = char(str%to_string())
    end function

! ------------------------------------------------------------------------------
    function pdf_get_data_cmd(this) result(rst)
        !! Gets the GNUPLOT command string defining the data for this object.
        class(plot_data_function), intent(in) :: this
            !! The plot_data_function object.
        character(len = :), allocatable :: rst
            !! The command string.

        ! Local Variables
        type(string_builder) :: str

        ! Build the command string
        call str%initialize()
        call str%append(new_line('a'))
        call str%append(this%get_function_name())
        call str%append(" = ")
        call str%append(this%get_function())

        rst = char(str%to_string())
    end function

! ------------------------------------------------------------------------------
    subroutine pdf_clear_data(this)
        !! Clears the function from this data set.
        class(plot_data_function), intent(inout) :: this
            !! The plot_data_function object.
        this%m_function = ""
        this%m_functionName = ""
    end subroutine

! ------------------------------------------------------------------------------
    pure function pdf_get_min_x(this) result(rst)
        !! Gets the minimum X plot range value.
        class(plot_data_function), intent(in) :: this
            !! The plot_data_function object.
        real(real64) :: rst
            !! The value.
        rst = this%m_minX
    end function

! --------------------
    subroutine pdf_set_min_x(this, x)
        !! Sets the minimum X plot range value.
        class(plot_data_function), intent(inout) :: this
            !! The plot_data_function object.
        real(real64), intent(in) :: x
            !! The value.
        this%m_minX = x
    end subroutine

! ------------------------------------------------------------------------------
    pure function pdf_get_max_x(this) result(rst)
        !! Gets the maximum X plot range value.
        class(plot_data_function), intent(in) :: this
            !! The plot_data_function object.
        real(real64) :: rst
            !! The value.
        rst = this%m_maxX
    end function

! --------------------
    subroutine pdf_set_max_x(this, x)
        !! Sets the maximum X plot range value.
        class(plot_data_function), intent(inout) :: this
            !! The plot_data_function object.
        real(real64), intent(in) :: x
            !! The value.
        this%m_maxX = x
    end subroutine

! ------------------------------------------------------------------------------

! --------------------

! ------------------------------------------------------------------------------

! --------------------

! ------------------------------------------------------------------------------

! --------------------

! ------------------------------------------------------------------------------
end module