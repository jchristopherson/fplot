! fplot_filled_plot_data.f90

module fplot_filled_plot_data
    use iso_fortran_env
    use fplot_plot_data
    use fplot_errors
    use fplot_colors
    use ferror
    use strings
    implicit none
    private
    public :: filled_plot_data

    type, extends(plot_data_colored) :: filled_plot_data
        !! Defines a two-dimensional filled plot data set.
        logical, private :: m_useY2 = .false.
            !! Plot against the secondary y-axis.
        real(real64), private, allocatable, dimension(:,:) :: m_data
            !! The data set (column 1 = x, column 2 = y, column 3 = constraint y)
    contains
        procedure, public :: get_axes_string => fpd_get_axes_cmd
        procedure, public :: get_draw_against_y2 => fpd_get_draw_against_y2
        procedure, public :: set_draw_against_y2 => fpd_set_draw_against_y2
        procedure, public :: get_command_string => fpd_get_cmd
        procedure, public :: get_data_string => fpd_get_data_cmd
        procedure, public :: define_data => fpd_define_data
    end type

contains
! ------------------------------------------------------------------------------
    function fpd_get_axes_cmd(this) result(x)
        !! Gets the GNUPLOT command string defining which axes the data
        !! is to be plotted against.
        class(filled_plot_data), intent(in) :: this
            !! The filled_plot_data object.
        character(len = :), allocatable :: x
            !! The command string.

        ! Define which axes the data is to be plotted against
        if (this%get_draw_against_y2()) then
            x = "axes x1y2"
        else
            x = "axes x1y1"
        end if
    end function

! ------------------------------------------------------------------------------
    pure function fpd_get_draw_against_y2(this) result(x)
        !! Gets a value determining if the data should be plotted against
        !! the secondary y-axis.
        class(filled_plot_data), intent(in) :: this
            !! The filled_plot_data object.
        logical :: x
            !! Returns true if the data should be plotted against the secondary
            !! y-axis; else, false to plot against the primary y-axis.
        x = this%m_useY2
    end function

! --------------------
    subroutine fpd_set_draw_against_y2(this, x)
        !! Sets a value determining if the data should be plotted against
        !! the secondary y-axis.
        class(filled_plot_data), intent(inout) :: this
            !! The filled_plot_data object.
        logical, intent(in) :: x
            !! Set to true if the data should be plotted against the secondary
            !! y-axis; else, false to plot against the primary y-axis.
        this%m_useY2 = x
    end subroutine

! ------------------------------------------------------------------------------
    function fpd_get_cmd(this) result(x)
        !! Gets the GNUPLOT command string to represent this
        !! filled_plot_data object.
        class(filled_plot_data), intent(in) :: this
            !! The filled_plot_data object.
        character(len = :), allocatable :: x
            !! The command string.

        ! Local Variables
        type(string_builder) :: str
        integer(int32) :: n
        type(color) :: clr

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

        ! Establish filled data
        call str%append(" with filledcurves")

        ! Line Color
        clr = this%get_line_color()
        call str%append(' lc rgb "#')
        call str%append(clr%to_hex_string())
        call str%append('"')

        ! Define the axes structure
        call str%append(" ")
        call str%append(this%get_axes_string())

        ! End
        x = char(str%to_string())
    end function

! ------------------------------------------------------------------------------
    function fpd_get_data_cmd(this) result(x)
        !! Gets the GNUPLOT command string containing the actual data to plot.
        class(filled_plot_data), intent(in) :: this
            !! The filled_plot_data object.
        character(len = :), allocatable :: x
            !! The command string.

        ! Local Variables
        type(string_builder) :: str
        integer(int32) :: i
        character(len = :), allocatable :: nl, delimiter

        ! Initialization
        call str%initialize()
        delimiter = achar(9)    ! tab delimiter
        nl = new_line(nl)

        ! Process
        do i = 1, size(this%m_data, 1)
            call str%append(to_string(this%m_data(i,1)))
            call str%append(delimiter)
            call str%append(to_string(this%m_data(i,2)))
            call str%append(delimiter)
            call str%append(to_string(this%m_data(i,3)))
            call str%append(nl)
        end do

        ! End
        x = char(str%to_string())
    end function

! ------------------------------------------------------------------------------
    subroutine fpd_define_data(this, x, y, yc, err)
        !! Defines the data set.
        class(filled_plot_data), intent(inout) :: this
            !! The filled_plot_data object.
        real(real64), intent(in), dimension(:) :: x
            !! An N-element array containing the x coordinate data.
        real(real64), intent(in), dimension(:) :: y
            !! An N-element array containing the y coordinate data.
        real(real64), intent(in), dimension(:) :: yc
            !! An N-element array containing the constraining curve y 
            !! coordinate data.
        class(errors), intent(inout), optional, target :: err
            !! An error handling object.

        ! Local Variables
        type(errors), target :: deferr
        class(errors), pointer :: errmgr
        integer(int32) :: i, n, flag

        ! Set up error handling
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Input Checking
        n = size(x)
        if (size(y) /= n) then
            call report_array_size_mismatch_error(errmgr, "fpd_define_data", &
                "y", n, size(y))
            return
        end if
        if (size(yc) /= n) then
            call report_array_size_mismatch_error(errmgr, "fpd_define_data", &
                "yc", n, size(yc))
            return
        end if

        ! Allocate space for the data
        if (allocated(this%m_data)) deallocate(this%m_data)
        allocate(this%m_data(n, 3), stat = flag)
        if (flag /= 0) then
            call report_memory_error(errmgr, "fpd_define_data", flag)
            return
        end if

        ! Store the data
        do concurrent (i = 1:n)
            this%m_data(i,1) = x(i)
            this%m_data(i,2) = y(i)
            this%m_data(i,3) = yc(i)
        end do
    end subroutine

! ------------------------------------------------------------------------------
end module
