! fplot_plot_data_error_bars.f90

module fplot_plot_data_error_bars
    use iso_fortran_env
    use fplot_plot_data
    use fplot_errors
    use fplot_colors
    use ferror
    use strings
    implicit none
    private
    public :: plot_data_error_bars

    type, extends(plot_data_colored) :: plot_data_error_bars
        !! Defines a 2D error-bar based data set.
        logical, private :: m_xBars = .false.
            !! Display x error bars?
        logical, private :: m_yBars = .false.
            !! Display y error bars?
        real(real64), private, allocatable, dimension(:,:) :: m_data
            !! A matrix containing the raw and error data.  Column 1 is for the
            !! x coordinate, column 2 for the y coordinate, and the remaining 
            !! columns are for the error data (x, then y if applicable).
        logical, private :: m_box = .false.
            !! Display an error box for the case where x and y errors are
            !! defined.
        logical, private :: m_range = .false.
            !! Plot error bars using a defined range vs. a +/- value.
    contains
        procedure, public :: get_command_string => pde_get_cmd
        procedure, public :: get_data_string => pde_get_data_cmd
        generic, public :: define_x_error_data => pde_define_x_err, &
            pde_define_x_err_lim
        generic, public :: define_y_error_data => pde_define_y_err, &
            pde_define_y_err_lim
        generic, public :: define_xy_error_data => pde_define_xy_err, &
            pde_define_xy_err_lim
        procedure, public :: get_plot_x_error_bars => pde_get_plot_x_err
        procedure, public :: get_plot_y_error_bars => pde_get_plot_y_err
        procedure, public :: get_count => pde_get_count
        procedure, public :: get_use_error_box => pde_get_box
        procedure, public :: set_use_error_box => pde_set_box
        procedure, public :: get_use_range => pde_get_use_range

        procedure :: pde_define_x_err
        procedure :: pde_define_y_err
        procedure :: pde_define_xy_err
        procedure :: pde_define_x_err_lim
        procedure :: pde_define_y_err_lim
        procedure :: pde_define_xy_err_lim
    end type

contains
! ------------------------------------------------------------------------------
    function pde_get_cmd(this) result(cmd)
        !! Gets the appropriate GNUPLOT command string for the object.
        class(plot_data_error_bars), intent(in) :: this
            !! The plot_data_error_bars object.
        character(len = :), allocatable :: cmd
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

        ! Color
        clr = this%get_line_color()
        call str%append(' lc rgb "#')
        call str%append(clr%to_hex_string())
        call str%append('"')

        ! Error Bars
        if (this%get_plot_x_error_bars() .and. this%get_plot_y_error_bars()) then
            if (this%get_use_error_box()) then
                call str%append(" w boxxyerr")
            else
                call str%append(" w xyerr")
            end if
        else if (this%get_plot_x_error_bars() .and. .not.this%get_plot_y_error_bars()) then
            call str%append(" w xerr")
        else if (.not.this%get_plot_x_error_bars() .and. this%get_plot_y_error_bars()) then
            call str%append(" w yerr")
        end if

        ! Output
        cmd = char(str%to_string())
    end function

! ------------------------------------------------------------------------------
    function pde_get_data_cmd(this) result(cmd)
        !! Gets the appropriate GNUPLOT commands to plot the data itself.
        class(plot_data_error_bars), intent(in) :: this
            !! The plot_data_error_bars object.
        character(len = :), allocatable :: cmd
            !! The command string.

        ! Local Variables
        type(string_builder) :: str
        integer(int32) :: i, n
        character :: delimiter, nl

        ! Initialization
        call str%initialize()
        delimiter = achar(9) ! tab delimiter
        nl = new_line(nl)
        n = this%get_count()

        ! Process
        if (this%get_plot_x_error_bars() .and. this%get_plot_y_error_bars()) then
            if (this%get_use_range()) then
                do i = 1, n
                    call str%append(to_string(this%m_data(i, 1)))
                    call str%append(delimiter)
                    call str%append(to_string(this%m_data(i, 2)))
                    call str%append(delimiter)
                    call str%append(to_string(this%m_data(i, 3)))
                    call str%append(delimiter)
                    call str%append(to_string(this%m_data(i, 4)))
                    call str%append(delimiter)
                    call str%append(to_string(this%m_data(i, 5)))
                    call str%append(delimiter)
                    call str%append(to_string(this%m_data(i, 6)))
                    call str%append(nl)
                end do
            else
                do i = 1, n
                    call str%append(to_string(this%m_data(i, 1)))
                    call str%append(delimiter)
                    call str%append(to_string(this%m_data(i, 2)))
                    call str%append(delimiter)
                    call str%append(to_string(this%m_data(i, 3)))
                    call str%append(delimiter)
                    call str%append(to_string(this%m_data(i, 4)))
                    call str%append(nl)
                end do
            end if
        else
            if (this%get_use_range()) then
                do i = 1, n
                    call str%append(to_string(this%m_data(i, 1)))
                    call str%append(delimiter)
                    call str%append(to_string(this%m_data(i, 2)))
                    call str%append(delimiter)
                    call str%append(to_string(this%m_data(i, 3)))
                    call str%append(delimiter)
                    call str%append(to_string(this%m_data(i, 4)))
                    call str%append(nl)
                end do
            else
                do i = 1, n
                    call str%append(to_string(this%m_data(i, 1)))
                    call str%append(delimiter)
                    call str%append(to_string(this%m_data(i, 2)))
                    call str%append(delimiter)
                    call str%append(to_string(this%m_data(i, 3)))
                    call str%append(nl)
                end do
            end if
        end if

        ! if (this%get_plot_x_error_bars() .and. this%get_plot_y_error_bars()) then
        !     do i = 1, n
        !         call str%append(to_string(this%m_data(i, 1)))
        !         call str%append(delimiter)
        !         call str%append(to_string(this%m_data(i, 2)))
        !         call str%append(delimiter)
        !         call str%append(to_string(this%m_data(i, 3)))
        !         call str%append(delimiter)
        !         call str%append(to_string(this%m_data(i, 4)))
        !         call str%append(nl)
        !     end do
        ! else
        !     do i = 1, n
        !         call str%append(to_string(this%m_data(i, 1)))
        !         call str%append(delimiter)
        !         call str%append(to_string(this%m_data(i, 2)))
        !         call str%append(delimiter)
        !         call str%append(to_string(this%m_data(i, 3)))
        !         call str%append(nl)
        !     end do
        ! end if

        ! End
        cmd = char(str%to_string())
    end function

! ------------------------------------------------------------------------------
    subroutine pde_define_x_err(this, x, y, xerr, err)
        !! Defines the x error data.
        class(plot_data_error_bars), intent(inout) :: this
            !! The plot_data_error_bars object.
        real(real64), intent(in), dimension(:) :: x
            !! An N-element array containing the x coordinates of the data.
        real(real64), intent(in), dimension(:) :: y
            !! An N-element array containing the y coordinates of the data.
        real(real64), intent(in), dimension(:) :: xerr
            !! An N-element array containing the x errors at each data point.
        class(errors), intent(inout), optional, target :: err
            !! An error handling object.

        ! Local Variables
        integer(int32) :: i, n, flag
        class(errors), pointer :: errmgr
        type(errors), target :: deferr

        ! Initialization
        n = size(x)
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Input Checking
        if (size(y) /= n) then
            call report_array_size_mismatch_error(errmgr, "pde_define_x_err", &
                "y", n, size(y))
            return
        end if

        if (size(xerr) /= n) then
            call report_array_size_mismatch_error(errmgr, "pde_define_x_err", &
                "xerr", n, size(xerr))
            return
        end if

        ! Process
        this%m_xBars = .false.
        this%m_yBars = .false.
        this%m_range = .false.
        if (allocated(this%m_data)) deallocate(this%m_data)
        allocate(this%m_data(n, 3), stat = flag)
        if (flag /= 0) then
            call report_memory_error(errmgr, "pde_define_x_err", flag)
            return
        end if
        do i = 1, n
            this%m_data(i, 1) = x(i)
            this%m_data(i, 2) = y(i)
            this%m_data(i, 3) = xerr(i)
        end do
        this%m_xBars = .true.
        this%m_range = .false.
    end subroutine

! ------------------------------------------------------------------------------
    subroutine pde_define_y_err(this, x, y, yerr, err)
        !! Defines the y error data.
        class(plot_data_error_bars), intent(inout) :: this
            !! The plot_data_error_bars object.
        real(real64), intent(in), dimension(:) :: x
            !! An N-element array containing the x coordinates of the data.
        real(real64), intent(in), dimension(:) :: y
            !! An N-element array containing the y coordinates of the data.
        real(real64), intent(in), dimension(:) :: yerr
            !! An N-element array containing the y errors at each data point.
        class(errors), intent(inout), optional, target :: err
            !! An error handling object.

        ! Local Variables
        integer(int32) :: i, n, flag
        class(errors), pointer :: errmgr
        type(errors), target :: deferr

        ! Initialization
        n = size(x)
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Input Checking
        if (size(y) /= n) then
            call report_array_size_mismatch_error(errmgr, "pde_define_y_err", &
                "y", n, size(y))
            return
        end if
        if (size(yerr) /= n) then
            call report_array_size_mismatch_error(errmgr, "pde_define_y_err", &
                "yerr", n, size(yerr))
            return
        end if

        ! Process
        this%m_xBars = .false.
        this%m_yBars = .false.
        this%m_range = .false.
        if (allocated(this%m_data)) deallocate(this%m_data)
        allocate(this%m_data(n, 3), stat = flag)
        if (flag /= 0) then
            call report_memory_error(errmgr, "pde_define_y_err", flag)
            return
        end if
        do i = 1, n
            this%m_data(i, 1) = x(i)
            this%m_data(i, 2) = y(i)
            this%m_data(i, 3) = yerr(i)
        end do
        this%m_yBars = .true.
        this%m_range = .false.
    end subroutine

! ------------------------------------------------------------------------------
    subroutine pde_define_xy_err(this, x, y, xerr, yerr, err)
        !! Defines x and y error data.
        class(plot_data_error_bars), intent(inout) :: this
            !! The plot_data_error_bars object.
        real(real64), intent(in), dimension(:) :: x
            !! An N-element array containing the x coordinates of the data.
        real(real64), intent(in), dimension(:) :: y
            !! An N-element array containing the y coordinates of the data.
        real(real64), intent(in), dimension(:) :: xerr
            !! An N-element array containing the x errors at each data point.
        real(real64), intent(in), dimension(:) :: yerr
            !! An N-element array containing the y errors at each data point.
        class(errors), intent(inout), optional, target :: err
            !! An error handling object.

        ! Local Variables
        integer(int32) :: i, n, flag
        class(errors), pointer :: errmgr
        type(errors), target :: deferr

        ! Initialization
        n = size(x)
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Input Checking
        if (size(y) /= n) then
            call report_array_size_mismatch_error(errmgr, "pde_define_xy_err", &
                "y", n, size(y))
            return
        end if

        if (size(xerr) /= n) then
            call report_array_size_mismatch_error(errmgr, "pde_define_xy_err", &
                "xerr", n, size(xerr))
            return
        end if

        if (size(yerr) /= n) then
            call report_array_size_mismatch_error(errmgr, "pde_define_xy_err", &
                "yerr", n, size(yerr))
            return
        end if

        ! Process
        this%m_xBars = .false.
        this%m_yBars = .false.
        this%m_range = .false.
        if (allocated(this%m_data)) deallocate(this%m_data)
        allocate(this%m_data(n, 4), stat = flag)
        if (flag /= 0) then
            call report_memory_error(errmgr, "pde_define_xy_err", flag)
            return
        end if
        do i = 1, n
            this%m_data(i, 1) = x(i)
            this%m_data(i, 2) = y(i)
            this%m_data(i, 3) = xerr(i)
            this%m_data(i, 4) = yerr(i)
        end do
        this%m_xBars = .true.
        this%m_yBars = .true.
        this%m_range = .false.
    end subroutine

! ------------------------------------------------------------------------------
    pure function pde_get_plot_x_err(this) result(x)
        !! Checks to see if the x error bar data has been defined, and as
        !! a result, if the x error data is to be plotted.
        class(plot_data_error_bars), intent(in) :: this
            !! The plot_data_error_bars object.
        logical :: x
            !! Returns true if the x error bars are to be plotted; else, false.
        x = this%m_xBars
    end function

! ------------------------------------------------------------------------------
    pure function pde_get_plot_y_err(this) result(x)
        !! Checks to see if the y error bar data has been defined, and as
        !! a result, if the x error data is to be plotted.
        class(plot_data_error_bars), intent(in) :: this
            !! The plot_data_error_bars object.
        logical :: x
            !! Returns true if the y error bars are to be plotted; else, false.
        x = this%m_yBars
    end function
! ------------------------------------------------------------------------------
    pure function pde_get_count(this) result(x)
        !! Gets the number of stored data points.
        class(plot_data_error_bars), intent(in) :: this
            !! The plot_data_error_bars object.
        integer(int32) :: x
            !! The number of data points.
        if (allocated(this%m_data)) then
            x = size(this%m_data, 1)
        else
            x = 0
        end if
    end function

! ------------------------------------------------------------------------------
    pure function pde_get_box(this) result(x)
        !! Checks to see if the x and y error boxes should be utilized.
        class(plot_data_error_bars), intent(in) :: this
            !! The plot_data_error_bars object.
        logical :: x
            !! Returns true if the error boxes are to be plotted; else,
            !! false.  Notice, the error boxes are only utilized if there is 
            !! both x and y error data defined, regardless of the value of this 
            !! property.
        x = this%m_box
    end function

! --------------------
    subroutine pde_set_box(this, x)
        !! Deterimines if the x and y error boxes should be utilized.
        class(plot_data_error_bars), intent(inout) :: this
            !! The plot_data_error_bars object.
        logical, intent(in) :: x
            !! Set to true if the error boxes are to be plotted; else,
            !! false.  Notice, the error boxes are only utilized if there is 
            !! both x and y error data defined, regardless of the value of this 
            !! property.
        this%m_box = x
    end subroutine

! ------------------------------------------------------------------------------
    pure function pde_get_use_range(this) result(x)
        !! Gets a value determining if a defined range is being used
        !! to define the error bar extremes.
        class(plot_data_error_bars), intent(in) :: this
            !! The plot_data_error_bars object.
        logical :: x
            !! True if a defined range is being used; else, false.
        x = this%m_range
    end function

! ------------------------------------------------------------------------------
    subroutine pde_define_x_err_lim(this, x, y, xmin, xmax, err)
        !! Defines the x error data.
        class(plot_data_error_bars), intent(inout) :: this
            !! The plot_data_error_bars object.
        real(real64), intent(in), dimension(:) :: x
            !! An N-element array containing the x coordinates of the data.
        real(real64), intent(in), dimension(:) :: y
            !! An N-element array containing the y coordinates of the data.
        real(real64), intent(in), dimension(:) :: xmin
            !! An N-element array containing the minimum x values at each data 
            !! point.
        real(real64), intent(in), dimension(:) :: xmax
            !! An N-element array containing the maximum x values at each data 
            !! point.
        class(errors), intent(inout), optional, target :: err
            !! An error handling object.

        ! Local Variables
        integer(int32) :: i, n, flag
        class(errors), pointer :: errmgr
        type(errors), target :: deferr

        ! Initialization
        n = size(x)
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Input Checking
        if (size(y) /= n) then
            call report_array_size_mismatch_error(errmgr, &
                "pde_define_x_err_lim", "y", n, size(y))
            return
        end if

        if (size(xmin) /= n) then
            call report_array_size_mismatch_error(errmgr, &
                "pde_define_x_err_lim", "xmin", n, size(xmin))
            return
        end if

        if (size(xmax) /= n) then
            call report_array_size_mismatch_error(errmgr, &
                "pde_define_x_err_lim", "xmax", n, size(xmax))
            return
        end if

        ! Process
        this%m_xBars = .false.
        this%m_yBars = .false.
        this%m_range = .false.
        if (allocated(this%m_data)) deallocate(this%m_data)
        allocate(this%m_data(n, 4), stat = flag)
        if (flag /= 0) then
            call report_memory_error(errmgr, "pde_define_x_err_lim", flag)
            return
        end if
        do i = 1, n
            this%m_data(i, 1) = x(i)
            this%m_data(i, 2) = y(i)
            this%m_data(i, 3) = xmin(i)
            this%m_data(i, 4) = xmax(i)
        end do
        this%m_xBars = .true.
        this%m_range = .true.
    end subroutine

! ------------------------------------------------------------------------------
    subroutine pde_define_y_err_lim(this, x, y, ymin, ymax, err)
        !! Defines the y error data.
        class(plot_data_error_bars), intent(inout) :: this
            !! The plot_data_error_bars object.
        real(real64), intent(in), dimension(:) :: x
            !! An N-element array containing the x coordinates of the data.
        real(real64), intent(in), dimension(:) :: y
            !! An N-element array containing the y coordinates of the data.
        real(real64), intent(in), dimension(:) :: ymin
            !! An N-element array containing the minimum y values at each data 
            !! point.
        real(real64), intent(in), dimension(:) :: ymax
            !! An N-element array containing the maximum y values at each data 
            !! point.
        class(errors), intent(inout), optional, target :: err
            !! An error handling object.

        ! Local Variables
        integer(int32) :: i, n, flag
        class(errors), pointer :: errmgr
        type(errors), target :: deferr

        ! Initialization
        n = size(x)
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Input Checking
        if (size(y) /= n) then
            call report_array_size_mismatch_error(errmgr, &
                "pde_define_y_err_lim", "y", n, size(y))
            return
        end if

        if (size(ymin) /= n) then
            call report_array_size_mismatch_error(errmgr, &
                "pde_define_y_err_lim", "ymin", n, size(ymin))
            return
        end if

        if (size(ymax) /= n) then
            call report_array_size_mismatch_error(errmgr, &
                "pde_define_y_err_lim", "ymax", n, size(ymax))
            return
        end if

        ! Process
        this%m_xBars = .false.
        this%m_yBars = .false.
        this%m_range = .false.
        if (allocated(this%m_data)) deallocate(this%m_data)
        allocate(this%m_data(n, 4), stat = flag)
        if (flag /= 0) then
            call report_memory_error(errmgr, "pde_define_y_err_lim", flag)
            return
        end if
        do i = 1, n
            this%m_data(i, 1) = x(i)
            this%m_data(i, 2) = y(i)
            this%m_data(i, 3) = ymin(i)
            this%m_data(i, 4) = ymax(i)
        end do
        this%m_yBars = .true.
        this%m_range = .true.
    end subroutine
! ------------------------------------------------------------------------------
    subroutine pde_define_xy_err_lim(this, x, y, xmin, xmax, ymin, &
        ymax, err)
        !! Defines the x and y error data.
        class(plot_data_error_bars), intent(inout) :: this
            !! The plot_data_error_bars object.
        real(real64), intent(in), dimension(:) :: x
            !! An N-element array containing the x coordinates of the data.
        real(real64), intent(in), dimension(:) :: y
            !! An N-element array containing the y coordinates of the data.
        real(real64), intent(in), dimension(:) :: xmin
            !! An N-element array containing the minimum x values at each data 
            !! point.
        real(real64), intent(in), dimension(:) :: xmax
            !! An N-element array containing the maximum x values at each data 
            !! point.
        real(real64), intent(in), dimension(:) :: ymin
            !! An N-element array containing the minimum y values at each data 
            !! point.
        real(real64), intent(in), dimension(:) :: ymax
            !! An N-element array containing the maximum x values at each data 
            !! point.
        class(errors), intent(inout), optional, target :: err
            !! An error handling object.

        ! Local Variables
        integer(int32) :: i, n, flag
        class(errors), pointer :: errmgr
        type(errors), target :: deferr

        ! Initialization
        n = size(x)
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Input Checking
        if (size(y) /= n) then
            call report_array_size_mismatch_error(errmgr, &
                "pde_define_xy_err_lim", "y", n, size(y))
            return
        end if

        if (size(xmin) /= n) then
            call report_array_size_mismatch_error(errmgr, &
                "pde_define_xy_err_lim", "xmin", n, size(xmin))
            return
        end if

        if (size(xmax) /= n) then
            call report_array_size_mismatch_error(errmgr, &
                "pde_define_xy_err_lim", "xmax", n, size(xmax))
            return
        end if

        if (size(ymin) /= n) then
            call report_array_size_mismatch_error(errmgr, &
                "pde_define_xy_err_lim", "ymin", n, size(ymin))
            return
        end if

        if (size(ymax) /= n) then
            call report_array_size_mismatch_error(errmgr, &
                "pde_define_xy_err_lim", "ymax", n, size(ymax))
            return
        end if

        ! Process
        this%m_xBars = .false.
        this%m_yBars = .false.
        this%m_range = .false.
        if (allocated(this%m_data)) deallocate(this%m_data)
        allocate(this%m_data(n, 6), stat = flag)
        if (flag /= 0) then
            call report_memory_error(errmgr, "pde_define_xy_err_lim", flag)
            return
        end if
        do i = 1, n
            this%m_data(i, 1) = x(i)
            this%m_data(i, 2) = y(i)
            this%m_data(i, 3) = xmin(i)
            this%m_data(i, 4) = xmax(i)
            this%m_data(i, 5) = ymin(i)
            this%m_data(i, 6) = ymax(i)
        end do
        this%m_xBars = .true.
        this%m_yBars = .true.
        this%m_range = .true.
    end subroutine

! ------------------------------------------------------------------------------
end module
