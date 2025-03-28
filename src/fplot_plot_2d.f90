! fplot_plot_2d.f90
module fplot_plot_2d
    use iso_fortran_env
    use fplot_plot_data
    use fplot_plot
    use fplot_errors
    use fplot_plot_axis
    use fplot_legend
    use ferror
    use strings
    implicit none
    private
    public :: plot_2d

    type, extends(plot) :: plot_2d
        !! A plot object defining a 2D plot.
        type(x_axis), private, pointer :: m_xAxis => null()
            !! The x-axis.
        type(y_axis), private, pointer :: m_yAxis => null()
            !! The y-axis.
        type(y2_axis), private, pointer :: m_y2Axis => null()
            !! The secondary y-axis.
        logical, private :: m_useY2 = .false.
            !! Display the secondary y axis?
        logical, private :: m_set2square = .false.
            !! Set to square scaling.
    contains
        final :: p2d_clean_up
        procedure, public :: initialize => p2d_init
        procedure, public :: get_command_string => p2d_get_cmd
        procedure, public :: get_x_axis => p2d_get_x_axis
        procedure, public :: get_y_axis => p2d_get_y_axis
        procedure, public :: get_y2_axis => p2d_get_y2_axis
        procedure, public :: get_use_y2_axis => p2d_get_use_y2
        procedure, public :: set_use_y2_axis => p2d_set_use_y2
        procedure, public :: get_square_axes => p2d_get_square_axes
        procedure, public :: set_square_axes => p2d_set_square_axes
    end type

contains
! ------------------------------------------------------------------------------
    subroutine p2d_clean_up(this)
        !! Cleans up resources held by the plot_2d object.
        type(plot_2d), intent(inout) :: this
            !! The plot_2d object.
        call this%free_resources()
        if (associated(this%m_xAxis)) then
            deallocate(this%m_xAxis)
            nullify(this%m_xAxis)
        end if
        if (associated(this%m_yAxis)) then
            deallocate(this%m_yAxis)
            nullify(this%m_yAxis)
        end if
        if (associated(this%m_y2Axis)) then
            deallocate(this%m_y2Axis)
            nullify(this%m_y2Axis)
        end if
    end subroutine

! ------------------------------------------------------------------------------
    subroutine p2d_init(this, term, fname, err)
        !! Initializes the plot_2d object.
        class(plot_2d), intent(inout) :: this
            !! The plot_2d object.
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
        if (flag == 0 .and. .not.associated(this%m_y2Axis)) then
            allocate(this%m_y2Axis, stat = flag)
        end if

        ! Error Checking
        if (flag /= 0) then
            call report_memory_error(errmgr, "p2d_init", flag)
            return
        end if
    end subroutine

! ------------------------------------------------------------------------------
    function p2d_get_cmd(this) result(x)
        !! Gets the GNUPLOT command string to represent this plot_2d object.
        class(plot_2d), intent(in) :: this
            !! The plot_2d object.
        character(len = :), allocatable :: x
            !! The command string.

        ! Local Variables
        type(string_builder) :: str
        integer(int32) :: i, n
        class(plot_data), pointer :: ptr
        class(plot_axis), pointer :: axis, xAxis, yAxis
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

        ! Secondary Axes
        if (this%get_use_y2_axis()) then
            call str%append(new_line('a'))
            axis => this%get_y2_axis()
            if (associated(axis)) then
                call str%append(axis%get_command_string())
                call str%append(new_line('a'))
                call str%append("set y2tics")
                call str%append(new_line('a'))
                call str%append("set ytics nomirror")
            end if
        end if

        ! Tic Marks
        if (.not.this%get_tics_inward()) then
            call str%append(new_line('a'))
            call str%append("set tics out")
        end if
        if (xAxis%get_zero_axis()) then
            call str%append(new_line('a'))
            call str%append("set xtics axis")
        end if
        if (yAxis%get_zero_axis()) then
            call str%append(new_line('a'))
            call str%append("set ytics axis")
        end if


        ! Border
        call str%append(new_line('a'))
        call str%append("set border back")

        if (this%get_draw_border()) then
            n = 31
        else
            n = 0
            if (.not.xAxis%get_zero_axis()) n = n + 1
            if (.not.yAxis%get_zero_axis()) n = n + 2

            call str%append(new_line('a'))
            call str%append("set xtics nomirror")
            call str%append(new_line('a'))
            call str%append("set ytics nomirror")

            if (this%get_use_y2_axis()) then
                n = n + 8
            end if
        end if

        call str%append(new_line('a'))
        if (n > 0) then
            call str%append("set border ")
            call str%append(to_string(n))
        else
            call str%append("unset border")
        end if

        ! Scaling
        if (this%get_axis_equal()) then
            call str%append(new_line('a'))
            call str%append("set view equal xy")
        end if

        if (this%get_square_axes()) then
            call str%append(new_line('a'))
            call str%append("set size square")
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
            ! if (i /= n) then
            !     call str%append("e")
            ! end if
        end do

        ! End
        x = char(str%to_string())
    end function

! ------------------------------------------------------------------------------
    function p2d_get_x_axis(this) result(ptr)
        !! Gets the x-axis object.
        class(plot_2d), intent(in) :: this
            !! The plot_2d object.
        class(plot_axis), pointer :: ptr
            !! A pointer to the x-axis object.
        ptr => this%m_xAxis
    end function

! ------------------------------------------------------------------------------
    function p2d_get_y_axis(this) result(ptr)
        !! Gets the y-axis object.
        class(plot_2d), intent(in) :: this
            !! The plot_2d object.
        class(plot_axis), pointer :: ptr
            !! A pointer to the y-axis object.
        ptr => this%m_yAxis
    end function

! ------------------------------------------------------------------------------
    function p2d_get_y2_axis(this) result(ptr)
        !! Gets the secondary y-axis object.
        class(plot_2d), intent(in) :: this
            !! The plot_2d object.
        class(plot_axis), pointer :: ptr
            !! A pointer to the secondary y-axis object.
        ptr => this%m_y2Axis
    end function

! ------------------------------------------------------------------------------
    pure function p2d_get_use_y2(this) result(x)
        !! Gets a flag determining if the secondary y-axis should be
        !! displayed.
        class(plot_2d), intent(in) :: this
            !! The plot_2d object.
        logical :: x
            !! Returns true if the axis should be displayed; else, false.
        x = this%m_useY2
    end function

! --------------------
    subroutine p2d_set_use_y2(this, x)
        !! Sets a flag determining if the secondary y-axis should be
        !! displayed.
        class(plot_2d), intent(inout) :: this
            !! The plot_2d object.
        logical, intent(in) :: x
            !! Set to true if the axis should be displayed; else, false.
        this%m_useY2 = x
    end subroutine

! ------------------------------------------------------------------------------
    pure function p2d_get_square_axes(this) result(rst)
        !! Gets a logical flag determining if the axes size should be squared
        !! off.
        class(plot_2d), intent(in) :: this
            !! The plot_2d object.
        logical :: rst
            !! Returns true if the axes are to be sized to a square; else,
            !! false.
        rst = this%m_set2square
    end function

! --------------------
    subroutine p2d_set_square_axes(this, x)
        !! Sets a logical flag determining if the axes size should be
        !! squared off.
        class(plot_2d), intent(inout) :: this
            !! The plot_2d object.
        logical, intent(in) :: x
            !! Set to true if the axes are to be sized to a square; else,
            !! false.
        this%m_set2square = x
    end subroutine

! ------------------------------------------------------------------------------
end module
