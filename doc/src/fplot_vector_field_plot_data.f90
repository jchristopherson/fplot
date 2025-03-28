! fplot_vector_field_plot_data.f90
! REF:
    ! http://www.gnuplotting.org/vector-field-from-data-file/
    ! http://gnuplot.sourceforge.net/demo_5.4/vector.html
    ! http://www.gnuplot.info/docs_5.4/Gnuplot_5_4.pdf (pg 79)

module fplot_vector_field_plot_data
    use iso_fortran_env
    use fplot_plot_data
    use fplot_errors
    use fplot_colors
    use ferror
    use strings
    implicit none
    private
    public :: vector_field_plot_data

    type, extends(plot_data_colored) :: vector_field_plot_data
        !! Defines a two-dimensional vector-field plot data set.
        real(real64), private, allocatable, dimension(:,:,:) :: m_data
            !! An M-by-N-by-4 array containing the x, y, dx, and dy plot
            !! data points.  Optionally, a 5th page can be added to define the
            !! color for each arrow.
        real(real64), private :: m_arrowSize = 1.0d0
            !! The vector size (scaling factor).
        logical, private :: m_filledHeads = .false.
            !! Fill the arrow heads?
    contains
        procedure, public :: get_data_string => vfpd_get_data_cmd
        procedure, public :: get_command_string => vfpd_get_cmd
        procedure, public :: define_data => vfpd_define_data
        procedure, public :: get_arrow_size => vfpd_get_arrow_size
        procedure, public :: set_arrow_size => vfpd_set_arrow_size
        procedure, public :: get_fill_arrow => vfpd_get_fill_arrow
        procedure, public :: set_fill_arrow => vfpd_set_fill_arrow
        procedure, public :: get_use_data_dependent_colors => &
            vfpd_get_use_data_dependent_colors
    end type

contains
! ------------------------------------------------------------------------------
    function vfpd_get_data_cmd(this) result(x)
        !! Gets the GNUPLOT command string containing the actual data to plot.
        class(vector_field_plot_data), intent(in) :: this
            !! The vector_field_plot_data object.
        character(len = :), allocatable :: x
            !! The command string.

        ! Local Variables
        type(string_builder) :: str
        integer(int32) :: i, j, m, n
        character :: delimiter, nl
        real(real64) :: scaling

        ! Initialization
        call str%initialize()
        delimiter = achar(9)    ! tab delimiter
        nl = new_line(nl)
        scaling = this%get_arrow_size()
        
        ! Fix later
        m = size(this%m_data, 1)
        n = size(this%m_data, 2)

        ! Need a quick return in the event no data exists

        ! Process
        if (this%get_use_data_dependent_colors()) then
            do j = 1, n
                do i = 1, m
                    ! ORDER: X, Y, DX, DY
                    call str%append(to_string(this%m_data(i,j,1)))
                    call str%append(delimiter)
                    call str%append(to_string(this%m_data(i,j,2)))
                    call str%append(delimiter)
                    call str%append(to_string(scaling * this%m_data(i,j,3)))
                    call str%append(delimiter)
                    call str%append(to_string(scaling * this%m_data(i,j,4)))
                    call str%append(delimiter)
                    call str%append(to_string(this%m_data(i,j,5)))
                    call str%append(nl)
                end do
            end do
        else
            do j = 1, n
                do i = 1, m
                    ! ORDER: X, Y, DX, DY
                    call str%append(to_string(this%m_data(i,j,1)))
                    call str%append(delimiter)
                    call str%append(to_string(this%m_data(i,j,2)))
                    call str%append(delimiter)
                    call str%append(to_string(scaling * this%m_data(i,j,3)))
                    call str%append(delimiter)
                    call str%append(to_string(scaling * this%m_data(i,j,4)))
                    call str%append(nl)
                end do
            end do
        end if

        ! End
        x = char(str%to_string())
    end function

! ------------------------------------------------------------------------------
    function vfpd_get_cmd(this) result(x)
        !! Gets the GNUPLOT command string to represent this
        !! vector_field_plot_data object.
        class(vector_field_plot_data), intent(in) :: this
            !! The vector_field_plot_data object.
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

        ! Property Definition
        call str%append(" with vectors")

        if (this%get_fill_arrow()) then
            call str%append(" filled head")
        end if

        if (this%get_use_data_dependent_colors()) then
            call str%append(" lc palette")
        else
            clr = this%get_line_color()
            call str%append(' lc rgb "#')
            call str%append(clr%to_hex_string())
            call str%append('"')
        end if

        ! End
        x = char(str%to_string())
    end function

! ------------------------------------------------------------------------------
    subroutine vfpd_define_data(this, x, y, dx, dy, c, err)
        !! Defines the data set.
        class(vector_field_plot_data), intent(inout) :: this
            !! The vector_field_plot_data object.
        real(real64), intent(in), dimension(:,:) :: x
            !! An M-by-N matrix containing the x-locations of each arrow's 
            !! origin.
        real(real64), intent(in), dimension(:,:) :: y
            !! An M-by-N matrix containing the y-locations of each arrow's 
            !! origin.
        real(real64), intent(in), dimension(:,:) :: dx
            !! An M-by-N matrix containing the x-direction of each arrow.
        real(real64), intent(in), dimension(:,:) :: dy
            !! An M-by-N matrix containing the y-direction of each arrow.
        real(real64), intent(in), dimension(:,:), optional :: c
            !! An optional M-by-N matrix containing information on how to color 
            !! the arrows.  The colors are determined by the active colormap.
        class(errors), intent(inout), optional, target :: err
            !! An error handling object.

        ! Local Variables
        integer(int32) :: i, j, m, n, flag
        type(errors), target :: deferr
        class(errors), pointer :: errmgr
        character(len = 256) :: errmsg

        ! Set up error handling
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Input Checking
        m = size(x, 1)
        n = size(x, 2)
        if (size(y, 1) /= m .or. size(y, 2) /= n) then
            call report_matrix_size_mismatch_error(errmgr, "vfpd_define_data", &
                "y", m, n, size(y, 1), size(y, 2))
            return
        end if
        if (size(dx, 1) /= m .or. size(dx, 2) /= n) then
            call report_matrix_size_mismatch_error(errmgr, "vfpd_define_data", &
                "dx", m, n, size(dx, 1), size(dx, 2))
            return
        end if
        if (size(dy, 1) /= m .or. size(dy, 2) /= n) then
            call report_matrix_size_mismatch_error(errmgr, "vfpd_define_data", &
                "dy", m, n, size(dy, 1), size(dy, 2))
            return
        end if
        if (present(c)) then
            if (size(c, 1) /= m .or. size(c, 2) /= n) then
                call report_matrix_size_mismatch_error(errmgr, &
                    "vfpd_define_data", "c", m, n, size(c, 1), size(c, 2))
                return
            end if
        end if

        ! Allocate space for the data
        if (allocated(this%m_data)) deallocate(this%m_data)
        if (present(c)) then
            allocate(this%m_data(m, n, 5), stat = flag)
        else
            allocate(this%m_data(m, n, 4), stat = flag)
        end if
        if (flag /= 0) then
            call report_memory_error(errmgr, "vfpd_define_data", flag)
            return
        end if

        ! Store the data
        if (present(c)) then
            do concurrent(j = 1:n)
                do i = 1, m
                    this%m_data(i,j,1) = x(i,j)
                    this%m_data(i,j,2) = y(i,j)
                    this%m_data(i,j,3) = dx(i,j)
                    this%m_data(i,j,4) = dy(i,j)
                    this%m_data(i,j,5) = c(i,j)
                end do
            end do
        else
            do concurrent(j = 1:n)
                do i = 1, m
                    this%m_data(i,j,1) = x(i,j)
                    this%m_data(i,j,2) = y(i,j)
                    this%m_data(i,j,3) = dx(i,j)
                    this%m_data(i,j,4) = dy(i,j)
                end do
            end do
        end if

        ! End
        return
    end subroutine

! ------------------------------------------------------------------------------
    pure function vfpd_get_arrow_size(this) result(rst)
        !! Gets the scaling factor used to determine the arrow size.
        class(vector_field_plot_data), intent(in) :: this
            !! The vector_field_plot_data object.
        real(real64) :: rst
            !! The scaling factor.
        rst = this%m_arrowSize
    end function

! --------------------
    subroutine vfpd_set_arrow_size(this, x)
        !! Sets the scaling factor used to determine the arrow size.
        class(vector_field_plot_data), intent(inout) :: this
            !! The vector_field_plot_data object.
        real(real64), intent(in) :: x
            !! The scaling factor.
        this%m_arrowSize = x
    end subroutine

! ------------------------------------------------------------------------------
    pure function vfpd_get_fill_arrow(this) result(rst)
        !! Gets a value determining if the arrow heads should be filled.
        class(vector_field_plot_data), intent(in) :: this
            !! The vector_field_plot_data object.
        logical :: rst
            !! True if the arrow heads should be filled; else, false.
        rst = this%m_filledHeads
    end function

! --------------------
    subroutine vfpd_set_fill_arrow(this, x)
        !! Sets a value determining if the arrow heads should be filled.
        class(vector_field_plot_data), intent(inout) :: this
            !! The vector_field_plot_data object.
        logical, intent(in) :: x
            !! True if the arrow heads should be filled; else, false.
        this%m_filledHeads = x
    end subroutine

! ------------------------------------------------------------------------------
    pure function vfpd_get_use_data_dependent_colors(this) result(rst)
        !! Gets a value indicating if data-dependent coloring should be
        !! used.  This is defined by supplying information on how to scale the
        !! coloring when calling define_data.
        class(vector_field_plot_data), intent(in) :: this
            !! The vector_field_plot_data object.
        logical :: rst
            !! Returns true if data-dependent coloring is being used; else,
            !! false.
        rst = .false.
        if (.not.allocated(this%m_data)) return
        rst = size(this%m_data, 3) >= 5
    end function

! ------------------------------------------------------------------------------
end module
