! fplot_plot_data_histogram.f90

module fplot_plot_data_histogram
    use iso_fortran_env
    use fplot_plot_data
    use fplot_errors
    use ferror
    use strings
    use fplot_colors
    use fplot_errors
    implicit none
    private
    public :: plot_data_histogram

    type, extends(plot_data_colored) :: plot_data_histogram
        !! A container for plotting data in the form of a histogram.
        integer(int32), private :: m_binCount = 20
            !! The number of bins.
        real(real64), private :: m_minX
            !! The minimum data value.
        real(real64), private :: m_maxX
            !! The maximum data value.
        real(real64), private, allocatable, dimension(:,:) :: m_data
            !! Column 1 is the center of each bin and column 2 is the number
            !! of items in each bin.
        logical, private :: m_filled = .true.
            !! Determines if each bar is filled.
        logical, private :: m_useY2 = .false.
            !! Draw against the secondary y axis?
    contains
        procedure, public :: get_bin_count => pdh_get_bin_count
        procedure, public :: set_bin_count => pdh_set_bin_count
        procedure, public :: get_minimum_value => pdh_get_min_x
        procedure, public :: get_maximum_value => pdh_get_max_x
        procedure, public :: define_data => pdh_define_data
        procedure, public :: get_command_string => pdh_get_cmd
        procedure, public :: get_data_string => pdh_get_data_cmd
        procedure, public :: get_axes_string => pdh_get_axes_cmd
        procedure, public :: get_is_filled => pdh_get_is_filled
        procedure, public :: set_is_filled => pdh_set_is_filled
        procedure, public :: get_draw_against_y2 => pdh_get_use_y2
        procedure, public :: set_draw_against_y2 => pdh_set_use_y2
        procedure, public :: get => pdh_get_bin_data
    end type

contains
! ------------------------------------------------------------------------------
pure function pdh_get_bin_count(this) result(x)
    !! Gets the number of bins.
    class(plot_data_histogram), intent(in) :: this
        !! The plot_data_histogram object.
    integer(int32) :: x
        !! The bin count.
    x = this%m_binCount
end function

! ------------------------------------------------------------------------------
subroutine pdh_set_bin_count(this, x)
    !! Sets the bin count.  For this property to have an effect, call before
    !! calling the define_data subroutine or bin_data subroutine.
    class(plot_data_histogram), intent(inout) :: this
        !! The plot_data_histogram object.
    integer(int32), intent(in) :: x
        !! The bin count.
    this%m_binCount = x
end subroutine

! ------------------------------------------------------------------------------
pure function pdh_get_min_x(this) result(x)
    !! Gets the minimum data value.
    class(plot_data_histogram), intent(in) :: this
        !! The plot_data_histogram object.
    real(real64) :: x
        !! The minimum data value.
    x = this%m_minX
end function

! ------------------------------------------------------------------------------
pure function pdh_get_max_x(this) result(x)
    !! Gets the maximum data value.
    class(plot_data_histogram), intent(in) :: this
        !! The plot_data_histogram object.
    real(real64) :: x
        !! The maximum data value.
    x = this%m_maxX
end function

! ------------------------------------------------------------------------------
subroutine pdh_define_data(this, x, err)
    !! Defines the data set to plot.
    class(plot_data_histogram), intent(inout) :: this
        !! The plot_data_histogram object.
    real(real64), intent(in), dimension(:) :: x
        !! The data set to plot.
    class(errors), intent(inout), optional, target :: err
        !! An error handling object.

    ! Local Variables
    integer(int32) :: i, j, n, nbins, flag
    real(real64) :: maxX, minX, width, val
    real(real64), allocatable, dimension(:,:) :: ranges
    class(errors), pointer :: errmgr
    type(errors), target :: deferr
    
    ! Initialization
    if (present(err)) then
        errmgr => err
    else
        errmgr => deferr
    end if
    n = size(x)
    nbins = min(n, this%get_bin_count()) ! protects against the case where nbins > n however unlikely

    ! Get the max and min of the entire data set
    maxX = maxval(x)
    minX = minval(x)
    width = (maxX - minX) / (nbins - 1.0)
    this%m_minX = minX
    this%m_maxX = maxX

    ! Allocate space for the output
    if (allocated(this%m_data)) deallocate(this%m_data)
    allocate(this%m_data(nbins, 2), stat = flag, source = 0.0d0)
    if (flag == 0) allocate(ranges(nbins, 2), stat = flag)
    if (flag /= 0) then
        call report_memory_error(errmgr, "pdh_define_data", flag)
        return
    end if

    ! Define each range
    ranges(1,:) = [minX, minX + width]
    do i = 2, nbins
        ranges(i,1) = ranges(i-1,2)
        ranges(i,2) = ranges(i,1) + width
    end do

    ! Construct the bins
    do i = 1, n
        val = x(i)
        do j = 1, nbins
            if ((val >= ranges(j,1)) .and. (val <= ranges(j,2))) then
                this%m_data(j,1) = this%m_data(j,1) + 1.0d0   ! Counter
                exit    ! Exit the inner do loop
            end if
        end do
    end do

    ! Now compute the center of each bin - store in column 2 of this%m_data
    this%m_data(:,2) = 0.5d0 * (ranges(:,1) + ranges(:,2))
end subroutine

! ------------------------------------------------------------------------------
function pdh_get_cmd(this) result(rst)
    !! Gets the GNUPLOT command string for this object.
    class(plot_data_histogram), intent(in) :: this
        !! The plot_data_histogram object.
    character(len = :), allocatable :: rst
        !! The command string.

    ! Local Variables
    type(string_builder) :: str
    integer(int32) :: n, ncols
    type(color) :: clr

    ! Process
    call str%append(' "-" ')
    call str%append(" with boxes ")

    ! Color
    clr = this%get_line_color()
    call str%append(' lc rgb "#')
    call str%append(clr%to_hex_string())
    call str%append('"')

    ! Filled
    if (this%get_is_filled()) then
        call str%append(" fill solid ")
    else
        call str%append(" fill empty ")
    end if

    ! Define the axes structure
    call str%append(" ")
    call str%append(this%get_axes_string())

    ! End
    rst = char(str%to_string())
end function

! ------------------------------------------------------------------------------
function pdh_get_data_cmd(this) result(rst)
    !! Gets the GNUPLOT command string defining the data for this object.
    class(plot_data_histogram), intent(in) :: this
        !! The plot_data_histogram object.
    character(len = :), allocatable :: rst
        !! The command string.

    ! Local Variables
    type(string_builder) :: str
    integer(int32) :: i, nbars, cnt
    real(real64) :: val
    character :: delimiter, nl

    ! Initialization
    delimiter = achar(9)
    nl = new_line(nl)
    nbars = size(this%m_data, 1)

    ! Process
    do i = 1, nbars
        call this%get(i, val, cnt)
        call str%append(to_string(val))
        call str%append(delimiter)
        call str%append(to_string(cnt))
        call str%append(nl)
    end do

    ! End
    rst = char(str%to_string())
end function

! ------------------------------------------------------------------------------
function pdh_get_axes_cmd(this) result(rst)
    !! Gets the GNUPLOT command string defining which axes the data is to be
    !! plotted against.
    class(plot_data_histogram), intent(in) :: this
        !! The plot_data_histogram object.
    character(len = :), allocatable :: rst
        !! The command string.

    ! Define which axes the data is to be plotted against
    if (this%get_draw_against_y2()) then
        rst = "axes x1y2"
    else
        rst = "axes x1y1"
    end if
end function

! ------------------------------------------------------------------------------
pure function pdh_get_is_filled(this) result(rst)
    !! Gets a value determining if each box is filled.
    class(plot_data_histogram), intent(in) :: this
        !! The plot_data_histogram object.
    logical :: rst
        !! Returns true if the boxes are filled; else, false for an empty box.
    rst = this%m_filled
end function

! --------------------
subroutine pdh_set_is_filled(this, x)
    !! Sets a value determining if each box is filled.
    class(plot_data_histogram), intent(inout) :: this
        !! The plot_data_histogram object.
    logical, intent(in) :: x
        !! Set to true if the boxes should be filled; else, false for an empty
        !! box.
    this%m_filled = x
end subroutine

! ------------------------------------------------------------------------------
pure function pdh_get_use_y2(this) result(rst)
    !! Gets a value determining if the data is to be plotted against the
    !! secondary y axis.
    class(plot_data_histogram), intent(in) :: this
        !! The plot_data_histogram object.
    logical :: rst
        !! Returns true if the data is to be plotted against the secondary y 
        !! axis; else, false for the primary y axis.
    rst = this%m_useY2
end function

! --------------------
subroutine pdh_set_use_y2(this, x)
    !! Sets a value determining if the data is to be plotted against the
    !! secondary y axis.
    class(plot_data_histogram), intent(inout) :: this
        !! The plot_data_histogram object.
    logical, intent(in) :: x
        !! Set to true if the data is to be plotted against the secondary y 
        !! axis; else, false for the primary y axis.
    this%m_useY2 = x
end subroutine

! ------------------------------------------------------------------------------
subroutine pdh_get_bin_data(this, i, x, cnt)
    !! Gets the requested binned data.
    class(plot_data_histogram), intent(in) :: this
        !! The plot_data_histogram object.
    integer(int32), intent(in) :: i
        !! The bin number to get.
    real(real64), intent(out) :: x
        !! The center of the bin.
    integer(int32), intent(out) :: cnt
        !! The number of items in the bin.

    ! Process
    if (.not.allocated(this%m_data)) then
        cnt = 0
        x = 0.0d0
        return
    end if
    x = this%m_data(i,2)
    cnt = floor(this%m_data(i,1))
end subroutine

! ------------------------------------------------------------------------------
end module
