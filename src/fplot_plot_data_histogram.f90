! fplot_plot_data_histogram.f90

module fplot_plot_data_histogram
    use iso_fortran_env
    use fplot_plot_data_bar
    use fplot_errors
    use ferror
    use strings
    implicit none
    private
    public :: plot_data_histogram

    type, extends(plot_data_bar) :: plot_data_histogram
        !! A container for plotting data in the form of a histogram.
        integer(int32), private :: m_binCount = 20
            !! The number of bins.
        character(len = :), private, allocatable :: m_numberFmt
            !! The numerical label format string.
        logical, private :: m_useTicLabels = .false.
            !! Flag to indicate if user-defind tic labels are used.  This
            !! overrides the base class behavior.
        real(real64), private :: m_minX
            !! The minimum data value.
        real(real64), private :: m_maxX
            !! The maximum data value.
    contains
        procedure, public :: get_bin_count => pdh_get_bin_count
        procedure, public :: set_bin_count => pdh_set_bin_count
        procedure, public :: bin_data => pdh_bin_data
        procedure, public :: get_extreme_values => pdh_get_extremes
        procedure, public :: get_number_format => pdh_get_num_fmt
        procedure, public :: set_number_format => pdh_set_num_fmt
        procedure, public :: set_data_1 => pdh_set_data_1
        procedure, public :: set_data_2 => pdh_set_data_2
        procedure, public :: set_data_3 => pdh_set_data_3
        procedure, public :: get_use_labels => pdh_get_use_labels
        procedure, public :: set_use_labels => pdh_set_use_labels
        procedure, public :: get_minimum_value => pdh_get_min_x
        procedure, public :: get_maximum_value => pdh_get_max_x
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
function pdh_bin_data(this, x, err) result(bx)
    !! Bins the supplied data set.
    class(plot_data_histogram), intent(inout) :: this
        !! The plot_data_histogram object.
    real(real64), intent(in), dimension(:) :: x
        !! The data set to bin.
    class(errors), intent(inout), optional, target :: err
        !! An error handling object.
    real(real64), allocatable, dimension(:,:) :: bx
        !! The binned data.

    ! Local Variables
    real(real64) :: maxX, minX, width, val
    integer(int32) :: i, j, flag, n, nbins
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
    nbins = this%get_bin_count()

    ! Get the max and min of the entire data set
    maxX = maxval(x)
    minX = minval(x)
    width = (maxX - minX) / (nbins - 1.0)
    this%m_minX = minX
    this%m_maxX = maxX

    ! Allocate space for the output
    allocate(bx(nbins, 2), stat = flag)
    if (flag == 0) allocate(ranges(nbins, 2), stat = flag)
    if (flag /= 0) then
        call report_memory_error(errmgr, "pdh_bin_data", flag)
        return
    end if
    bx = 0.0d0

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
                bx(j,1) = bx(j,1) + 1.0d0   ! Counter
                exit    ! Exit the inner do loop
            end if
        end do
    end do

    ! Now compute the center of each bin - store in column 2 of bx
    bx(:,2) = 0.5d0 * (ranges(:,1) + ranges(:,2))
end function

! ------------------------------------------------------------------------------
pure function pdh_get_extremes(this) result(x)
    !! Returns the extreme values in the data set.
    class(plot_data_histogram), intent(in) :: this
        !! The plot_data_histogram object.
    real(real64), dimension(2) :: x
        !! A two-element array containing the minimum and maximum values, in 
        !! that order.

    ! Local Variables
    integer(int32) :: i, j, nrows, ncols
    real(real64) :: maxX, minX, val
    logical :: check

    ! Initialization
    nrows = this%get_count()
    ncols = this%get_bar_per_label_count()
    check = .true.

    ! Process
    do j = 1, ncols
        do i = 1, nrows
            val = this%get(i, j)
            if (check) then
                maxX = val
                minX = val
                check = .false.
            else
                if (val > maxX) maxX = val
                if (val < minX) minX = val
            end if
        end do
    end do

    ! End
    x = [minX, maxX]
end function

! ------------------------------------------------------------------------------
subroutine pdh_set_data_1(this, x, err)
    !! Defines the data set.
    class(plot_data_histogram), intent(inout) :: this
        !! The plot_data_histogram object.
    real(real64), intent(in), dimension(:) :: x
        !! The data set.
    class(errors), intent(inout), optional, target :: err
        !! An error handling object.

    ! Local Variables
    real(real64), allocatable, dimension(:,:) :: bx
    class(errors), pointer :: errmgr
    type(errors), target :: deferr
    
    ! Initialization
    if (present(err)) then
        errmgr => err
    else
        errmgr => deferr
    end if

    ! Bin the data
    bx = this%bin_data(x, errmgr)
    if (errmgr%has_error_occurred()) return

    ! Call the base routine to store the data - use the average values to 
    ! establish labels for the x-axis
    call this%plot_data_bar%set_data_3(bx(:,2), bx(:,1), &
        this%get_number_format(), errmgr)
end subroutine

! ------------------------------------------------------------------------------
subroutine pdh_set_data_2(this, labels, x, err)
    !! Defines the data set with associated axis labels.
    class(plot_data_histogram), intent(inout) :: this
        !! The plot_data_histogram object.
    class(string), intent(in), dimension(:) :: labels
        !! The axis labels.
    real(real64), intent(in), dimension(:) :: x
        !! The data set.
    class(errors), intent(inout), optional, target :: err
        !! An error handling object.

    ! Local Variables
    real(real64), allocatable, dimension(:,:) :: bx
    class(errors), pointer :: errmgr
    type(errors), target :: deferr
    
    ! Initialization
    if (present(err)) then
        errmgr => err
    else
        errmgr => deferr
    end if

    ! Bin the data
    bx = this%bin_data(x, errmgr)
    if (errmgr%has_error_occurred()) return

    ! Ensure the labels array is the same size as the number of bins
    if (size(labels) /= this%get_bin_count()) then
        call report_array_size_mismatch_error(errmgr, "pdh_set_data_2", &
            "labels", this%get_bin_count(), size(labels))
        return
    end if

    ! Call the base routine to store the data
    call this%plot_data_bar%set_data_2(labels, bx(:,1), errmgr)
end subroutine

! ------------------------------------------------------------------------------
subroutine pdh_set_data_3(this, labels, x, fmt, err)
    !! Defines the data set with associated axis labels with a specific format.
    class(plot_data_histogram), intent(inout) :: this
        !! The plot_data_histogram object.
    real(real64), intent(in), dimension(:) :: labels
        !! The axis labels.
    real(real64), intent(in), dimension(:) :: x
        !! The data set.
    character(len = *), intent(in), optional :: fmt
        !! The format string for the labels (e.g. '(I0)', etc.).
    class(errors), intent(inout), optional, target :: err
        !! An error handling object.

    ! Local Variables
    real(real64), allocatable, dimension(:,:) :: bx
    class(errors), pointer :: errmgr
    type(errors), target :: deferr
    
    ! Initialization
    if (present(err)) then
        errmgr => err
    else
        errmgr => deferr
    end if

    ! Bin the data
    bx = this%bin_data(x, errmgr)
    if (errmgr%has_error_occurred()) return

    ! Ensure the labels array is the same size as the number of bins
    if (size(labels) /= this%get_bin_count()) then
        call report_array_size_mismatch_error(errmgr, "pdh_set_data_3", &
            "labels", this%get_bin_count(), size(labels))
        return
    end if

    ! Call the base routine to store the data
    call this%plot_data_bar%set_data_3(labels, bx(:,1), fmt, errmgr)
end subroutine

! ------------------------------------------------------------------------------
pure function pdh_get_num_fmt(this) result(x)
    !! Gets the numerical format string used for the labels.
    class(plot_data_histogram), intent(in) :: this
        !! The plot_data_histogram object.
    character(len = :), allocatable :: x
        !! The format string.
    if (allocated(this%m_numberFmt)) then
        x = this%m_numberFmt
    else
        x = "(F6.2)"
    end if
end function

! ------------------------------------------------------------------------------
subroutine pdh_set_num_fmt(this, x)
    !! Sets the numerical format string used for the labels.
    class(plot_data_histogram), intent(inout) :: this
        !! The plot_data_histogram object.
    character(len = *), intent(in) :: x
        !! The format string (e.g. "(F6.2)").
    this%m_numberFmt = x
end subroutine

! ------------------------------------------------------------------------------
pure function pdh_get_use_labels(this) result(x)
    !! Gets the flag indicating if user-defined tic labels are used.
    class(plot_data_histogram), intent(in) :: this
        !! The plot_data_histogram object.
    logical :: x
        !! The flag.
    x = this%m_useTicLabels
end function

! --------------------
subroutine pdh_set_use_labels(this, x)
    !! Sets the flag indicating if user-defined tic labels are used.
    class(plot_data_histogram), intent(inout) :: this
        !! The plot_data_histogram object.
    logical, intent(in) :: x
        !! The flag.
    this%m_useTicLabels = x
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
end module
