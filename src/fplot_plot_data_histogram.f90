! fplot_plot_data_histogram.f90

submodule (fplot_core) fplot_plot_data_histogram
contains
! ------------------------------------------------------------------------------
pure module function pdh_get_bin_count(this) result(x)
    class(plot_data_histogram), intent(in) :: this
    integer(int32) :: x
    x = this%m_binCount
end function

! ------------------------------------------------------------------------------
module subroutine pdh_set_bin_count(this, x)
    class(plot_data_histogram), intent(inout) :: this
    integer(int32), intent(in) :: x
    this%m_binCount = x
end subroutine

! ------------------------------------------------------------------------------
module function pdh_bin_data(this, x, err) result(bx)
    ! Arguments
    class(plot_data_histogram), intent(in) :: this
    real(real64), intent(in), dimension(:) :: x
    class(errors), intent(inout), optional, target :: err
    real(real64), allocatable, dimension(:,:) :: bx

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

    ! Allocate space for the output
    allocate(bx(nbins, 2), stat = flag)
    if (flag == 0) allocate(ranges(nbins, 2), stat = flag)
    if (flag /= 0) then
        call errmgr%report_error("pdh_bin_data", &
            "Insufficient memory available.", PLOT_OUT_OF_MEMORY_ERROR)
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
pure module function pdh_get_extremes(this) result(x)
    ! Arguments
    class(plot_data_histogram), intent(in) :: this
    real(real64), dimension(2) :: x

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
module subroutine pdh_set_data_1(this, x, err)
    ! Arguments
    class(plot_data_histogram), intent(inout) :: this
    real(real64), intent(in), dimension(:) :: x
    class(errors), intent(inout), optional, target :: err

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
module subroutine pdh_set_data_2(this, labels, x, err)
    ! Arguments
    class(plot_data_histogram), intent(inout) :: this
    class(string), intent(in), dimension(:) :: labels
    real(real64), intent(in), dimension(:) :: x
    class(errors), intent(inout), optional, target :: err

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

    ! Ensure the labels array is the same size as the number of bins
    if (size(labels) /= this%get_bin_count()) then
        call errmgr%report_error("pdh_set_data_2", &
            "The labels array must be the same size as the number of bins.", &
            PLOT_ARRAY_SIZE_MISMATCH_ERROR)
        return
    end if

    ! Call the base routine to store the data
    call this%plot_data_bar%set_data_2(labels, bx(:,1), errmgr)
end subroutine

! ------------------------------------------------------------------------------
module subroutine pdh_set_data_3(this, labels, x, fmt, err)
    ! Arguments
    class(plot_data_histogram), intent(inout) :: this
    real(real64), intent(in), dimension(:) :: labels
    real(real64), intent(in), dimension(:) :: x
    character(len = *), intent(in), optional :: fmt
    class(errors), intent(inout), optional, target :: err

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

    ! Ensure the labels array is the same size as the number of bins
    if (size(labels) /= this%get_bin_count()) then
        call errmgr%report_error("pdh_set_data_3", &
            "The labels array must be the same size as the number of bins.", &
            PLOT_ARRAY_SIZE_MISMATCH_ERROR)
        return
    end if

    ! Call the base routine to store the data
    call this%plot_data_bar%set_data_3(labels, bx(:,1), fmt, errmgr)
end subroutine

! ------------------------------------------------------------------------------
pure module function pdh_get_num_fmt(this) result(x)
    class(plot_data_histogram), intent(in) :: this
    character(len = :), allocatable :: x
    if (allocated(this%m_numberFmt)) then
        x = this%m_numberFmt
    else
        x = "F6.2"
    end if
end function

! ------------------------------------------------------------------------------
module subroutine pdh_set_num_fmt(this, x)
    class(plot_data_histogram), intent(inout) :: this
    character(len = *), intent(in) :: x
    this%m_numberFmt = x
end subroutine

! ------------------------------------------------------------------------------
end submodule
