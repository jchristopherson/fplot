! fplot_plot_data_bar.f90

submodule (fplot_core) fplot_plot_data_bar
contains
! ------------------------------------------------------------------------------
pure module function pdb_get_count(this) result(x)
    class(plot_data_bar), intent(in) :: this
    integer(int32) :: x
    if (allocated(this%m_barData)) then
        x = size(this%m_barData, 1)
    else
        x = 0
    end if
end function

! ------------------------------------------------------------------------------
pure module function pdb_get_data(this, index, col) result(x)
    class(plot_data_bar), intent(in) :: this
    integer(int32), intent(in) :: index, col
    real(real64) :: x
    if (allocated(this%m_barData)) then
        x = this%m_barData(index, col)
    else
        x = 0.0d0
    end if
end function

! ------------------------------------------------------------------------------
module subroutine pdb_set_data(this, index, col, x)
    class(plot_data_bar), intent(inout) :: this
    integer(int32), intent(in) :: index, col
    real(real64), intent(in) :: x
    if (allocated(this%m_barData)) then
        this%m_barData(index, col) = x
    end if
end subroutine

! ------------------------------------------------------------------------------
pure module function pdb_get_data_set(this, col) result(x)
    class(plot_data_bar), intent(in) :: this
    integer(int32), intent(in) :: col
    real(real64), allocatable, dimension(:) :: x
    if (allocated(this%m_barData)) then
        x = this%m_barData(:,col)
    else
        allocate(x(0))
    end if
end function

! ------------------------------------------------------------------------------
pure module function pdb_get_label(this, index) result(x)
    class(plot_data_bar), intent(in) :: this
    integer(int32), intent(in) :: index
    character(len = :), allocatable :: x
    if (allocated(this%m_axisLabels)) then
        x = this%m_axisLabels(index)%str
    else
        x = ""
    end if
end function

! ------------------------------------------------------------------------------
module subroutine pdb_set_label(this, index, txt)
    class(plot_data_bar), intent(inout) :: this
    integer(int32) :: index
    character(len = *), intent(in) :: txt
    if (allocated(this%m_axisLabels)) then
        this%m_axisLabels(index)%str = txt
    end if
end subroutine

! ------------------------------------------------------------------------------
pure module function pdb_get_use_labels(this) result(x)
    class(plot_data_bar), intent(in) :: this
    logical :: x
    x = this%m_useAxisLabels
end function

! ------------------------------------------------------------------------------
module subroutine pdb_set_use_labels(this, x)
    class(plot_data_bar), intent(inout) :: this
    logical, intent(in) :: x
    this%m_useAxisLabels = x
end subroutine

! ------------------------------------------------------------------------------
module function pdb_get_cmd(this) result(x)
    ! Arguments
    class(plot_data_bar), intent(in) :: this
    character(len = :), allocatable :: x

    ! Local Variables
    type(string_builder) :: str
    integer(int32) :: n, ncols
    type(color) :: clr

    ! Initialization
    call str%initialize()

    ! Starting off...
    call str%append(' "-" ')

    ! Tic Labels
    if (this%get_use_labels() .and. allocated(this%m_barData) .and. &
            allocated(this%m_axisLabels)) then
        ncols = size(this%m_barData, 2)
        if (ncols == 1) then
            call str%append(" using 2:xtic(1) ")
        else
            call str%append(" using 2:")
            call str%append(to_string(ncols))
            call str%append(":xtic(1) ")
        end if
    end if

    ! Enforce a box plot
    call str%append(" with boxes ")

    ! Filled?
    if (this%get_is_filled()) then
        call str%append(" fill solid ")
    else
        call str%append(" fill empty ")
    end if

    ! Transparency
    call str%append(to_string(this%get_transparency()))

    ! Title
    n = len_trim(this%get_name())
    if (n > 0) then
        call str%append(' title "')
        call str%append(this%get_name())
        call str%append('"')
    else
        call str%append(' notitle')
    end if

    ! Color
    clr = this%get_line_color()
    call str%append(' lc rgb "#')
    call str%append(clr%to_hex_string())
    call str%append('"')

    ! Define the axes structure
    call str%append(" ")
    call str%append(this%get_axes_string())

    ! End
    x = str%to_string()
end function

! ------------------------------------------------------------------------------
module function pdb_get_data_cmd(this) result(x)
    ! Arguments
    class(plot_data_bar), intent(in) :: this
    character(len = :), allocatable :: x

    ! Local Variables
    type(string_builder) :: str
    integer(int32) :: i, j, nbars, ncols
    character :: delimiter, nl

    ! Initialization
    call str%initialize()
    delimiter = achar(9)
    nl = new_line(nl)
    nbars = this%get_count()
    ncols = this%get_bar_per_label_count()

    ! Process
    if (this%get_use_labels() .and. allocated(this%m_axisLabels) .and. &
            allocated(this%m_barData)) then
        do i = 1, nbars
            call str%append(this%m_axisLabels(i)%str)
            call str%append(delimiter)
            do j = 1, ncols
                call str%append(to_string(this%get(i, j)))
                if (j /= nbars) call str%append(delimiter)
            end do
            call str%append(nl)
        end do
    else
        do i = 1, nbars
            do j = 1, ncols
                call str%append(to_string(this%get(i, j)))
                if (j /= nbars) call str%append(delimiter)
            end do
            call str%append(nl)
        end do
    end if

    ! End
    x = str%to_string()
end function

! ------------------------------------------------------------------------------
module function pdb_get_axes_cmd(this) result(x)
    ! Arguments
    class(plot_data_bar), intent(in) :: this
    character(len = :), allocatable :: x

    ! Define which axes the data is to be plotted against
    if (this%get_draw_against_y2()) then
        x = "axes x1y2"
    else
        x = "axes x1y1"
    end if
end function

! ------------------------------------------------------------------------------
pure module function pdb_get_col_count(this) result(x)
    class(plot_data_bar), intent(in) :: this
    integer(int32) :: x
    if (allocated(this%m_barData)) then
        x = size(this%m_barData, 2)
    else
        x = 0
    end if
end function

! ------------------------------------------------------------------------------
pure module function pdb_get_use_y2(this) result(x)
    class(plot_data_bar), intent(in) :: this
    logical :: x
    x = this%m_useY2
end function

! ------------------------------------------------------------------------------
module subroutine pdb_set_use_y2(this, x)
    class(plot_data_bar), intent(inout) :: this
    logical, intent(in) :: x
    this%m_useY2 = x
end subroutine

! ------------------------------------------------------------------------------
module subroutine pdb_set_data_1(this, x, err)
    ! Arguments
    class(plot_data_bar), intent(inout) :: this
    real(real64), intent(in), dimension(:) :: x
    class(errors), intent(inout), optional, target :: err

    ! Process
    call this%set_data_1(x, err)
end subroutine

! ------------------------------------------------------------------------------
module subroutine pdb_set_data_2(this, labels, x, err)
    ! Arguments
    class(plot_data_bar), intent(inout) :: this
    class(string), intent(in), dimension(:) :: labels
    real(real64), intent(in), dimension(:) :: x
    class(errors), intent(inout), optional, target :: err

    ! Process
    call this%set_data_2(labels, x, err)
end subroutine

! ------------------------------------------------------------------------------
module subroutine pdb_set_data_3(this, labels, x, fmt, err)
    ! Arguments
    class(plot_data_bar), intent(inout) :: this
    real(real64), intent(in), dimension(:) :: labels
    real(real64), intent(in), dimension(:) :: x
    character(len = *), intent(in), optional :: fmt
    class(errors), intent(inout), optional, target :: err

    ! Process
    call this%set_data_3(labels, x, fmt, err)
end subroutine

! ------------------------------------------------------------------------------
pure module function pdb_get_is_filled(this) result(x)
    class(plot_data_bar), intent(in) :: this
    logical :: x
    x = this%m_filled
end function

! ------------------------------------------------------------------------------
module subroutine pdb_set_is_filled(this, x)
    class(plot_data_bar), intent(inout) :: this
    logical, intent(in) :: x
    this%m_filled = x
end subroutine

! ------------------------------------------------------------------------------
pure module function pdb_get_alpha(this) result(x)
    class(plot_data_bar), intent(in) :: this
    real(real32) :: x
    x = this%m_alpha
end function

! ------------------------------------------------------------------------------
module subroutine pdb_set_alpha(this, x)
    class(plot_data_bar), intent(inout) :: this
    real(real32), intent(in) :: x
    if (x > 1.0) then
        this%m_alpha = 1.0
    else if (x < 0.0) then
        this%m_alpha = 0.0
    else
        this%m_alpha = x
    end if
end subroutine

! ------------------------------------------------------------------------------
module subroutine pdb_set_data_1_core(this, x, err)
    ! Arguments
    class(plot_data_bar), intent(inout) :: this
    real(real64), intent(in), dimension(:) :: x
    class(errors), intent(inout), optional, target :: err

    ! Local Variables
    class(errors), pointer :: errmgr
    type(errors), target :: deferr
    integer(int32) :: n, flag
    
    ! Initialization
    if (present(err)) then
        errmgr => err
    else
        errmgr => deferr
    end if
    n = size(x)

    ! Process
    if (allocated(this%m_axisLabels)) deallocate(this%m_axisLabels)
    if (allocated(this%m_barData)) deallocate(this%m_barData)
    allocate(this%m_barData(n, 1), stat = flag)
    if (flag /= 0) then
        call errmgr%report_error("pdb_set_data_1_core", &
            "Insufficient memory available.", PLOT_OUT_OF_MEMORY_ERROR)
        return
    end if
    this%m_barData(:,1) = x
end subroutine

! ------------------------------------------------------------------------------
module subroutine pdb_set_data_2_core(this, labels, x, err)
    ! Arguments
    class(plot_data_bar), intent(inout) :: this
    class(string), intent(in), dimension(:) :: labels
    real(real64), intent(in), dimension(:) :: x
    class(errors), intent(inout), optional, target :: err

    ! Local Variables
    class(errors), pointer :: errmgr
    type(errors), target :: deferr
    integer(int32) :: n, flag
    
    ! Initialization
    if (present(err)) then
        errmgr => err
    else
        errmgr => deferr
    end if
    n = size(x)

    ! Input Check
    if (size(labels) /= n) then
        call errmgr%report_error("pdb_set_data_2_core", &
            "The input arrays are not the same size.", &
            PLOT_ARRAY_SIZE_MISMATCH_ERROR)
        return
    end if

    ! Process
    if (allocated(this%m_axisLabels)) deallocate(this%m_axisLabels)
    if (allocated(this%m_barData)) deallocate(this%m_barData)
    allocate(this%m_barData(n, 1), stat = flag)
    if (flag == 0) allocate(this%m_axisLabels(n), stat = flag)
    if (flag /= 0) then
        call errmgr%report_error("pdb_set_data_2_core", &
            "Insufficient memory available.", PLOT_OUT_OF_MEMORY_ERROR)
        return
    end if
    this%m_barData(:,1) = x
    this%m_axisLabels = labels
end subroutine

! ------------------------------------------------------------------------------
module subroutine pdb_set_data_3_core(this, labels, x, fmt, err)
    ! Arguments
    class(plot_data_bar), intent(inout) :: this
    real(real64), intent(in), dimension(:) :: labels
    real(real64), intent(in), dimension(:) :: x
    character(len = *), intent(in), optional :: fmt
    class(errors), intent(inout), optional, target :: err

    ! Local Variables
    class(errors), pointer :: errmgr
    type(errors), target :: deferr
    integer(int32) :: i, n, flag
    type(string), allocatable, dimension(:) :: lbls
    
    ! Initialization
    if (present(err)) then
        errmgr => err
    else
        errmgr => deferr
    end if
    n = size(x)

    ! Input Check
    if (size(labels) /= n) then
        call errmgr%report_error("pdb_set_data_3_core", &
            "The input arrays are not the same size.", &
            PLOT_ARRAY_SIZE_MISMATCH_ERROR)
        return
    end if

    ! Convert the numeric labels to strings
    allocate(lbls(n), stat = flag)
    if (flag /= 0) then
        call errmgr%report_error("pdb_set_data_3_core", &
            "Insufficient memory available.", PLOT_OUT_OF_MEMORY_ERROR)
        return
    end if
    do i = 1, n
        lbls(i)%str = to_string(labels(i), fmt)
    end do

    ! Store the data
    if (allocated(this%m_axisLabels)) deallocate(this%m_axisLabels)
    if (allocated(this%m_barData)) deallocate(this%m_barData)
    allocate(this%m_barData(n, 1), stat = flag)
    if (flag == 0) allocate(this%m_axisLabels(n), stat = flag)
    if (flag /= 0) then
        call errmgr%report_error("pdb_set_data_3_core", &
            "Insufficient memory available.", PLOT_OUT_OF_MEMORY_ERROR)
        return
    end if
    this%m_barData(:,1) = x
    this%m_axisLabels = lbls
end subroutine

! ------------------------------------------------------------------------------
end submodule
