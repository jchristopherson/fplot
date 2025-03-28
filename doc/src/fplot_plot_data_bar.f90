! fplot_plot_data_bar.f90

module fplot_plot_data_bar
    use iso_fortran_env
    use fplot_plot_data
    use fplot_errors
    use fplot_colors
    use strings
    use ferror
    implicit none
    private
    public :: plot_data_bar

    type, extends(plot_data_colored) :: plot_data_bar
        !! Defines a data set tailored to bar charts.
        type(string), private, allocatable, dimension(:) :: m_axisLabels
            !! An array containing axis labels to associate with each bar.
        real(real64), private, allocatable, dimension(:,:) :: m_barData
            !! An array of data defining each bar - the matrix contains
            !! multiple columns to allow multiple bars per label.
        logical, private :: m_useAxisLabels = .true.
            !! Determines if the axis labels should be used - only applicable
            !! if there is existing data stored in m_axisLabels & m_axisLabels
            !! is the same size as m_barData.
        logical, private :: m_useY2 = .false.
            !! Draw against the secondary y axis?
        logical, private :: m_filled = .true.
            !! Determines if each bar is filled.
        real(real32), private :: m_alpha = 1.0
            !! The alpha value (transparency) for each bar.
    contains
        procedure, public :: get_count => pdb_get_count
        procedure, public :: get => pdb_get_data
        procedure, public :: set => pdb_set_data
        procedure, public :: get_data => pdb_get_data_set
        procedure, public :: get_label => pdb_get_label
        procedure, public :: set_label => pdb_set_label
        procedure, public :: get_use_labels => pdb_get_use_labels
        procedure, public :: set_use_labels => pdb_set_use_labels
        procedure, public :: get_command_string => pdb_get_cmd
        procedure, public :: get_data_string => pdb_get_data_cmd
        procedure, public :: get_axes_string => pdb_get_axes_cmd
        procedure, public :: get_bar_per_label_count => pdb_get_col_count
        procedure, public :: get_draw_against_y2 => pdb_get_use_y2
        procedure, public :: set_draw_against_y2 => pdb_set_use_y2
        procedure, public :: get_is_filled => pdb_get_is_filled
        procedure, public :: set_is_filled => pdb_set_is_filled
        procedure, public :: get_transparency => pdb_get_alpha
        procedure, public :: set_transparency => pdb_set_alpha
        generic, public :: define_data => pdb_set_data_1, pdb_set_data_2, &
            pdb_set_data_3
        procedure, private :: pdb_set_data_1
        procedure, private :: pdb_set_data_2
        procedure, private :: pdb_set_data_3
        procedure, public :: set_data_1 => pdb_set_data_1_core
        procedure, public :: set_data_2 => pdb_set_data_2_core
        procedure, public :: set_data_3 => pdb_set_data_3_core
    end type

contains
! ------------------------------------------------------------------------------
pure function pdb_get_count(this) result(x)
    !!  Gets the number of stored data points.
    class(plot_data_bar), intent(in) :: this
        !! The plot_data_bar object.
    integer(int32) :: x
        !! The number of stored data points.
    if (allocated(this%m_barData)) then
        x = size(this%m_barData, 1)
    else
        x = 0
    end if
end function

! ------------------------------------------------------------------------------
pure function pdb_get_data(this, index, col) result(x)
    !! Gets the requested data point.
    class(plot_data_bar), intent(in) :: this
        !! The plot_data_bar object.
    integer(int32), intent(in) :: index
        !! The data point index.
    integer(int32), intent(in) :: col
        !! The column index.
    real(real64) :: x
        !! The value.
    if (allocated(this%m_barData)) then
        x = this%m_barData(index, col)
    else
        x = 0.0d0
    end if
end function

! ------------------------------------------------------------------------------
subroutine pdb_set_data(this, index, col, x)
    !! Replaces the requested data point.
    class(plot_data_bar), intent(inout) :: this
        !! The plot_data_bar object.
    integer(int32), intent(in) :: index
        !! The data point index.
    integer(int32), intent(in) :: col
        !! The column index.
    real(real64), intent(in) :: x
        !! The new value.
    if (allocated(this%m_barData)) then
        this%m_barData(index, col) = x
    end if
end subroutine

! ------------------------------------------------------------------------------
pure function pdb_get_data_set(this, col) result(x)
    !! Gets the requested data set.
    class(plot_data_bar), intent(in) :: this
        !! The plot_data_bar object.
    integer(int32), intent(in) :: col
        !! The column index.
    real(real64), allocatable, dimension(:) :: x
        !! A copy of the data set.
    if (allocated(this%m_barData)) then
        x = this%m_barData(:,col)
    else
        allocate(x(0))
    end if
end function

! ------------------------------------------------------------------------------
pure function pdb_get_label(this, index) result(x)
    !! Gets the axis label associated with a specific data set.
    class(plot_data_bar), intent(in) :: this
        !! The plot_data_bar object.
    integer(int32), intent(in) :: index
        !! The index of the data set.
    character(len = :), allocatable :: x
        !! The label.
    if (allocated(this%m_axisLabels)) then
        x = char(this%m_axisLabels(index))
    else
        x = ""
    end if
end function

! ------------------------------------------------------------------------------
subroutine pdb_set_label(this, index, txt)
    !! Sets the axis label for a specific data set.
    class(plot_data_bar), intent(inout) :: this
        !! The plot_data_bar object.
    integer(int32) :: index
        !! The index of the data set.
    character(len = *), intent(in) :: txt
        !! The label.
    if (allocated(this%m_axisLabels)) then
        this%m_axisLabels(index) = txt
    end if
end subroutine

! ------------------------------------------------------------------------------
pure function pdb_get_use_labels(this) result(x)
    !! Gets a value determining if labels are used to identify the data.
    class(plot_data_bar), intent(in) :: this
        !! The plot_data_bar object.
    logical :: x
        !! Returns true if labels are used; else, false.
    x = this%m_useAxisLabels
end function

! ------------------------------------------------------------------------------
subroutine pdb_set_use_labels(this, x)
    !! Sets a value determining if labels are used to identify the data.
    class(plot_data_bar), intent(inout) :: this
        !! The plot_data_bar object.
    logical, intent(in) :: x
        !! Set to true if labels are used; else, false.
    this%m_useAxisLabels = x
end subroutine

! ------------------------------------------------------------------------------
function pdb_get_cmd(this) result(x)
    !! Gets the GNUPLOT command string for this object.
    class(plot_data_bar), intent(in) :: this
        !! The plot_data_bar object.
    character(len = :), allocatable :: x
        !! The command string.

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
    x = char(str%to_string())
end function

! ------------------------------------------------------------------------------
function pdb_get_data_cmd(this) result(x)
    !! Gets the GNUPLOT command string defining the data for this object.
    class(plot_data_bar), intent(in) :: this
        !! The plot_data_bar object.
    character(len = :), allocatable :: x
        !! The command string.

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
            call str%append(char(this%m_axisLabels(i)))
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
    x = char(str%to_string())
end function

! ------------------------------------------------------------------------------
function pdb_get_axes_cmd(this) result(x)
    !! Gets the GNUPLOT command defining which axes to plot against.
    class(plot_data_bar), intent(in) :: this
        !! The plot_data_bar object.
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
pure function pdb_get_col_count(this) result(x)
    !! Gets the number of data sets (columns).
    class(plot_data_bar), intent(in) :: this
        !! The plot_data_bar object.
    integer(int32) :: x
        !! The count.
    if (allocated(this%m_barData)) then
        x = size(this%m_barData, 2)
    else
        x = 0
    end if
end function

! ------------------------------------------------------------------------------
pure function pdb_get_use_y2(this) result(x)
    !! Gets a value determining if the data should be plotted against a
    !! secondary y-axis.
    class(plot_data_bar), intent(in) :: this
        !! The plot_data_bar object.
    logical :: x
        !! Returns true to plot against a secondary y-axis; else, false.
    x = this%m_useY2
end function

! ------------------------------------------------------------------------------
subroutine pdb_set_use_y2(this, x)
    !! Sets a value determining if the data should be plotted against a
    !! secondary y-axis.
    class(plot_data_bar), intent(inout) :: this
        !! The plot_data_bar object.
    logical, intent(in) :: x
        !! Set to true to plot against a secondary y-axis; else, false.
    this%m_useY2 = x
end subroutine

! ------------------------------------------------------------------------------
subroutine pdb_set_data_1(this, x, err)
    !! Defines a single data set.
    class(plot_data_bar), intent(inout) :: this
        !! The plot_data_bar object.
    real(real64), intent(in), dimension(:) :: x
        !! The data to plot.
    class(errors), intent(inout), optional, target :: err
        !! An error handling object.

    ! Process
    call this%set_data_1(x, err)
end subroutine

! ------------------------------------------------------------------------------
subroutine pdb_set_data_2(this, labels, x, err)
    !! Defines data along with associated axis labels.
    class(plot_data_bar), intent(inout) :: this
        !! The plot_data_bar object.
    class(string), intent(in), dimension(:) :: labels
        !! The axis labels to associate with the data.
    real(real64), intent(in), dimension(:) :: x
        !! The data set.
    class(errors), intent(inout), optional, target :: err
        !! An error handling object.

    ! Process
    call this%set_data_2(labels, x, err)
end subroutine

! ------------------------------------------------------------------------------
subroutine pdb_set_data_3(this, labels, x, fmt, err)
    !! Defines data along with labels and formatting information.
    class(plot_data_bar), intent(inout) :: this
        !! The plot_data_bar object.
    real(real64), intent(in), dimension(:) :: labels
        !! The axis labels to associate with the data.
    real(real64), intent(in), dimension(:) :: x
        !! The data set.
    character(len = *), intent(in), optional :: fmt
        !! The format string for the labels (e.g. '(I0)', etc.).
    class(errors), intent(inout), optional, target :: err
        !! An error handling object.

    ! Process
    call this%set_data_3(labels, x, fmt, err)
end subroutine

! ------------------------------------------------------------------------------
pure function pdb_get_is_filled(this) result(x)
    !! Gets a value determining if each bar is filled.
    class(plot_data_bar), intent(in) :: this
        !! The plot_data_bar object.
    logical :: x
        !! Returns true if the bars are to be filled; else, false.
    x = this%m_filled
end function

! ------------------------------------------------------------------------------
subroutine pdb_set_is_filled(this, x)
    !! Sets a value determining if each bar is filled.
    class(plot_data_bar), intent(inout) :: this
        !! The plot_data_bar object.
    logical, intent(in) :: x
        !! Set to true if the bars are to be filled; else, false.
    this%m_filled = x
end subroutine

! ------------------------------------------------------------------------------
pure function pdb_get_alpha(this) result(x)
    !! Gets the alpha (transparency) for the bar color.
    class(plot_data_bar), intent(in) :: this
        !! The plot_data_bar object.
    real(real32) :: x
        !! The alpha value ([0, 1]).
    x = this%m_alpha
end function

! ------------------------------------------------------------------------------
subroutine pdb_set_alpha(this, x)
    !! Gets the alpha (transparency) for the bar color.
    class(plot_data_bar), intent(inout) :: this
        !! The plot_data_bar object.
    real(real32), intent(in) :: x
        !! The alpha value ([0, 1]).
    if (x > 1.0) then
        this%m_alpha = 1.0
    else if (x < 0.0) then
        this%m_alpha = 0.0
    else
        this%m_alpha = x
    end if
end subroutine

! ------------------------------------------------------------------------------
subroutine pdb_set_data_1_core(this, x, err)
    !! Defines the data set.
    class(plot_data_bar), intent(inout) :: this
        !! The plot_data_bar object.
    real(real64), intent(in), dimension(:) :: x
        !! The data set.
    class(errors), intent(inout), optional, target :: err
        !! An error handling object.

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
        call report_memory_error(errmgr, "pdb_set_data_1_core", flag)
        return
    end if
    this%m_barData(:,1) = x
end subroutine

! ------------------------------------------------------------------------------
subroutine pdb_set_data_2_core(this, labels, x, err)
    ! Arguments
    class(plot_data_bar), intent(inout) :: this
        !! The plot_data_bar object.
    class(string), intent(in), dimension(:) :: labels
        !! The axis labels.
    real(real64), intent(in), dimension(:) :: x
        !! The data set.
    class(errors), intent(inout), optional, target :: err
        !! An error handling object.

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
        call report_array_size_mismatch_error(errmgr, "pdb_set_data_2_core", &
            "labels", n, size(labels))
        return
    end if

    ! Process
    if (allocated(this%m_axisLabels)) deallocate(this%m_axisLabels)
    if (allocated(this%m_barData)) deallocate(this%m_barData)
    allocate(this%m_barData(n, 1), stat = flag)
    if (flag == 0) allocate(this%m_axisLabels(n), stat = flag)
    if (flag /= 0) then
        call report_memory_error(errmgr, "pdb_set_data_2_core", flag)
        return
    end if
    this%m_barData(:,1) = x
    this%m_axisLabels = labels
end subroutine

! ------------------------------------------------------------------------------
subroutine pdb_set_data_3_core(this, labels, x, fmt, err)
    ! Arguments
    class(plot_data_bar), intent(inout) :: this
        !! The plot_data_bar object.
    real(real64), intent(in), dimension(:) :: labels
        !! The axis labels.
    real(real64), intent(in), dimension(:) :: x
        !! The data set.
    character(len = *), intent(in), optional :: fmt
        !! The format string for the labels (e.g. '(I0)', etc.).
    class(errors), intent(inout), optional, target :: err
        !! An error handling object.

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
        call report_array_size_mismatch_error(errmgr, "pdb_set_data_3_core", &
            "labels", n, size(labels))
        return
    end if

    ! Convert the numeric labels to strings
    allocate(lbls(n), stat = flag)
    if (flag /= 0) then
        call report_memory_error(errmgr, "pdb_set_data_3_core", flag)
        return
    end if
    do i = 1, n
        lbls(i) = to_string(labels(i), fmt)
    end do

    ! Store the data
    if (allocated(this%m_axisLabels)) deallocate(this%m_axisLabels)
    if (allocated(this%m_barData)) deallocate(this%m_barData)
    allocate(this%m_barData(n, 1), stat = flag)
    if (flag == 0) allocate(this%m_axisLabels(n), stat = flag)
    if (flag /= 0) then
        call report_memory_error(errmgr, "pdb_set_data_3_core", flag)
        return
    end if
    this%m_barData(:,1) = x
    this%m_axisLabels = lbls
end subroutine

! ------------------------------------------------------------------------------
end module
