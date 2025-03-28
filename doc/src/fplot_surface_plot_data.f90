module fplot_surface_plot_data
    use iso_fortran_env
    use fplot_plot_data
    use ferror
    use fplot_errors
    use strings
    implicit none
    private
    public :: surface_plot_data

    type, extends(plot_data) :: surface_plot_data
        !! Provides a three-dimensional surface plot data set.
        real(real64), private, allocatable, dimension(:,:) :: m_x
            !! Stores the x-coordinate data
        real(real64), private, allocatable, dimension(:,:) :: m_y
            !! Stores the y-coordinate data
        real(real64), private, allocatable, dimension(:,:) :: m_z
            !! Stores the z-coordinate data
        logical, private :: m_wireframe = .false.
            !! Set to true to display a wireframe of the surface; else, just a
            !! smooth surface will be drawn
    contains
        procedure, public :: get_size => surfd_get_size
        procedure, public :: get_x => surfd_get_x
        procedure, public :: set_x => surfd_set_x
        procedure, public :: get_y => surfd_get_y
        procedure, public :: set_y => surfd_set_y
        procedure, public :: get_z => surfd_get_z
        procedure, public :: set_z => surfd_set_z
        procedure, public :: get_use_wireframe => surfd_get_wireframe
        procedure, public :: set_use_wireframe => surfd_set_wireframe
        procedure, public :: get_command_string => surfd_get_cmd
        procedure, public :: get_data_string => surfd_get_data_cmd
        procedure, public :: define_data => surfd_set_data_1
    end type

contains
! ------------------------------------------------------------------------------
pure function surfd_get_size(this, dim) result(x)
    !! Gets the size of the stored data set.
    class(surface_plot_data), intent(in) :: this
        !! The suface_plot_data object.
    integer(int32), intent(in) :: dim
        !! The dimension of interest.  Notice, data is stored as a
        !! 2D matrix (i.e. only 1 and 2 are valid inputs).
    integer(int32) :: x
        !! The size of the requested dimension.
    if (allocated(this%m_x)) then
        x = size(this%m_x, dim)
    else
        x = 0
    end if
end function

! ------------------------------------------------------------------------------
pure function surfd_get_x(this, i, j) result(x)
    !! Gets the requested X data point.
    class(surface_plot_data), intent(in) :: this
        !! The suface_plot_data object.
    integer(int32), intent(in) :: i
        !! The row index.
    integer(int32), intent(in) :: j
        !! The column index.
    real(real64) :: x
        !! The value.
    if (allocated(this%m_x)) then
        x = this%m_x(i,j)
    else
        x = 0.0d0
    end if
end function

! --------------------
subroutine surfd_set_x(this, i, j, x)
    !! Sets the requested X data point.
    class(surface_plot_data), intent(inout) :: this
        !! The suface_plot_data object.
    integer(int32), intent(in) :: i
        !! The row index.
    integer(int32), intent(in) :: j
        !! The column index.
    real(real64), intent(in) :: x
        !! The value.
    if (allocated(this%m_x)) then
        this%m_x(i,j) = x
    end if
end subroutine

! ------------------------------------------------------------------------------
pure function surfd_get_y(this, i, j) result(x)
    !! Gets the requested Y data point.
    class(surface_plot_data), intent(in) :: this
        !! The suface_plot_data object.
    integer(int32), intent(in) :: i
        !! The row index.
    integer(int32), intent(in) :: j
        !! The column index.
    real(real64) :: x
        !! The value.
    if (allocated(this%m_y)) then
        x = this%m_y(i,j)
    else
        x = 0.0d0
    end if
end function

! --------------------
subroutine surfd_set_y(this, i, j, x)
    !! Sets the requested Y data point.
    class(surface_plot_data), intent(inout) :: this
        !! The suface_plot_data object.
    integer(int32), intent(in) :: i
        !! The row index.
    integer(int32), intent(in) :: j
        !! The column index.
    real(real64), intent(in) :: x
        !! The value.
    if (allocated(this%m_y)) then
        this%m_y(i,j) = x
    end if
end subroutine

! ------------------------------------------------------------------------------
pure function surfd_get_z(this, i, j) result(x)
    !! Gets the requested Z data point.
    class(surface_plot_data), intent(in) :: this
        !! The suface_plot_data object.
    integer(int32), intent(in) :: i
        !! The row index.
    integer(int32), intent(in) :: j
        !! The column index.
    real(real64) :: x
        !! The value.
    if (allocated(this%m_z)) then
        x = this%m_z(i,j)
    else
        x = 0.0d0
    end if
end function

! --------------------
subroutine surfd_set_z(this, i, j, x)
    !! Sets the requested Z data point.
    class(surface_plot_data), intent(inout) :: this
        !! The suface_plot_data object.
    integer(int32), intent(in) :: i
        !! The row index.
    integer(int32), intent(in) :: j
        !! The column index.
    real(real64), intent(in) :: x
        !! The value.
    if (allocated(this%m_z)) then
        this%m_z(i,j) = x
    end if
end subroutine

! ------------------------------------------------------------------------------
pure function surfd_get_wireframe(this) result(x)
    !! Gets a value determining if a wireframe mesh should be displayed.
    class(surface_plot_data), intent(in) :: this
        !! The suface_plot_data object.
    logical :: x
        !! Returns true if a wireframe mesh should be displayed; else, 
        !! false to display a solid surface.
    x = this%m_wireframe
end function

! --------------------
subroutine surfd_set_wireframe(this, x)
    !! Sets a value determining if a wireframe mesh should be displayed.
    class(surface_plot_data), intent(inout) :: this
        !! The suface_plot_data object.
    logical, intent(in) :: x
        !! Set to true if a wireframe mesh should be displayed; else,
        !! false to display a solid surface.
    this%m_wireframe = x
end subroutine

! ------------------------------------------------------------------------------
function surfd_get_cmd(this) result(x)
    !! Gets the GNUPLOT command string to represent this surface_plot_data 
    !! object.
    class(surface_plot_data), intent(in) :: this
        !! The suface_plot_data object.
    character(len = :), allocatable :: x
        !! The command string.

    ! Local Variables
    type(string_builder) :: str
    integer(int32) :: n

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

    ! PM3D or wireframe?
    if (this%get_use_wireframe()) then
        call str%append(" with lines")
    else
        call str%append(" with pm3d")
    end if

    ! End
    x = char(str%to_string())
end function

! ------------------------------------------------------------------------------
function surfd_get_data_cmd(this) result(x)
    !! Gets the GNUPLOT command string containing the actual data to plot.
    class(surface_plot_data), intent(in) :: this
        !! The suface_plot_data object.
    character(len = :), allocatable :: x
        !! The GNUPLOT command string.

    ! Local Variables
    type(string_builder) :: str
    integer(int32) :: i, j, m, n
    character :: delimiter, nl

    ! Initialization
    call str%initialize()
    m = this%get_size(1)
    n = this%get_size(2)
    delimiter = achar(9) ! tab delimiter
    nl = new_line(nl)

    ! Process
    do j = 1, n
        do i = 1, m
            call str%append(to_string(this%get_x(i,j)))
            call str%append(delimiter)
            call str%append(to_string(this%get_y(i,j)))
            call str%append(delimiter)
            call str%append(to_string(this%get_z(i,j)))
            call str%append(nl)
        end do
        if (j /= n) call str%append(nl)
    end do

    ! End
    x = char(str%to_string())
end function

! ------------------------------------------------------------------------------
subroutine surfd_set_data_1(this, x, y, z, err)
    !! Defines the data set.
    class(surface_plot_data), intent(inout) :: this
        !! The suface_plot_data object.
    real(real64), intent(in), dimension(:,:) :: x
        !! An M-by-N matrix containing the x-coordinate data.
    real(real64), intent(in), dimension(:,:) :: y
        !! An M-by-N matrix containing the y-coordinate data.
    real(real64), intent(in), dimension(:,:) :: z
        !! An M-by-N matrix containing the z-coordinate data.
    class(errors), intent(inout), optional, target :: err
        !! An error handling object.

    ! Local Variables
    integer(int32) :: i, j, m, n, flag
    class(errors), pointer :: errmgr
    type(errors), target :: deferr

    ! Initialization
    m = size(x, 1)
    n = size(x, 2)
    if (present(err)) then
        errmgr => err
    else
        errmgr => deferr
    end if

    ! Input Check
    if (size(y, 1) /= m .or. size(y, 2) /= n) then
        call report_matrix_size_mismatch_error(errmgr, "surfd_set_data_1", &
            "y", m, n, size(y, 1), size(y,2))
        return
    end if

    if (size(z, 1) /= m .or. size(z, 2) /= n) then
        call report_matrix_size_mismatch_error(errmgr, "surfd_set_data_1", &
            "z", m, n, size(z, 1), size(z,2))
        return
    end if

    ! Process
    if (allocated(this%m_x)) deallocate(this%m_x)
    if (allocated(this%m_y)) deallocate(this%m_y)
    if (allocated(this%m_z)) deallocate(this%m_z)
    allocate(this%m_x(m, n), stat = flag)
    if (flag == 0) allocate(this%m_y(m, n), stat = flag)
    if (flag == 0) allocate(this%m_z(m, n), stat = flag)
    if (flag /= 0) then
        call report_memory_error(errmgr, "surfd_set_data_1", flag)
        return
    end if
    do concurrent (j = 1:n)
        do i = 1, m
            this%m_x(i, j) = x(i, j)
            this%m_y(i, j) = y(i, j)
            this%m_z(i, j) = z(i, j)
        end do
    end do
end subroutine

! ------------------------------------------------------------------------------
end module