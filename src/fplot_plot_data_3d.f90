module fplot_plot_data_3d
    use iso_fortran_env
    use fplot_plot_data
    use fplot_errors
    use fplot_simplify
    use ferror
    use strings
    implicit none
    private
    public :: plot_data_3d

    type, extends(scatter_plot_data) :: plot_data_3d
        !! Defines a three-dimensional plot data set.
        real(real64), private, allocatable, dimension(:,:) :: m_data
            !! An N-by-3 matrix containing the x, y, and z data points.
    contains
        procedure, public :: get_count => pd3d_get_data_count
        procedure, public :: get_x => pd3d_get_x_data
        procedure, public :: set_x => pd3d_set_x_data
        procedure, public :: get_y => pd3d_get_y_data
        procedure, public :: set_y => pd3d_set_y_data
        procedure, public :: get_z => pd3d_get_z_data
        procedure, public :: set_z => pd3d_set_z_data
        procedure, public :: get_axes_string => pd3d_get_axes_cmd
        procedure, public :: get_data_string => pd3d_get_data_cmd
        procedure, public :: define_data => pd3d_set_data_1
        procedure, public :: get_x_data => pd3d_get_x_array
        procedure, public :: get_y_data => pd3d_get_y_array
        procedure, public :: get_z_data => pd3d_get_z_array
        procedure, public :: get_color_data => pd3d_get_c_array
        procedure, public :: get_point_size_data => pd3d_get_c_array
    end type

contains
! ------------------------------------------------------------------------------
pure function pd3d_get_data_count(this) result(x)
    !! Gets the number of data points.
    class(plot_data_3d), intent(in) :: this
        !! The plot_data_3d object.
    integer(int32) :: x
        !! The number of data points.
    if (allocated(this%m_data)) then
        x = size(this%m_data, 1)
    else
        x = 0
    end if
end function

! ------------------------------------------------------------------------------
pure function pd3d_get_x_data(this, index) result(x)
    !! Gets the requested X data point.
    class(plot_data_3d), intent(in) :: this
        !! The plot_data_3d object.
    integer(int32), intent(in) :: index
        !! The index of the data point to retrieve.
    real(real64) :: x
        !! The requested data point.
    if (allocated(this%m_data)) then
        x = this%m_data(index, 1)
    else
        x = 0.0d0
    end if
end function

! --------------------
subroutine pd3d_set_x_data(this, index, x)
    !! Sets the requested X data point.
    class(plot_data_3d), intent(inout) :: this
        !! The plot_data_3d object.
    integer(int32), intent(in) :: index
        !! The index of the data point to replace.
    real(real64), intent(in) :: x
        !! The data point.
    if (allocated(this%m_data)) then
        this%m_data(index, 1) = x
    end if
end subroutine

! ------------------------------------------------------------------------------
pure function pd3d_get_y_data(this, index) result(x)
    !! Gets the requested Y data point.
    class(plot_data_3d), intent(in) :: this
        !! The plot_data_3d object.
    integer(int32), intent(in) :: index
        !! The index of the data point to retrieve.
    real(real64) :: x
        !! The requested data point.
    if (allocated(this%m_data)) then
        x = this%m_data(index, 2)
    else
        x = 0.0d0
    end if
end function

! --------------------
subroutine pd3d_set_y_data(this, index, x)
    !! Sets the requested Y data point.
    class(plot_data_3d), intent(inout) :: this
        !! The plot_data_3d object.
    integer(int32), intent(in) :: index
        !! The index of the data point to replace.
    real(real64), intent(in) :: x
        !! The data point.
    if (allocated(this%m_data)) then
        this%m_data(index, 2) = x
    end if
end subroutine

! ------------------------------------------------------------------------------
pure function pd3d_get_z_data(this, index) result(x)
    !! Gets the requested Z data point.
    class(plot_data_3d), intent(in) :: this
        !! The plot_data_3d object.
    integer(int32), intent(in) :: index
        !! The index of the data point to retrieve.
    real(real64) :: x
        !! The requested data point.
    if (allocated(this%m_data)) then
        x = this%m_data(index, 3)
    else
        x = 0.0d0
    end if
end function

! --------------------
subroutine pd3d_set_z_data(this, index, x)
    !! Sets the requested Z data point.
    class(plot_data_3d), intent(inout) :: this
        !! The plot_data_3d object.
    integer(int32), intent(in) :: index
        !! The index of the data point to replace.
    real(real64), intent(in) :: x
        !! The data point.
    if (allocated(this%m_data)) then
        this%m_data(index, 3) = x
    end if
end subroutine

! ------------------------------------------------------------------------------
function pd3d_get_axes_cmd(this) result(x)
    !! Gets the GNUPLOT command string defining which axes the data
    !! is to be plotted against.
    class(plot_data_3d), intent(in) :: this
        !! The plot_data_3d object.
    character(len = :), allocatable :: x
        !! The command string.

    ! Output
    x = ""
end function

! ------------------------------------------------------------------------------
function pd3d_get_data_cmd(this) result(x)
    !! Gets the GNUPLOT command string containing the actual data to plot.
    class(plot_data_3d), intent(in) :: this
        !! The plot_data_3d object.
    character(len = :), allocatable :: x
        !! The command string.

    ! Local Variables
    type(string_builder) :: str
    integer(int32) :: i
    character :: delimiter, nl
    real(real64), allocatable, dimension(:) :: xv, yv, zv, cv, ps
    real(real64), allocatable, dimension(:,:) :: pts
    real(real64) :: tol, maxz, minz, eps
    logical :: usecolors, usevarpoints

    ! Initialization
    call str%initialize()
    delimiter = achar(9) ! tab delimiter
    nl = new_line(nl)
    usecolors = this%get_use_data_dependent_colors()
    usevarpoints = this%get_use_variable_size_points()

    ! Process
    xv = this%get_x_data()
    yv = this%get_y_data()
    zv = this%get_z_data()
    if (usecolors .and. usevarpoints) then
        cv = this%get_color_data()
        ps = this%get_point_size_data()
        do i = 1, size(xv)
            call str%append(to_string(xv(i)))
            call str%append(delimiter)
            call str%append(to_string(yv(i)))
            call str%append(delimiter)
            call str%append(to_string(zv(i)))
            call str%append(delimiter)
            call str%append(to_string(ps(i)))
            call str%append(delimiter)
            call str%append(to_string(cv(i)))
            call str%append(nl)
        end do
    else if (usecolors .and. .not.usevarpoints) then
        cv = this%get_color_data()
        do i = 1, size(xv)
            call str%append(to_string(xv(i)))
            call str%append(delimiter)
            call str%append(to_string(yv(i)))
            call str%append(delimiter)
            call str%append(to_string(zv(i)))
            call str%append(delimiter)
            call str%append(to_string(cv(i)))
            call str%append(nl)
        end do
    else if (.not.usecolors .and. usevarpoints) then
        ps = this%get_point_size_data()
        do i = 1, size(xv)
            call str%append(to_string(xv(i)))
            call str%append(delimiter)
            call str%append(to_string(yv(i)))
            call str%append(delimiter)
            call str%append(to_string(zv(i)))
            call str%append(delimiter)
            call str%append(to_string(ps(i)))
            call str%append(nl)
        end do
    else
        if (this%get_simplify_data()) then
            maxz = maxval(zv)
            minz = minval(zv)
            tol = abs(this%get_simplification_factor() * (maxz - minz))
            eps = 10.0d0 * epsilon(eps)
            if (tol < eps) tol = eps
            pts = simplify_polyline(xv, yv, zv, tol)
            do i = 1, size(pts, 1)
                call str%append(to_string(pts(i,1)))
                call str%append(delimiter)
                call str%append(to_string(pts(i,2)))
                call str%append(delimiter)
                call str%append(to_string(pts(i,3)))
                call str%append(nl)
            end do
        else
            do i = 1, size(xv)
                call str%append(to_string(xv(i)))
                call str%append(delimiter)
                call str%append(to_string(yv(i)))
                call str%append(delimiter)
                call str%append(to_string(zv(i)))
                call str%append(nl)
            end do
        end if
    end if

    ! End
    x = char(str%to_string())
end function

! ------------------------------------------------------------------------------
subroutine pd3d_set_data_1(this, x, y, z, c, ps, err)
    !! Defines the data set.
    class(plot_data_3d), intent(inout) :: this
        !! The plot_data_3d object.
    real(real64), intent(in), dimension(:) :: x
        !! An N-element array containing the x coordinate data.
    real(real64), intent(in), dimension(:) :: y
        !! An N-element array containing the y coordinate data.
    real(real64), intent(in), dimension(:) :: z
        !! An N-element array containing the z coordinate data.
    real(real64), intent(in), dimension(:), optional :: c
        !! An N-element array defining how color should vary with the 
        !! current colormap for each value.
    real(real64), intent(in), dimension(:), optional :: ps
        !! An N-element array defining the size of each data point.
    class(errors), intent(inout), optional, target :: err
        !! An error handling object.

    ! Local Variables
    integer(int32) :: i, n, flag, ncols
    class(errors), pointer :: errmgr
    type(errors), target :: deferr

    ! Initialization
    n = size(x)
    ncols = 3
    if (present(c)) ncols = ncols + 1
    if (present(ps)) ncols = ncols + 1
    if (present(err)) then
        errmgr => err
    else
        errmgr => deferr
    end if

    ! Input Check
    if (size(y) /= n) then
        call report_array_size_mismatch_error(errmgr, "pd3d_set_data_1", &
            "y", n, size(y))
        return
    end if
    if (size(z) /= n) then
        call report_array_size_mismatch_error(errmgr, "pd3d_set_data_1", &
            "z", n, size(z))
        return
    end if
    if (present(c)) then
        if (size(c) /= n) then
            call report_array_size_mismatch_error(errmgr, &
                "pd3d_set_data_1", "c", n, size(c))
            return
        end if
    end if
    if (present(ps)) then
        if (size(ps) /= n) then
            call report_array_size_mismatch_error(errmgr, &
                "pd3d_set_data_1", "ps", n, size(ps))
        end if
    end if

    ! Process
    if (allocated(this%m_data)) deallocate(this%m_data)
    allocate(this%m_data(n, ncols), stat = flag)
    if (flag /= 0) then
        call report_memory_error(errmgr, "pd3d_set_data_1", flag)
        return
    end if
    if (present(c) .and. present(ps)) then
        call this%set_use_data_dependent_colors(.true.)
        call this%set_use_variable_size_points(.true.)
        do concurrent (i = 1:n)
            this%m_data(i, 1) = x(i)
            this%m_data(i, 2) = y(i)
            this%m_data(i, 3) = z(i)
            this%m_data(i, 4) = ps(i)
            this%m_data(i, 5) = c(i)
        end do
    else if (present(c) .and. .not.present(ps)) then
        call this%set_use_data_dependent_colors(.true.)
        call this%set_use_variable_size_points(.false.)
        do concurrent (i = 1:n)
            this%m_data(i, 1) = x(i)
            this%m_data(i, 2) = y(i)
            this%m_data(i, 3) = z(i)
            this%m_data(i, 4) = c(i)
        end do
    else if (.not.present(c) .and. present(ps)) then
        call this%set_use_data_dependent_colors(.false.)
        call this%set_use_variable_size_points(.true.)
        do concurrent (i = 1:n)
            this%m_data(i, 1) = x(i)
            this%m_data(i, 2) = y(i)
            this%m_data(i, 3) = z(i)
            this%m_data(i, 4) = ps(i)
        end do
    else
        call this%set_use_data_dependent_colors(.false.)
        call this%set_use_variable_size_points(.false.)
        do concurrent (i = 1:n)
            this%m_data(i, 1) = x(i)
            this%m_data(i, 2) = y(i)
            this%m_data(i, 3) = z(i)
        end do
    end if
end subroutine

! ------------------------------------------------------------------------------
function pd3d_get_x_array(this) result(x)
    !! Gets the stored X data array.
    class(plot_data_3d), intent(in) :: this
        !! The plot_data_3d object.
    real(real64), allocatable, dimension(:) :: x
        !! A copy of the stored data array.

    ! Process
    if (allocated(this%m_data)) then
        x = this%m_data(:,1)
    end if
end function

! ------------------------------------------------------------------------------
function pd3d_get_y_array(this) result(x)
    !! Gets the stored Y data array.
    class(plot_data_3d), intent(in) :: this
        !! The plot_data_3d object.
    real(real64), allocatable, dimension(:) :: x
        !! A copy of the stored data array.

    ! Process
    if (allocated(this%m_data)) then
        x = this%m_data(:,2)
    end if
end function

! ------------------------------------------------------------------------------
function pd3d_get_z_array(this) result(x)
    !! Gets the stored Z data array.
    class(plot_data_3d), intent(in) :: this
        !! The plot_data_3d object.
    real(real64), allocatable, dimension(:) :: x
        !! A copy of the stored data array.

    ! Process
    if (allocated(this%m_data)) then
        x = this%m_data(:,3)
    end if
end function

! ******************************************************************************
! ADDED: OCT. 9, 2020 - JAC
! ------------------------------------------------------------------------------
function pd3d_get_c_array(this) result(x)
    !! Gets the stored color scaling data array.
    class(plot_data_3d), intent(in) :: this
        !! The plot_data_3d object.
    real(real64), allocatable, dimension(:) :: x
        !! A copy of the stored data array.

    ! Process
    if (allocated(this%m_data)) then
        if (size(this%m_data, 2) == 4) then
            x = this%m_data(:,4)
        else if (size(this%m_data, 2) == 5) then
            x = this%m_data(:,5)
        end if
    end if
end function

! ******************************************************************************
! ADDED: JAN. 12, 2020 - JAC
! ------------------------------------------------------------------------------
function pd3d_get_ps_array(this) result(x)
    !! Gets the stored point scaling data array.
    class(plot_data_3d), intent(in) :: this
        !! The plot_data_3d object.
    real(real64), allocatable, dimension(:) :: x
        !! A copy of the stored data array.

    ! Process
    if (allocated(this%m_data)) then
        if (size(this%m_data, 2) > 3) then
            x = this%m_data(:,4)
        end if
    end if
end function

! ------------------------------------------------------------------------------
end module