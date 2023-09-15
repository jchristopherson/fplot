! fplot_filled_plot_data.f90

submodule (fplot_core) fplot_filled_plot_data
contains
! ------------------------------------------------------------------------------
    module function fpd_get_axes_cmd(this) result(x)
        ! Arguments
        class(filled_plot_data), intent(in) :: this
        character(len = :), allocatable :: x

        ! Define which axes the data is to be plotted against
        if (this%get_draw_against_y2()) then
            x = "axes x1y2"
        else
            x = "axes x1y1"
        end if
    end function

! ------------------------------------------------------------------------------
    pure module function fpd_get_draw_against_y2(this) result(x)
        class(filled_plot_data), intent(in) :: this
        logical :: x
        x = this%m_useY2
    end function

! --------------------
    module subroutine fpd_set_draw_against_y2(this, x)
        class(filled_plot_data), intent(inout) :: this
        logical, intent(in) :: x
        this%m_useY2 = x
    end subroutine

! ------------------------------------------------------------------------------
    module function fpd_get_cmd(this) result(x)
        ! Arguments
        class(filled_plot_data), intent(in) :: this
        character(len = :), allocatable :: x

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

        ! Establish filled data
        call str%append(" with filledcurves")

        ! Line Color
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
    module function fpd_get_data_cmd(this) result(x)
        ! Arguments
        class(filled_plot_data), intent(in) :: this
        character(len = :), allocatable :: x

        ! Local Variables
        type(string_builder) :: str
        integer(int32) :: i
        character(len = :), allocatable :: nl, delimiter

        ! Initialization
        call str%initialize()
        delimiter = achar(9)    ! tab delimiter
        nl = new_line(nl)

        ! Process
        do i = 1, size(this%m_data, 1)
            call str%append(to_string(this%m_data(i,1)))
            call str%append(delimiter)
            call str%append(to_string(this%m_data(i,2)))
            call str%append(delimiter)
            call str%append(to_string(this%m_data(i,3)))
            call str%append(nl)
        end do

        ! End
        x = str%to_string()
    end function

! ------------------------------------------------------------------------------
    module subroutine fpd_define_data(this, x, y, yc, err)
        ! Arguments
        class(filled_plot_data), intent(inout) :: this
        real(real64), intent(in), dimension(:) :: x, y, yc
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        type(errors), target :: deferr
        class(errors), pointer :: errmgr
        character(len = 256) :: errmsg
        integer(int32) :: i, n, flag

        ! Set up error handling
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Input Checking
        n = size(x)
        if (size(y) /= n) then
            write(errmsg, 100) "Expected the y array to have ", n, &
                " elements, but found an array with ", size(y), " elements."
            call errmgr%report_error("fpd_define_data", trim(errmsg), &
                PLOT_ARRAY_SIZE_MISMATCH_ERROR)
            return
        end if
        if (size(yc) /= n) then
            write(errmsg, 100) "Expected the yc array to have ", n, &
                " elements, but found an array with ", size(yc), " elements."
            call errmgr%report_error("fpd_define_data", trim(errmsg), &
                PLOT_ARRAY_SIZE_MISMATCH_ERROR)
            return
        end if

        ! Allocate space for the data
        if (allocated(this%m_data)) deallocate(this%m_data)
        allocate(this%m_data(n, 3), stat = flag)
        if (flag /= 0) then
            call errmgr%report_error("fpd_define_data", &
                "Insufficient memory available.", &
                PLOT_OUT_OF_MEMORY_ERROR)
            return
        end if

        ! Store the data
        do concurrent (i = 1:n)
            this%m_data(i,1) = x(i)
            this%m_data(i,2) = y(i)
            this%m_data(i,3) = yc(i)
        end do
        
100     format(A, I0, A, I0, A)
    end subroutine

! ------------------------------------------------------------------------------
end submodule
