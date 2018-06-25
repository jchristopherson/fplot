! fplot_multiplot.f90

submodule (fplot_core) fplot_multiplot
contains
! ------------------------------------------------------------------------------
    module function mp_get_command(this) result(x)
        ! Arguments
        class(multiplot), intent(in) :: this
        character(len = :), allocatable :: x

        ! Local Variables
        type(string_builder) :: str
        integer(int32) :: i, j, m, n
        class(plot), pointer :: ptr

        ! Initialization
        call str%initialize()
        m = this%get_row_count()
        n = this%get_column_count()

        ! Write the terminal commands

        ! Set up the multiplot
        call str%append("set multiplot layout ")
        call str%append(to_string(this%get_row_count()))
        call str%append(",")
        call str%append(to_string(this%get_column_count()))
        call str%append(new_line('a'))

        ! Write commands for each plot object
        do j = 1, n
            do i = 1, m
                ptr => this%get(i, j)
                call str%append(new_line('a'))
                call str%append(ptr%get_command_string())
            end do
        end do

        ! Close out the multiplot
        call str%append(new_line('a'))
        call str%append("unset multiplot")

        ! Get the string
        x = str%to_string()
    end function

! ------------------------------------------------------------------------------
    module subroutine mp_init(this, m, n)
        class(multiplot), intent(inout) :: this
        integer(int32), intent(in) :: m, n
        call this%m_plots%clear()
        this%m_rows = m
        this%m_cols = n
    end subroutine
    
! ------------------------------------------------------------------------------
    pure module function mp_get_rows(this) result(x)
        class(multiplot), intent(in) :: this
        integer(int32) :: x
        x = this%m_rows
    end function

! --------------------
    pure module function mp_get_cols(this) result(x)
        class(multiplot), intent(in) :: this
        integer(int32) :: x
        x = this%m_cols
    end function
    
! ------------------------------------------------------------------------------
    module function mp_get_title(this) result(x)
        class(multiplot), intent(in) :: this
        character(len = :), allocatable :: x
        x = this%m_title
    end function

! --------------------
    module subroutine mp_set_title(this, x)
        ! Arguments
        class(multiplot), intent(inout) :: this
        character(len = *), intent(in) :: x

        ! Local Variables
        integer(int32) :: n

        ! Process
        n = min(len(x), PLOTDATA_MAX_NAME_LENGTH)
        this%m_title = ""
        this%m_title(1:n) = x(1:n)
    end subroutine
    
! ------------------------------------------------------------------------------
    module subroutine mp_draw(this, persist, err)
        ! Arguments
        class(multiplot), intent(in) :: this
        logical, intent(in), optional :: persist
        class(errors), intent(inout), optional, target :: err

        ! Parameters
        character(len = *), parameter :: fname = "temp_gnuplot_file.plt"

        ! Local Variables
        logical :: p
        integer(int32) :: fid, flag
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg

        ! Initialization
        if (present(persist)) then
            p = persist
        else
            p = .true.
        end if
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Open the file for writing, and write the contents to file
        open(newunit = fid, file = fname, iostat = flag)
        if (flag > 0) then
            write(errmsg, "(AI0A)") &
                "The file could not be opened/created.  Error code ", flag, &
                " was encountered."
            call errmgr%report_error("mp_draw", trim(errmsg), &
                PLOT_GNUPLOT_FILE_ERROR)
            return
        end if
        write(fid, '(A)') this%get_command_string()
        close(fid)

        ! Launch GNUPLOT
        if (p) then
            call execute_command_line("gnuplot --persist " // fname)
        else
            call execute_command_line("gnuplot " // fname)
        end if

        ! Clean up by deleting the file
        open(newunit = fid, file = fname)
        close(fid, status = "delete")
    end subroutine

! ------------------------------------------------------------------------------
    module function mp_get(this, i, j) result(x)
        ! Arguments
        class(multiplot), intent(in) :: this
        integer(int32), intent(in) :: i, j
        class(plot), pointer :: x

        ! Local Variables
        class(*), pointer :: item
        integer(int32) :: ind

        ! Process
        ind = this%m_rows * (j - 1) + i
        item => this%m_plots%get(ind)
        select type (item)
        class is (plot)
            x => item
        class default
            nullify(x)
        end select
    end function

! --------------------
    module subroutine mp_set(this, i, j, x)
        ! Arguments
        class(multiplot), intent(inout) :: this
        integer(int32), intent(in) :: i, j
        class(plot), intent(in) :: x

        ! Local Variables
        integer(int32) :: ind

        ! Process
        ind = this%m_rows * (j - 1) + i
        call this%m_plots%set(ind, x)
    end subroutine
    
! ------------------------------------------------------------------------------
end submodule
