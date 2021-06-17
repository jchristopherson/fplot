! fplot_vector_field_plot_data.f90

submodule (fplot_core) fplot_vector_field_plot_data
contains
! ------------------------------------------------------------------------------
    module function vfpd_get_data_cmd(this) result(x)
        ! Arguments
        class(vector_field_plot_data), intent(in) :: this
        character(len = :), allocatable :: x

        ! Local Variables
        type(string_builder) :: str
        integer(int32) :: i, j, m, n
        character :: delimiter, nl

        ! Initialization
        call str%initialize()
        delimiter = achar(9)    ! tab delimiter
        nl = new_line(nl)
        
        ! Fix later
        m = size(this%m_data, 1)
        n = size(this%m_data, 2)

        ! Need a quick return in the event no data exists

        ! Process
        do j = 1, n
            do i = 1, m
                ! ORDER: X, Y, DX, DY
                call str%append(to_string(this%m_data(i,j,1)))
                call str%append(delimiter)
                call str%append(to_string(this%m_data(i,j,2)))
                call str%append(delimiter)
                call str%append(to_string(this%m_data(i,j,3)))
                call str%append(delimiter)
                call str%append(to_string(this%m_data(i,j,4)))
                call str%append(nl)
            end do
        end do

        ! End
        x = str%to_string()
    end function

! ------------------------------------------------------------------------------
    module function vfpd_get_cmd(this) result(x)
        ! Arguments
        class(vector_field_plot_data), intent(in) :: this
        character(len = :), allocatable :: x

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

        ! Property Definition
        call str%append(" with vectors")

        ! End
        x = str%to_string()
    end function

! ------------------------------------------------------------------------------
    module subroutine vfpd_define_data(this, x, y, dx, dy, err)
        ! Arguments
        class(vector_field_plot_data), intent(inout) :: this
        real(real64), intent(in), dimension(:,:) :: x, y, dx, dy
        class(errors), intent(inout), optional, target :: err

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
        if (.not.check_size(y, m, n)) then
            call write_errmsg("y", size(y, 1), size(y, 2), m, n, errmsg)
            go to 100
        end if
        if (.not.check_size(dx, m, n)) then
            call write_errmsg("dx", size(y, 1), size(y, 2), m, n, errmsg)
            go to 100
        end if
        if (.not.check_size(dy, m, n)) then
            call write_errmsg("dy", size(y, 1), size(y, 2), m, n, errmsg)
            go to 100
        end if

        ! Allocate space for the data
        if (allocated(this%m_data)) deallocate(this%m_data)
        allocate(this%m_data(m, n, 4), stat = flag)
        if (flag /= 0) then
            call errmgr%report_error("vfpd_define_data", &
                "Insufficient memory available.", &
                PLOT_OUT_OF_MEMORY_ERROR)
            return
        end if

        ! Store the data
        do concurrent(j = 1:n)
            do i = 1, m
                this%m_data(i,j,1) = x(i,j)
                this%m_data(i,j,2) = y(i,j)
                this%m_data(i,j,3) = dx(i,j)
                this%m_data(i,j,4) = dy(i,j)
            end do
        end do

        ! End
        return

        ! Error Handling
    100 continue
        call errmgr%report_error("vfpd_define_data", trim(errmsg), &
            PLOT_ARRAY_SIZE_MISMATCH_ERROR)
        return

    contains
        ! Checks the size of the supplied array (xc) vs the reference row (mref)
        ! and column (nref) dimensions.
        !
        ! Returns true if the array size matches the reference; else, false.
        function check_size(xc, mref, nref) result(rst)
            ! Arguments
            real(real64), intent(in), dimension(:,:) :: xc
            integer(int32), intent(in) :: mref, nref
            logical :: rst

            ! Process
            if (size(xc, 1) /= mref .or. size(xc, 2) /= nref) then
                rst = .false.
            else
                rst = .true.
            end if
        end function

        ! Writes an error message regarding array size.
        subroutine write_errmsg(name, mfound, nfound, mexpect, nexpect, msg)
            ! Arguments
            character(len = *), intent(in) :: name
            integer(int32), intent(in) :: mfound, nfound, mexpect, nexpect
            character(len = *), intent(out) :: msg

            ! Process
            write(msg, '(AI0AI0AI0AI0A)') "Input " // name // &
                " is not sized correctly.  Expected a ", mexpect, "-by-", &
                nexpect, " matrix, but found a ", mfound, "-by-", nfound, &
                " matrix."
        end subroutine
    end subroutine

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------
end submodule
