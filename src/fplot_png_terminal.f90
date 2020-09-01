! fplot_png_terminal.f90

submodule (fplot_core) fplot_png_terminal
contains
! ------------------------------------------------------------------------------
    !> @brief Retrieves a GNUPLOT terminal identifier string.
    !!
    !! @param[in] this The png_terminal object.
    !! @return The string.
    module function png_get_term_string(this) result(x)
        class(png_terminal), intent(in) :: this
        character(len = :), allocatable :: x
        n = len_trim(this%m_id)
        allocate(character(len = n) :: x)
        x = this%m_id
    end function

! ------------------------------------------------------------------------------
    !> @brief Gets the filename for the output PNG file.
    !!
    !! @param[in] this The png_terminal object.
    !! @return The filename, including the file extension (.png).
    module function png_get_filename(this) result(txt)
        class(png_terminal), intent(in) :: this
        character(len = :), allocatable :: txt
        integer(int32) :: n
        n = len_trim(this%m_fname)
        allocate(character(len = n) :: txt)
        txt = trim(this%m_fname)
    end function

! --------------------
    !> @brief Sets the filename for the output PNG file.
    !!
    !! @param[in,out] this The png_terminal object.
    !! @param[in] The filename, including the file extension (.png).
    module subroutine png_set_filename(this, txt)
        class(png_terminal), intent(inout) :: this
        character(len = *), intent(in) :: txt
        integer(int32) :: n
        n = min(len_trim(txt), GNUPLOT_MAX_PATH_LENGTH)
        this%m_fname = ""
        if (n /= 0) then
            this%m_fname(1:n) = txt(1:n)
        else
            this%m_fname = "default.png"
        end if
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Returns the appropriate GNUPLOT command string to establish
    !! appropriate parameters.
    !!
    !! @param[in] this The terminal object.
    !! @return The GNUPLOT command string.
    module function png_get_command_string(this) result(x)
        ! Arguments
        class(png_terminal), intent(in) :: this
        character(len = :), allocatable :: x

        ! Local Variables
        type(string_builder) :: str

        ! Process
        call str%initialize()
        call str%append("set term pngcairo enhanced ")
        call str%append(" font ")
        call str%append('"')
        call str%append(this%get_font_name())
        call str%append(',')
        call str%append(to_string(this%get_font_size()))
        call str%append('"')
        call str%append(" size ")
        call str%append(to_string(this%get_window_width()))
        call str%append(",")
        call str%append(to_string(this%get_window_height()))
        call str%append(new_line('a'))
        call str%append("set output ")
        call str%append('"')
        call str%append(this%get_filename())
        call str%append('"')
        x = str%to_string()
    end function

end submodule