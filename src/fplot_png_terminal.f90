! fplot_png_terminal.f90

module fplot_png_terminal
    use iso_fortran_env
    use strings
    use fplot_terminal
    use fplot_constants
    implicit none
    private
    public :: png_terminal

    type, extends(terminal) :: png_terminal
        !! Defines a terminal used for producing PNG outputs.
        character(len = 3), private :: m_id = "png"
            !! The terminal ID string
        character(len = GNUPLOT_MAX_PATH_LENGTH), private :: m_fname = "default.png"
            !! The filename of the PNG file to write.
    contains
        procedure, public :: get_filename => png_get_filename
        procedure, public :: set_filename => png_set_filename
        procedure, public :: get_id_string => png_get_term_string
        procedure, public :: get_command_string => png_get_command_string
    end type

contains
! ------------------------------------------------------------------------------
    function png_get_term_string(this) result(x)
        !! Retrieves a GNUPLOT terminal identifier string.
        class(png_terminal), intent(in) :: this
            !! The png_terminal object.
        character(len = :), allocatable :: x
            !! The string.
        integer(int32) :: n
        n = len_trim(this%m_id)
        allocate(character(len = n) :: x)
        x = this%m_id
    end function

! ------------------------------------------------------------------------------
    function png_get_filename(this) result(txt)
        !! Gets the filename for the output PNG file.
        class(png_terminal), intent(in) :: this
            !! The png_terminal object.
        character(len = :), allocatable :: txt
            !! The filename, including the file extension (.png).
        integer(int32) :: n
        n = len_trim(this%m_fname)
        allocate(character(len = n) :: txt)
        txt = trim(this%m_fname)
    end function

! --------------------
    subroutine png_set_filename(this, txt)
        !!Sets the filename for the output PNG file.
        class(png_terminal), intent(inout) :: this
            !! The png_terminal object.
        character(len = *), intent(in) :: txt
            !! The filename, including the file extension (.png).
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
    function png_get_command_string(this) result(x)
        !! Returns the appropriate GNUPLOT command string to establish
        !! appropriate parameters.
        class(png_terminal), intent(in) :: this
            !! The png_terminal object.
        character(len = :), allocatable :: x
            !! The GNUPLOT command string.

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
        x = char(str%to_string())
    end function

end module