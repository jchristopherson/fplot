module fplot_svg_terminal
    use iso_fortran_env
    use fplot_terminal
    use fplot_constants
    use strings
    implicit none
    private
    public :: svg_terminal

    type, extends(file_terminal) :: svg_terminal
        !! An SVG terminal.
        character(len = 12), private :: m_id = 'svg enhanced background "white"'
            !! The terminal ID string
    contains
        procedure, public :: get_id_string => svg_get_term_string
        procedure, public :: get_command_string => svg_get_command_string
    end type

contains
! ------------------------------------------------------------------------------
    function svg_get_term_string(this) result(x)
        !! Retrieves a GNUPLOT terminal identifier string.
        class(svg_terminal), intent(in) :: this
            !! The svg_terminal object.
        character(len = :), allocatable :: x
            !! The string.
        integer(int32) :: n
        n = len_trim(this%m_id)
        allocate(character(len = n) :: x)
        x = this%m_id
    end function

! ------------------------------------------------------------------------------
    function svg_get_command_string(this) result(x)
        !! Returns the appropriate GNUPLOT command string to establish
        !! appropriate parameters.
        class(svg_terminal), intent(in) :: this
            !! The svg_terminal object.
        character(len = :), allocatable :: x
            !! The GNUPLOT command string.

        ! Local Variables
        type(string_builder) :: str

        ! Process
        call str%initialize()
        call str%append("set term ")
        call str%append(this%get_id_string())
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
        call str%append(' background "white"')
        call str%append(new_line('a'))
        call str%append("set output ")
        call str%append('"')
        call str%append(this%get_filename())
        call str%append('"')
        x = char(str%to_string())
    end function

! ------------------------------------------------------------------------------
end module