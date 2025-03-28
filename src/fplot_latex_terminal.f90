! fplot_latex_terminal.f90

module fplot_latex_terminal
    use iso_fortran_env
    use fplot_terminal
    use fplot_constants
    use strings
    implicit none
    private
    public :: latex_terminal

    type, extends(terminal) :: latex_terminal
        !! A LATEX terminal.
        character(len = 14), private :: m_id = "epslatex color"
            !! The terminal ID string
        character(len = GNUPLOT_MAX_PATH_LENGTH), private :: m_fname = "default.tex"
            !! The filename of the file to write.
    contains
        procedure, public :: get_filename => tex_get_filename
        procedure, public :: set_filename => tex_set_filename
        procedure, public :: get_id_string => tex_get_term_string
        procedure, public :: get_command_string => tex_get_command_string
    end type

contains
! ------------------------------------------------------------------------------
    function tex_get_term_string(this) result(x)
        !! Retrieves a GNUPLOT terminal identifier string.
        class(latex_terminal), intent(in) :: this
            !! The latex_terminal object.
        character(len = :), allocatable :: x
            !! The string.
        integer(int32) :: n
        n = len_trim(this%m_id)
        allocate(character(len = n) :: x)
        x = this%m_id
    end function

! ------------------------------------------------------------------------------
    function tex_get_filename(this) result(txt)
        !! Gets the filename for the output LATEX file.
        class(latex_terminal), intent(in) :: this
            !! The latex_terminal object.
        character(len = :), allocatable :: txt
            !! The filename, including the file extension (.tex).
        integer(int32) :: n
        n = len_trim(this%m_fname)
        allocate(character(len = n) :: txt)
        txt = trim(this%m_fname)
    end function

! --------------------
    subroutine tex_set_filename(this, txt)
        !! Sets the filename for the output LATEX file.
        class(latex_terminal), intent(inout) :: this
            !! The latex_terminal object.
        character(len = *), intent(in) :: txt
            !! The filename, including the file extension (.tex).
        integer(int32) :: n
        n = min(len_trim(txt), GNUPLOT_MAX_PATH_LENGTH)
        this%m_fname = ""
        if (n /= 0) then
            this%m_fname(1:n) = txt(1:n)
        else
            this%m_fname = "default.tex"
        end if
    end subroutine

! ------------------------------------------------------------------------------
    function tex_get_command_string(this) result(x)
        !! Returns the appropriate GNUPLOT command string to establish
        !! appropriate parameters.
        class(latex_terminal), intent(in) :: this
            !! The latex_terminal object.
        character(len = :), allocatable :: x
            !! The GNUPLOT command string.

        ! Local Variables
        type(string_builder) :: str

        ! Process
        call str%initialize()
        call str%append("set term epslatex color ")
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
