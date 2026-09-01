module fplot_pdf_terminal
    use iso_fortran_env
    use fplot_terminal
    use strings
    implicit none
    private
    public :: pdf_terminal

    type, extends(file_terminal) :: pdf_terminal
        !! Defines a terminal used for producing PDF outputs.
        character(len = 17), private :: m_id = "pdfcairo enhanced"
            !! The terminal ID string.
        integer(int32), private :: m_resolution = 100
            !! The device resolution in DPI.
    contains
        procedure, public :: get_id_string => pdf_get_term_string
        procedure, public :: get_command_string => pdf_get_command_string
        procedure, public :: get_resolution => pdf_get_resolution
        procedure, public :: set_resolution => pdf_set_resolution
    end type
contains
! ------------------------------------------------------------------------------
    function pdf_get_term_string(this) result(x)
        !! Retrieves a GNUPLOT terminal identifier string.
        class(pdf_terminal), intent(in) :: this
            !! The pdf_terminal object.
        character(len = :), allocatable :: x
            !! The string.
        integer(int32) :: n
        n = len(this%m_id)
        allocate(character(len = n) :: x)
        x = this%m_id
    end function

! ------------------------------------------------------------------------------
    function pdf_get_command_string(this) result(x)
        !! Returns the appropriate GNUPLOT command string to establish
        !! appropriate parameters.
        class(pdf_terminal), intent(in) :: this
            !! The pdf_terminal object.
        character(len = :), allocatable :: x
            !! The GNUPLOT command string.

        ! Local Variables
        type(string_builder) :: str
        real(real32) :: xsize, ysize

        ! Compute the size
        xsize = real(this%get_window_width(), real32) / real(this%get_resolution(), real32)
        ysize = real(this%get_window_height(), real32) / real(this%get_resolution(), real32)

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
        call str%append(to_string(xsize))
        call str%append("in,")
        call str%append(to_string(ysize))
        call str%append("in")
        call str%append(new_line('a'))
        call str%append("set output ")
        call str%append('"')
        call str%append(this%get_filename())
        call str%append('"')
        x = char(str%to_string())
    end function

! ------------------------------------------------------------------------------
    pure function pdf_get_resolution(this) result(x)
        !! Gets the target device resolution.
        class(pdf_terminal), intent(in) :: this
            !! The pdf_terminal object.
        integer(int32) :: x
            !! The resolution, in DPI.
        x = this%m_resolution
    end function

! --------------------
    subroutine pdf_set_resolution(this, x)
        !! Sets the target device resolution.
        class(pdf_terminal), intent(inout) :: this
            !! The pdf_terminal object.
        integer(int32), intent(in) :: x
            !! The resolution, in DPI.
        this%m_resolution = x
    end subroutine

! ------------------------------------------------------------------------------
end module