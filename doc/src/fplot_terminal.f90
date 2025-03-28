! fplot_terminal.f90

module fplot_terminal
    use iso_fortran_env
    use fplot_plot_object
    use fplot_constants
    use strings
    implicit none
    private
    public :: terminal
    public :: term_get_string_result

    type, abstract, extends(plot_object) :: terminal
        !! A GNUPLOT terminal object.
    private
        integer(int32) :: m_windowHeight = GNUPLOT_DEFAULT_WINDOW_HEIGHT
            !! The window height, in pixels.
        integer(int32) :: m_windowWidth = GNUPLOT_DEFAULT_WINDOW_WIDTH
            !! The window width, in pixels.
        integer(int32) :: m_termID = 0
            !! The terminal ID number.
        character(len = GNUPLOT_MAX_LABEL_LENGTH) :: m_title = ""
            !! The plot window title.
        logical :: m_hasTitle = .false.
            !! Determines if the plot title is defined.
        character(len = GNUPLOT_MAX_LABEL_LENGTH) :: m_fontName = &
            GNUPLOT_DEFAULT_FONTNAME
            !! The font used by the graph.
        integer(int32) :: m_fontSize = GNUPLOT_DEFAULT_FONT_SIZE
            !! The size of the font used by the graph.
    contains
        procedure, public :: get_window_width => term_get_window_width
        procedure, public :: set_window_width => term_set_window_width
        procedure, public :: get_window_height => term_get_window_height
        procedure, public :: set_window_height => term_set_window_height
        procedure, public :: get_command_string => term_get_command_string
        procedure, public :: get_plot_window_number => &
            term_get_plot_window_number
        procedure, public :: set_plot_window_number => &
            term_set_plot_window_number
        procedure, public :: get_title => term_get_title
        procedure, public :: set_title => term_set_title
        procedure, public :: get_font_name => term_get_font_name
        procedure, public :: set_font_name => term_set_font_name
        procedure, public :: get_font_size => term_get_font_size
        procedure, public :: set_font_size => term_set_font_size
        procedure(term_get_string_result), deferred, public :: get_id_string
    end type

    interface
        function term_get_string_result(this) result(x)
            !! Retrieves a string from a terminal.
            import terminal
            class(terminal), intent(in) :: this
                !! The terminal object.
            character(len = :), allocatable :: x
                !! The string.
        end function
    end interface
contains
! ------------------------------------------------------------------------------
    pure function term_get_window_width(this) result(x)
        !! Gets the width of the plot window.
        class(terminal), intent(in) :: this
            !! The terminal object.
        integer :: x
            !! The width of the plot window.
        x = this%m_windowWidth
    end function

! --------------------
    subroutine term_set_window_width(this, x)
        !! Sets the width of the plot window.
        class(terminal), intent(inout) :: this
            !! The terminal object.
        integer, intent(in) :: x
            !! The width of the plot window.
        if (x == 0) then
            this%m_windowWidth = GNUPLOT_DEFAULT_WINDOW_WIDTH
        else
            this%m_windowWidth = abs(x)
        end if
    end subroutine

! ------------------------------------------------------------------------------
    pure function term_get_window_height(this) result(x)
        !! Gets the height of the plot window.
        class(terminal), intent(in) :: this
            !! The terminal object.
        integer :: x
            !! The height of the plot window.
        x = this%m_windowHeight
    end function

! --------------------
    subroutine term_set_window_height(this, x)
        !! Sets the height of the plot window.
        class(terminal), intent(inout) :: this
            !! The terminal object.
        integer, intent(in) :: x
            !! The height of the plot window.
        if (x == 0) then
            this%m_windowHeight = GNUPLOT_DEFAULT_WINDOW_HEIGHT
        else
            this%m_windowHeight = abs(x)
        end if
    end subroutine

! ------------------------------------------------------------------------------
    pure function term_get_plot_window_number(this) result(x)
        !! Gets the targeted plot window number.
        class(terminal), intent(in) :: this
            !! The terminal object.
        integer(int32) :: x
            !! The plot window number.
        x = this%m_termID
    end function

! --------------------
    subroutine term_set_plot_window_number(this, x)
        !! Sets the targeted plot window number.
        class(terminal), intent(inout) :: this
            !! The terminal object.
        integer(int32), intent(in) :: x
            !! The plot window number.
        this%m_termID = x
    end subroutine

! ------------------------------------------------------------------------------
    function term_get_title(this) result(str)
        !! Gets the plot window's title.
        class(terminal), intent(in) :: this
            !! The terminal object.
        character(len = :), allocatable :: str
            !! The title.
        integer(int32) :: n
        n = len_trim(str)
        allocate(character(len = n) :: str)
        str = trim(this%m_title)
    end function

! --------------------
    subroutine term_set_title(this, txt)
        !! Sets the plot window's title.
        class(terminal), intent(inout) :: this
            !! The terminal object.
        character(len = *), intent(in) :: txt
            !! The title.
        integer(int32) :: n
        n = min(len_trim(txt), GNUPLOT_MAX_LABEL_LENGTH)
        this%m_title = ""
        if (n /= 0) then
            this%m_title(1:n) = txt(1:n)
            this%m_hasTitle = .true.
        else
            this%m_hasTitle = .false.
        end if
    end subroutine

! ------------------------------------------------------------------------------
    function term_get_font_name(this) result(name)
        !! Gets the name of the font used for text displayed by the graph.
        class(terminal), intent(in) :: this
            !! The terminal object.
        character(len = :), allocatable :: name
            !! The font name.
        integer(int32) :: n
        n = len_trim(this%m_fontName)
        allocate(character(len = n) :: name)
        name = trim(this%m_fontName)
    end function

! --------------------
    subroutine term_set_font_name(this, name)
        !! Sets the name of the font used for text displayed by the graph.
        class(terminal), intent(inout) :: this
            !! The terminal object.
        character(len = *), intent(in) :: name
            !! The font name.
        integer(int32) :: n
        n = min(len_trim(name), GNUPLOT_MAX_LABEL_LENGTH)
        this%m_fontName = ""
        if (n == 0) then
            this%m_fontName = GNUPLOT_DEFAULT_FONTNAME
        else
            this%m_fontName(1:n) = name(1:n)
        end if
    end subroutine

! ------------------------------------------------------------------------------
    pure function term_get_font_size(this) result(sz)
        !! Gets the size of the font used by the graph.
        class(terminal), intent(in) :: this
            !! The terminal object.
        integer(int32) :: sz
            !! The font size, in points.
        sz = this%m_fontSize
    end function

! --------------------
    subroutine term_set_font_size(this, sz)
        !! Sets the size of the font used by the graph.
        class(terminal), intent(inout) :: this
            !! The terminal object.
        integer(int32), intent(in) :: sz
            !! The font size, in points.
        if (sz == 0) then
            this%m_fontSize = GNUPLOT_DEFAULT_FONT_SIZE
        else
            this%m_fontSize = abs(sz)
        end if
    end subroutine

! ------------------------------------------------------------------------------
    function term_get_command_string(this) result(x)
        !! Returns the appropriate GNUPLOT command string to establish
        !! appropriate parameters.
        class(terminal), intent(in) :: this
            !! The terminal object.
        character(len = :), allocatable :: x
            !! The GNUPLOT command string.

        ! Local Variables
        type(string_builder) :: str

        ! Process
        call str%initialize()
        call str%append("set term ")
        call str%append(this%get_id_string())
        call str%append(" enhanced ")
        call str%append(to_string(this%get_plot_window_number()))
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
        if (this%m_hasTitle) then
            call str%append(' title "')
            call str%append(this%get_title())
            call str%append('"')
        end if
        x = char(str%to_string())
    end function

! ------------------------------------------------------------------------------
end module
