! fplot_label.f90

module fplot_label
    use iso_fortran_env
    use fplot_plot_object
    use fplot_constants
    use strings
    implicit none
    private
    public :: plot_label

    type, extends(plot_object) :: plot_label
        !! Defines a plot label.
        logical, private :: m_visible = .true.
            !! Is the label visible?
        real(real32), private, dimension(3) :: m_position
            !! The x, y, and z coordinates of the label.
        real(real32), private :: m_angle = 0.0
            !! The rotation angle of the label.
        character(len = PLOTDATA_MAX_NAME_LENGTH), private :: m_text
            !! The label text.
    contains
        procedure, public :: get_command_string => lbl_get_cmd
        procedure, public :: get_is_visible => lbl_get_is_visible
        procedure, public :: set_is_visible => lbl_set_is_visible
        procedure, public :: get_position => lbl_get_position
        procedure, public :: set_position => lbl_set_position
        procedure, public :: get_angle => lbl_get_angle
        procedure, public :: set_angle => lbl_set_angle
        procedure, public :: get_text => lbl_get_txt
        procedure, public :: set_text => lbl_set_txt
    end type
contains
! ------------------------------------------------------------------------------
    function lbl_get_cmd(this) result(x)
        !! Gets the GNUPLOT command string for the label.
        class(plot_label), intent(in) :: this
            !! The plot_label object.
        character(len = :), allocatable :: x
            !! The command string.

        ! Local Variables
        type(string_builder) :: str
        real(real32) :: pt(3)

        ! Initialization
        call str%initialize()
        pt = this%get_position()

        ! If visible, draw the label
        if (this%get_is_visible()) then
            call str%append('set label "')
            call str%append(this%get_text())
            call str%append('"')

            call str%append(" at ")
            call str%append(to_string(pt(1)))
            call str%append(",")
            call str%append(to_string(pt(2)))
            call str%append(",")
            call str%append(to_string(pt(3)))

            call str%append(" rotate by ")
            call str%append(to_string(this%get_angle()))

            x = char(str%to_string())
        end if
    end function

! ------------------------------------------------------------------------------
    pure function lbl_get_is_visible(this) result(x)
        !! Gets a value determining if the label is to be drawn.
        class(plot_label), intent(in) :: this
            !! The plot_label object.
        logical :: x
            !! Returns true if the label is to be drawn; else, false.
        x = this%m_visible
    end function

! --------------------
    subroutine lbl_set_is_visible(this, x)
        !! Sets a value determining if the label is to be drawn.
        class(plot_label), intent(inout) :: this
            !! The plot_label object.
        logical, intent(in) :: x
            !! Set to true if the label is to be drawn; else, false.
        this%m_visible = x
    end subroutine
    
! ------------------------------------------------------------------------------
    pure function lbl_get_position(this) result(x)
        !! Gets the position of the label in terms of plot coordinates.
        class(plot_label), intent(in) :: this
            !! The plot_label object.
        real(real32), dimension(3) :: x
            !! A 3-element array containing the X, Y, and Z position of the 
            !! label.
        x = this%m_position
    end function

! --------------------
    subroutine lbl_set_position(this, x)
        !! Sets the position of the label in terms of plot coordinates.
        class(plot_label), intent(inout) :: this
            !! The plot_label object.
        real(real32), intent(in), dimension(3) :: x
            !! A 3-element array containing the X, Y, and Z position of the 
            !! label.
        this%m_position = x
    end subroutine

! ------------------------------------------------------------------------------
    pure function lbl_get_angle(this) result(x)
        !! Gets the angle of the label text, in degrees.
        class(plot_label), intent(in) :: this
            !! The plot_label object.
        real(real32) :: x
            !! The angle, in degrees.
        x = this%m_angle
    end function

! --------------------
    subroutine lbl_set_angle(this, x)
        !! Sets the angle of the label text, in degrees.
        class(plot_label), intent(inout) :: this
            !! The plot_label object.
        real(real32), intent(in) :: x
            !! The angle, in degrees.
        this%m_angle = x
    end subroutine
    
! ------------------------------------------------------------------------------
    function lbl_get_txt(this) result(x)
        !! Gets the text displayed by the label.
        class(plot_label), intent(in) :: this
            !! The plot_label object.
        character(len = :), allocatable :: x
            !! The text string to display.
        x = trim(this%m_text)
    end function

! --------------------
    subroutine lbl_set_txt(this, x)
        !! Sets the text displayed by the label.
        class(plot_label), intent(inout) :: this
            !! The plot_label object.
        character(len = *), intent(in) :: x
            !! The text string to display.
        integer(int32) :: n
        n = min(len(x), PLOTDATA_MAX_NAME_LENGTH)
        this%m_text = ""
        this%m_text(1:n) = x(1:n)
    end subroutine

! ******************************************************************************
! ADDED: JAN. 09, 2024 - JAC
! ------------------------------------------------------------------------------
    ! pure subroutine lbl_assign(x, y)
    !     type(plot_label), intent(out) :: x
    !     class(plot_label), intent(in) :: y
    !     x%m_visible = y%m_visible
    !     x%m_position = y%m_position
    !     x%m_angle = y%m_angle
    !     x%m_text = y%m_text
    ! end subroutine

! ------------------------------------------------------------------------------
end module
