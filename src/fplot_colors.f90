! fplot_colors.f90

module fplot_colors
    use iso_fortran_env
    implicit none
    private
    public :: color
    public :: operator(==)
    public :: operator(/=)
    public :: CLR_BLACK
    public :: CLR_WHITE
    public :: CLR_RED
    public :: CLR_LIME
    public :: CLR_BLUE
    public :: CLR_YELLOW
    public :: CLR_CYAN
    public :: CLR_MAGENTA
    public :: CLR_SILVER
    public :: CLR_GRAY
    public :: CLR_MAROON
    public :: CLR_OLIVE
    public :: CLR_GREEN
    public :: CLR_PURPLE
    public :: CLR_TEAL
    public :: CLR_NAVY
    public :: CLR_ORANGE
    public :: color_list

    type color
        !! Describes an RGB color.
        integer(int32), public :: red = 0
            !! The red component of the color (must be between 0 and 255).
        integer(int32), public :: green = 0
            !! The green component of the color (must be between 0 and 255).
        integer(int32), public :: blue = 255
            !! The blue component of the color (must be between 0 and 255).
        integer(int32), public :: alpha = 0
            !! The alpha component of the color (must be between 0 and 255).
            !! Notice, 0 is fully opaque and 255 is fully transparent.
    contains
        procedure, public, pass :: to_hex_string => clr_to_hex_string
        procedure, public, pass :: copy_from => clr_copy_from
    end type

    interface operator(==)
        module procedure :: clr_equals
    end interface

    interface operator(/=)
        module procedure :: clr_not_equals
    end interface

    type(color), parameter :: CLR_BLACK = color(0, 0, 0, 0)
        !! Black.
    type(color), parameter :: CLR_WHITE = color(255, 255, 255, 0)
        !! White.
    type(color), parameter :: CLR_RED = color(255, 0, 0, 0)
        !! Red.
    type(color), parameter :: CLR_LIME = color(0, 255, 0, 0)
        !! Lime.
    type(color), parameter :: CLR_BLUE = color(0, 0, 255, 0)
        !! Blue.
    type(color), parameter :: CLR_YELLOW = color(255, 255, 0, 0)
        !! Yellow.
    type(color), parameter :: CLR_CYAN = color(0, 255, 255, 0)
        !! Cyan.
    type(color), parameter :: CLR_MAGENTA = color(255, 0, 255, 0)
        !! Magenta.
    type(color), parameter :: CLR_SILVER = color(192, 192, 192, 0)
        !! Silver.
    type(color), parameter :: CLR_GRAY = color(128, 128, 128, 0)
        !! Gray.
    type(color), parameter :: CLR_MAROON = color(128, 0, 0, 0)
        !! Maroon.
    type(color), parameter :: CLR_OLIVE = color(128, 128, 0, 0)
        !! Olive.
    type(color), parameter :: CLR_GREEN = color(0, 128, 0, 0)
        !! Green.
    type(color), parameter :: CLR_PURPLE = color(128, 0, 128, 0)
        !! Purple.
    type(color), parameter :: CLR_TEAL = color(0, 128, 128, 0)
        !! Teal.
    type(color), parameter :: CLR_NAVY = color(0, 0, 128, 0)
        !! Navy.
    type(color), parameter :: CLR_ORANGE = color(255, 165, 0, 0)
        !! Orange.

    ! A list of colors that can be cycled through by plotting code
    type(color), parameter, dimension(7) :: color_list = [ &
        color(0, int(0.447 * 255), int(0.741 * 255), 0), &
        color(int(0.85 * 255), int(0.325 * 255), int(0.098 * 255), 0), &
        color(int(0.929 * 255), int(0.694 * 255), int(0.125 * 255), 0), &
        color(int(0.494 * 255), int(0.184 * 255), int(0.556 * 255), 0), &
        color(int(0.466 * 255), int(0.674 * 255), int(0.188 * 255), 0), &
        color(int(0.301 * 255), int(0.745 * 255), int(0.933 * 255), 0), &
        color(int(0.635 * 255), int(0.078 * 255), int(0.184 * 255), 0)]

contains
! ------------------------------------------------------------------------------
    pure function clr_to_hex_string(this) result(txt)
        !! Returns the color in hexadecimal format.
        class(color), intent(in) :: this
            !! The color object.
        character(8) :: txt
            !! A string containing the hexadecimal equivalent.

        ! Local Variables
        integer(int32) :: r, g, b, a, clr

        ! Clip each color if necessary
        if (this%red < 0) then
            r = 0
        else if (this%red > 255) then
            r = 255
        else
            r = this%red
        end if

        if (this%green < 0) then
            g = 0
        else if (this%green > 255) then
            g = 255
        else
            g = this%green
        end if

        if (this%blue < 0) then
            b = 0
        else if (this%blue > 255) then
            b = 255
        else
            b = this%blue
        end if

        if (this%alpha < 0) then
            a = 0
        else if (this%alpha > 255) then
            a = 255
        else
            a = this%alpha
        end if

        ! Build the color information
        clr = ishft(a, 24) + ishft(r, 16) + ishft(g, 8) + b

        ! Convert the integer to a hexadecimal string
        write(txt, '(Z8.8)') clr
    end function

! ------------------------------------------------------------------------------
    subroutine clr_copy_from(this, clr)
        !! Copies another color to this color.
        class(color), intent(inout) :: this
            !! The color object.
        class(color), intent(in) :: clr
            !! The color to copy.

        this%red = clr%red
        this%green = clr%green
        this%blue = clr%blue
    end subroutine

! ******************************************************************************
! ADDED: JAN. 09, 2024 - JAC
! ------------------------------------------------------------------------------
    ! pure subroutine clr_assign(x, y)
    !     type(color), intent(out) :: x
    !     class(color), intent(in) :: y
    !     call x%copy_from(y)
    ! end subroutine

! ------------------------------------------------------------------------------
    pure function clr_equals(x, y) result(rst)
        type(color), intent(in) :: x, y
        logical :: rst
        rst = .true.
        if (x%red /= y%red .or. &
            x%green /= y%green .or. &
            x%blue /= y%blue &
        ) then
            rst = .false.
        end if
    end function

! ------------------------------------------------------------------------------
    pure function clr_not_equals(x, y) result(rst)
        type(color), intent(in) :: x, y
        logical :: rst
        rst = .not.clr_equals(x, y)
    end function

! ------------------------------------------------------------------------------
end module