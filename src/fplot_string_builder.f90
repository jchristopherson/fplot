!> @brief A module specific to FPLOT string building needs.
module fplot_string_builder
    use iso_varying_string
    use iso_fortran_env
    implicit none
    private
    public :: string_builder
    public :: to_string

    !> @brief A mechanism for constructing strings in a piecewise fashion in
    !! an efficient manner.
    type string_builder
    private
        integer(int32) :: m_length = 0
        character(len = :), allocatable :: m_buffer
    contains
        !> @brief Initializes the string_builder object.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine initialize(class(string_builder) this)
        !! @endcode
        !!
        !! @param[in,out] this The @ref string_builder object.
        procedure, public :: initialize => sb_init
        !> @brief Appends to the string.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine append(class(string_builder) this, character(len = *) txt)
        !! @endcode
        !!
        !! @param[in,out] this The @ref string_builder object.
        !! @param[in] txt The string to append.
        procedure, public :: append => sb_append
        !> @brief Returns the contents as a single string.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) pure function to_string(class(string_builder) this)
        !! @endcode
        !!
        !! @param[in] this The @ref string_builder object.
        !! @return Returns the contents as a string.
        procedure, public :: to_string => sb_to_string
        !> @brief Gets the current length of the string being built.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! integer(int32) pure function get_length(class(string_builder) this)
        !! @endcode
        !!
        !! @param[in] this The @ref string_builder object.
        !! @return Gets the length of the string.
        procedure, public :: get_length => sb_get_length
        !> @brief Clears the buffer.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine clear(class(string_builder) this)
        !! @endcode
        !!
        !! @param[in,out] this The @ref string_builder object.
        procedure, public :: clear => sb_clear
    end type

    !> @brief Converts an item to a string.
    !!
    !! @par Syntax
    !! @code{.f90}
    !! character(len = :) to_string(integer(int32) x)
    !! character(len = :) to_string(real(real64) x, optional character(len = *) fmt)
    !! character(len = :) to_string(real(real32) x, optional character(len = *) fmt)
    !! @endcode
    !!
    !! @param[in] x The item to convert.
    !! @param[in] fmt An optional formatting string.
    !!
    !! @return The resulting string.
    interface to_string
        module procedure :: to_string_int32
        module procedure :: to_string_real64
        module procedure :: to_string_real32
    end interface

    interface
        module subroutine sb_init(this)
            class(string_builder), intent(inout) :: this
        end subroutine

        module subroutine sb_append(this, txt)
            class(string_builder), intent(inout) :: this
            character(len = *), intent(in) :: txt
        end subroutine

        pure module function sb_to_string(this) result(txt)
            class(string_builder), intent(in) :: this
            character(len = :), allocatable :: txt
        end function

        pure module function sb_get_length(this) result(n)
            class(string_builder), intent(in) :: this
            integer(int32) :: n
        end function

        module subroutine sb_clear(this)
            class(string_builder), intent(inout) :: this
        end subroutine

        pure module function to_string_int32(x) result(rst)
            integer(int32), intent(in) :: x
            character(len = :), allocatable :: rst
        end function

        pure module function to_string_real64(x, fmt) result(rst)
            real(real64), intent(in) :: x
            character(len = *), intent(in), optional :: fmt
            character(len = :), allocatable :: rst
        end function

        pure module function to_string_real32(x, fmt) result(rst)
            real(real32), intent(in) :: x
            character(len = *), intent(in), optional :: fmt
            character(len = :), allocatable :: rst
        end function
    end interface
end module