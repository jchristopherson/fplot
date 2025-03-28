module fplot_plot_object
    use iso_fortran_env
    implicit none

    type, abstract :: plot_object
        !! The base type for all plot objects.
    contains
        procedure(get_string_result), deferred, public :: get_command_string
    end type

    interface
        function get_string_result(this) result(x)
            !! Returns a string from a plot_object.
            import plot_object
            class(plot_object), intent(in) :: this
                !! The plot_object object.
            character(len = :), allocatable :: x
                !! The result string.
        end function
    end interface
end module