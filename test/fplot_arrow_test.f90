module fplot_arrow_test
    use iso_fortran_env
    use fplot_core
    use fortran_test_helper
    implicit none
contains
function test_arrow() result(rst)
    ! Arguments
    logical :: rst

    ! Parameters
    real(real32), parameter :: p1(3) = [0.5, 1.3, -0.2]

    ! Local Variables
    type(plot_arrow) :: a

    ! Initialization
    rst = .true.

    ! Test 1
    call a%set_is_visible(.false.)
    if (a%get_is_visible()) then
        rst = .false.
        print '(A)', "TEST FAILED: test_arrow -1"
    end if

    ! Test 2
    call a%set_tail_location(p1)
    if (.not.assert(a%get_tail_location(), p1)) then
        rst = .false.
        print '(A)', "TEST FAILED: test_arrow -2"
    end if

    ! Test 3
    call a%set_head_location(p1)
    if (.not.assert(a%get_head_location(), p1)) then
        rst = .false.
        print '(A)', "TEST FAILED: test_arrow -3"
    end if

    ! Test 4
    call a%set_color(CLR_LIME)
    if (a%get_color() /= CLR_LIME) then
        rst = .false.
        print '(A)', "TEST FAILED: test_arrow -4"
    end if

    ! Test 5
    call a%set_line_style(LINE_DASH_DOT_DOT)
    if (a%get_line_style() /= LINE_DASH_DOT_DOT) then
        rst = .false.
        print '(A)', "TEST FAILED: test_arrow -5"
    end if

    ! Test 6
    call a%set_line_width(5.45)
    if (.not.assert(a%get_line_width(), 5.45)) then
        rst = .false.
        print '(A)', "TEST FAILED: test_arrow -6"
    end if

    ! Test 7
    call a%set_head_type(ARROW_NO_HEAD)
    if (a%get_head_type() /= ARROW_NO_HEAD) then
        rst = .false.
        print '(A)', "TEST FAILED: test_arrow -7"
    end if

    ! Test 8
    call a%set_head_fill(ARROW_EMPTY)
    if (a%get_head_fill() /= ARROW_EMPTY) then
        rst = .false.
        print '(A)', "TEST FAILED: test_arrow -8"
    end if

    ! Test 9
    call a%set_move_to_front(.false.)
    if (a%get_move_to_front()) then
        rst = .false.
        print '(A)', "TEST FAILED: test_arrow -9"
    end if

    ! Test 10
    call a%set_head_size(24.3)
    if (.not.assert(a%get_head_size(), 24.3)) then
        rst = .false.
        print '(A)', "TEST FAILED: test_arrow -10"
    end if

    ! Test 11
    call a%set_head_angle(0.5)
    if (.not.assert(a%get_head_angle(), 0.5)) then
        rst = .false.
        print '(A)', "TEST FAILED: test_arrow -11"
    end if

    ! Test 12
    call a%set_head_back_angle(-1.2)
    if (.not.assert(a%get_head_back_angle(), -1.2)) then
        rst = .false.
        print '(A)', "TEST FAILED: test_arrow -12"
    end if

    ! Test 13
    call a%set_use_default_size(.false.)
    if (a%get_use_default_size()) then
        rst = .false.
        print '(A)', "TEST FAILED: test_arrow -13"
    end if
end function

end module