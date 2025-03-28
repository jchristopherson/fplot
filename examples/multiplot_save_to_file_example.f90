! This example illustrates the frequency response of the following mechanical
! system.
!
! -> y       |-> x1       |-> x2
! |                                  |/
! |-/\/\/\-| m |-/\/\/\-| m |-/\/\/\-|/
! |   k, b         k, b        k, b  |/
!
! The equations of motion for this system are as follows.
! M x" + B x' + K x = F1 y' + F2 y
!
! Where:
!     | m   0 |
! M = |       |
!     | 0   m |
!
!     | 2b     -b |
! B = |           |
!     | -b     2b |
!
!     | 2k     -k |
! K = |           |
!     | -k     2k |
!
!      | b |
! F1 = |   |
!      | 0 |
!
!      | k |
! F2 = |   |
!      | 0 |
!
! The solution may be computed by applying the Laplace transform such that:
! (M s**2 + B s + K) X = (F1 s + F2) Y
!
! And then solving for X / Y such that:
! Z = X / Y = inv(M s**2 + B s + K) * (F1 s + F2)
program example
    use iso_fortran_env
    use fplot_core
    implicit none

    ! Parameters
    real(real64), parameter :: m = 2.0d0
    real(real64), parameter :: k = 450.0d3
    real(real64), parameter :: b = 3.0d0
    real(real64), parameter :: pi = 3.1415926535897932384626433832795d0
    integer(int32), parameter :: npts = 1000
    complex(real64), parameter :: j = (0.0d0, -1.0d0)

    ! Local Variables
    complex(real64), dimension(npts) :: s, z1, z2
    real(real64), dimension(npts) :: freq, omega
    type(multiplot) :: mplt
    type(plot_2d) :: plt, pplt
    type(plot_data_2d) :: d1, d2, d3, d4
    class(plot_axis), pointer :: xAxis, yAxis
    class(legend), pointer :: lgnd

    ! Generate a frequency vector from 10 Hz to 1 kHz
    freq = logspace(1.0d0, 3.0d0, npts)
    omega = 2.0d0 * pi * freq
    s = j * omega

    ! Compute the frequency response functions for each mass
    z1 = (b * s + k) * (m * s**2 + 2.0d0 * b * s + 2.0d0 * k) / &
        ((m * s**2 + 2.0d0 * b * s + 2.0d0 * k)**2 + (-b * s - k) * (b * s + k))
    z2 = (b * s + k)**2 / &
        ((m * s**2 + 2.0d0 * b * s + 2.0d0 * k)**2 + (-b * s - k) * (b * s + k))
    
    ! Create the plots
    call mplt%initialize(2, 1)
    call mplt%set_font_size(14)
    call plt%initialize()
    xAxis => plt%get_x_axis()
    yAxis => plt%get_y_axis()
    lgnd => plt%get_legend()

    call lgnd%set_is_visible(.true.)

    call xAxis%set_title("Frequency [Hz]")
    call yAxis%set_title("Amplitude (X / Y)")

    call xAxis%set_is_log_scaled(.true.)
    call yAxis%set_is_log_scaled(.true.)

    call d1%set_name("X1")
    call d1%set_line_width(2.0)
    call d1%define_data(freq, abs(z1))

    call d2%set_name("X2")
    call d2%set_line_width(2.0)
    call d2%set_line_style(LINE_DASHED)
    call d2%define_data(freq, abs(z2))

    call plt%push(d1)
    call plt%push(d2)
    
    ! Set up the phase plot
    call pplt%initialize()
    xAxis => pplt%get_x_axis()
    yAxis => pplt%get_y_axis()

    call xAxis%set_title("Frequency [Hz]")
    call yAxis%set_title("Phase [deg]")

    call xAxis%set_is_log_scaled(.true.)

    call d3%set_name("X1")
    call d3%set_line_width(2.0)
    call d3%define_data(freq, 180.0d0 * atan2(aimag(z1), real(z1)) / pi)

    call d4%set_name("X2")
    call d4%set_line_width(2.0)
    call d4%set_line_style(LINE_DASHED)
    call d4%define_data(freq, 180.0d0 * atan2(aimag(z2), real(z2)) / pi)

    call pplt%push(d3)
    call pplt%push(d4)

    ! Save the plot to file
    call mplt%set(1, 1, plt)
    call mplt%set(2, 1, pplt)
    call mplt%save_file("example_multiplot_file.plt")
end program
