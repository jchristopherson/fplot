program example
    use iso_fortran_env
    use fplot_core
    implicit none

    ! Parameters
    integer(int32), parameter :: npts = 1000
    integer(int32), parameter :: nparams = 3

    ! Local Variables
    type(correlation_plot) :: plt
    real(real64) :: m, x(npts, nparams)

    ! Generate some data
    call random_number(m)
    call random_number(x)
    x(:,2) = m * x(:,1) * (1.0d0 + 0.5d0 * x(:,1)) + 0.1d0 * (x(:,2) - 1.0d0)
    x(:,3) = 1.0d-2 * x(:,3) / (x(:,2) + 1.5d0)

    ! Create the plot
    call plt%initialize(x, width = 1200, height = 800)
    call plt%draw()
end program