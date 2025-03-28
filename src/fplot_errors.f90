module fplot_errors
    use iso_fortran_env
    use ferror
    implicit none

! ******************************************************************************
! ERROR CODES
! ------------------------------------------------------------------------------
    integer(int32), parameter :: PLOT_OUT_OF_MEMORY_ERROR = 1000
        !! Occurs if there is insufficient memory available for the
        !! requested operation.
    integer(int32), parameter :: PLOT_INVALID_INPUT_ERROR = 1001
        !! Occurs if an invalid input is provided.
    integer(int32), parameter :: PLOT_INVALID_OPERATION_ERROR = 1002
        !! Occurs if an attempt is made to perform an invalid operation.
    integer(int32), parameter :: PLOT_ARRAY_SIZE_MISMATCH_ERROR = 1003
        !! Occurs if there is an array size mismatch error.
    integer(int32), parameter :: PLOT_GNUPLOT_FILE_ERROR = 1004
        !! Occurs if there is a GNUPLOT file error.

contains
! ------------------------------------------------------------------------------
subroutine report_memory_error(err, fcn, flag)
    !! Reports a memory allocation error.
    class(errors), intent(inout) :: err
        !! The error handling object.
    character(len = *), intent(in) :: fcn
        !! The name of the function or subroutine in which the error occurred.
    integer(int32), intent(in) :: flag
        !! The error flag returned by the system.

    ! Local Variables
    character(len = 256) :: msg

    ! Define the error message
    write(100, msg) "Memory allocation error returning flag ", flag, "."
    call err%report_error(fcn, trim(msg), PLOT_OUT_OF_MEMORY_ERROR)

    ! Formatting
100 format(A, I0, A)
end subroutine

! ------------------------------------------------------------------------------
subroutine report_file_create_error(err, fcn, fname, flag)
    !! Reports an I/O error related to file creating.
    class(errors), intent(inout) :: err
        !! The error handling object.
    character(len = *), intent(in) :: fcn
        !! The name of the function or subroutine in which the error occurred.
    character(len = *), intent(in) :: fname
        !! The filename.
    integer(int32), intent(in) :: flag
        !! The error flag returned by the system.

    ! Local Variables
    character(len = 2048) :: msg

    ! Define the error message
    write(100, msg) "File ", fname, " could not be created.  The error flag ", &
        flag, " was returned."
    call err%report_error(fcn, trim(msg), PLOT_GNUPLOT_FILE_ERROR)

    ! Formatting
100 format(A, A, A, I0, A)
end subroutine

! ------------------------------------------------------------------------------
subroutine report_array_size_mismatch_error(err, fcn, name, expected, actual)
    !! Reports an array size mismatch error.
    class(errors), intent(inout) :: err
        !! The error handling object.
    character(len = *), intent(in) :: fcn
        !! The name of the function or subroutine in which the error occurred.
    character(len = *), intent(in) :: name
        !! The variable name.
    integer(int32), intent(in) :: expected
        !! The expected array size.
    integer(int32), intent(in) :: actual
        !! The actual array size.

    ! Local Variables
    character(len = 256) :: msg

    ! Define the message
    write(100, msg) "Array ", name, " was found to be of length ", actual, &
        ", but was expected to be of length ", expected, "."
    call err%report_error(fcn, trim(msg), PLOT_ARRAY_SIZE_MISMATCH_ERROR)

    ! Formatting
100 format(A, A, A, I0, A, I0, A)
end subroutine

! ------------------------------------------------------------------------------
subroutine report_matrix_size_mismatch_error(err, fcn, name, mexp, nexp, &
    mact, nact)
    !! Reports a matrix size mismatch error.
    class(errors), intent(inout) :: err
        !! The error handling object.
    character(len = *), intent(in) :: fcn
        !! The name of the function or subroutine in which the error occurred.
    character(len = *), intent(in) :: name
        !! The variable name.
    integer(int32), intent(in) :: mexp
        !! The expected number of rows.
    integer(int32), intent(in) :: nexp
        !! The expected number of columns.
    integer(int32), intent(in) :: mact
        !! The actual number of rows.
    integer(int32), intent(in) :: nact
        !! The actual number of columns.

    ! Local Variables
    character(len = 256) :: msg

    ! Define the error
    write(100, msg) "Matrix ", name, " was expected to be of size ", mexp, &
        "-by-", nexp, ", but was found to be of size ", mact, "-by-", nact, "."
    call err%report_error(fcn, trim(msg), PLOT_ARRAY_SIZE_MISMATCH_ERROR)

    ! Formatting
100 format(A, A, A, I0, A, I0, A, I0, A, I0, A)
end subroutine

! ------------------------------------------------------------------------------
end module