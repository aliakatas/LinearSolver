
    module initialisation

    implicit none
    
    private

    interface initialise_to_random
    procedure  initialise_to_random_1D
    procedure  initialise_to_random_2D
    end interface 
    
    public initialise_tridiagonal
    public initialise_to_random
    
    contains

    !#############################################
    subroutine initialise_to_random_2D(mat, nrows, ncols, start, last)
    real, allocatable, intent(inout)    :: mat(:,:)
    integer, intent(in)                 :: nrows, ncols
    real, intent(in)                    :: start, last

    real                                :: r, temp
    integer                             :: irow, icol

    r = last - start

    do icol = 1, ncols
        do irow = 1, nrows
            call random_number(temp)
            mat(irow, icol) = start + temp * r
        end do
    end do
    end subroutine initialise_to_random_2D

    !#############################################
    subroutine initialise_to_random_1D(vec, ncols, start, last)
    real, allocatable, intent(inout)    :: vec(:)
    integer, intent(in)                 :: ncols
    real, intent(in)                    :: start, last

    real                                :: r, temp
    integer                             :: icol

    r = last - start

    do icol = 1, ncols
        call random_number(temp)
        vec(icol) = start + temp * r
    end do
    end subroutine initialise_to_random_1D

    !#############################################
    subroutine initialise_tridiagonal(mat, n, upper, diag, lower)
    real, allocatable, intent(inout)    :: mat(:,:)
    integer, intent(in)                 :: n
    real, intent(in)                    :: upper, diag, lower

    integer                             :: irow, icol

    do icol = 1, n
        do irow = 1, n
            if (irow == icol) then
                mat(irow, icol) = diag
            else
                if (irow == icol + 1) then
                    mat(irow, icol) = upper
                else if (irow == icol - 1) then
                    mat(irow, icol) = lower
                else
                    mat(irow, icol) = 0.
                end if
            end if
        end do
    end do
    end subroutine initialise_tridiagonal

    end module initialisation
