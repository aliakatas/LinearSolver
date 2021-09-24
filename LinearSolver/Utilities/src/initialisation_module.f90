
    module initialisation
    
    use custom_precision

    implicit none
    
    private

    interface initialise_to_random
    procedure  initialise_to_random_1D
    procedure  initialise_to_random_2D
    procedure  initialise_to_random_1D_dp
    procedure  initialise_to_random_2D_dp
    end interface 
    
    interface initialise_tridiagonal
    procedure initialise_tridiagonal_sp
    procedure initialise_tridiagonal_dp
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
    subroutine initialise_tridiagonal_sp(mat, n, upper, diag, lower)
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
                    mat(irow, icol) = lower
                else if (irow == icol - 1) then
                    mat(irow, icol) = upper
                else
                    mat(irow, icol) = 0.
                end if
            end if
        end do
    end do
    end subroutine initialise_tridiagonal_sp
    
    !#############################################
    subroutine initialise_to_random_2D_dp(mat, nrows, ncols, start, last)
    real(DP), allocatable, intent(inout)    :: mat(:,:)
    integer, intent(in)                     :: nrows, ncols
    real(DP), intent(in)                    :: start, last

    real(DP)                                :: r, temp
    integer                                 :: irow, icol

    r = last - start

    do icol = 1, ncols
        do irow = 1, nrows
            call random_number(temp)
            mat(irow, icol) = start + temp * r
        end do
    end do
    end subroutine initialise_to_random_2D_dp

    !#############################################
    subroutine initialise_to_random_1D_dp(vec, ncols, start, last)
    real(DP), allocatable, intent(inout)    :: vec(:)
    integer, intent(in)                     :: ncols
    real(DP), intent(in)                    :: start, last

    real(DP)                                :: r, temp
    integer                                 :: icol

    r = last - start

    do icol = 1, ncols
        call random_number(temp)
        vec(icol) = start + temp * r
    end do
    end subroutine initialise_to_random_1D_dp

    !#############################################
    subroutine initialise_tridiagonal_dp(mat, n, upper, diag, lower)
    real(DP), allocatable, intent(inout)    :: mat(:,:)
    integer, intent(in)                     :: n
    real(DP), intent(in)                    :: upper, diag, lower

    integer                             :: irow, icol

    do icol = 1, n
        do irow = 1, n
            if (irow == icol) then
                mat(irow, icol) = diag
            else
                if (irow == icol + 1) then
                    mat(irow, icol) = lower
                else if (irow == icol - 1) then
                    mat(irow, icol) = upper
                else
                    mat(irow, icol) = 0.
                end if
            end if
        end do
    end do
    end subroutine initialise_tridiagonal_dp
    

    end module initialisation
