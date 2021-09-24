
    
    program hello
    use custom_precision
    use initialisation
    use pretty_print
    use test
    
    implicit none
    
    integer, parameter          :: NMAX = 6
    integer, parameter          :: MAX_EXPORT_SIZE = 2000
    
#ifdef _DP    
    real(DP), parameter         :: ST = -5., fn = 9.
    real(DP), parameter         :: DIAG_COEF = 2., UDIAG_COEF = -1., LDIAG_COEF = 1.
    real(DP), allocatable       :: matrix(:,:), vector_solution(:), vector_rhs(:), vector_rhs_blas(:)
#else
    real(SP), parameter         :: ST = -5., fn = 9.
    real(SP), parameter         :: DIAG_COEF = 2., UDIAG_COEF = -1., LDIAG_COEF = 1.
    real(SP), allocatable       :: matrix(:,:), vector_solution(:), vector_rhs(:), vector_rhs_blas(:)
#endif    
    integer                     :: alloc_status 
    real                        :: tstart, tfinish, tstart_total
    integer                     :: err
    integer                     :: N, nargs
    character(10)               :: arg
    
    nargs = command_argument_count()
    if (nargs > 0) then
        call get_command_argument(1, arg)
        read(arg, *) N
    else
        N = NMAX
    end if
    
    write(*,*) ''
    write(*,*) "Working with size ", N
#ifdef _DP
    write(*,*) "Double precision"
#else
    write(*,*) "Single precision"
#endif    
    write(*,*) ''    

    ! Start the timer...
    call cpu_time(tstart_total)

    allocate(matrix(N, N),  &
        vector_solution(N), &
        vector_rhs(N),      &
        vector_rhs_blas(N), &
        stat=alloc_status)
    
    if (alloc_status) then
        write(*,*) "ERROR: Failed to allocate required arrays...!"
        write(*,*) "       Try reducing the size."
        goto 100
    end if

    write(*,*) "Initialising arrays..."
    call initialise_tridiagonal(matrix, N, UDIAG_COEF, DIAG_COEF, LDIAG_COEF)
    call initialise_to_random(vector_solution, N, st, fn)
    
    call cpu_time(tfinish)
    write(*, '("Initialisation time = ",f6.3," seconds.")') tfinish - tstart_total
    
    write(*,*) "Getting correct result for reference..."
    call cpu_time(tstart)
    vector_rhs = matmul(matrix, vector_solution)
    call cpu_time(tfinish)
    write(*, '("matmul time = ",f6.3," seconds.")') tfinish - tstart
    
    call cpu_time(tstart)
    if (kind(matrix) == kind(1.)) then 
        call sgemv('N', N, N, 1., matrix, N, vector_solution, 1, 0., vector_rhs_blas, 1)
    else 
        call dgemv('N', N, N, 1.d0, matrix, N, vector_solution, 1, 0.d0, vector_rhs_blas, 1)
    end if
    call cpu_time(tfinish)
    write(*, '("?gemv time = ",f6.3," seconds.")') tfinish - tstart
    
    if (N < MAX_EXPORT_SIZE) then
        write(*,*) ''
        write(*,*) "Saving arrays..."
        call cpu_time(tstart)
        call pretty_write(matrix, 'matrix.log')
        call pretty_write(vector_solution, 'solution.log')
        call pretty_write(vector_rhs, 'vector_rhs.log')
        call pretty_write(vector_rhs_blas, 'vector_rhs_blas.log')
        call cpu_time(tfinish)
        write(*, '("Writing time = ",f6.3," seconds.")') tfinish - tstart
    end if
    
    write(*,*) ''
    call test_tridiagonal_solver(matrix, N, vector_rhs_blas, vector_solution, err)
    if (err /= 0) then
        write(*,*) "ERROR: Failed to complete test...!"
        goto 100
    end if
    
    write(*,*) ''
    call test_tridiagonal_solver_xp(matrix, N, vector_rhs_blas, vector_solution, err)
    if (err /= 0) then
        write(*,*) "ERROR: Failed to complete test...!"
        goto 100
    end if
    
    call cpu_time(tfinish)
    write(*, '("Total run time = ",f6.3," seconds.")') tfinish - tstart_total
    
100 write(*,*) "Done!"
#ifdef _VSBUILD    
    write(*,*) "Press Enter to exit..."
    read(*,*)
#endif 
    end program

