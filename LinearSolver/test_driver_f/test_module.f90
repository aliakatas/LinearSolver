
    module test
    use custom_precision
    use comparison
    use pretty_print
    
    implicit none
    
    private
    
    integer        :: info, diffcount
    real           :: maxabsdiff
    real(DP)       :: maxabsdiff_dp
    integer        :: alloc_status
    real           :: tstart, tfinish
    
    interface test_tridiagonal_solver
    procedure test_tridiagonal_solver_sp
    procedure test_tridiagonal_solver_dp
    end interface
    
    interface test_tridiagonal_solver_xp
    procedure test_tridiagonal_solver_xp_sp
    procedure test_tridiagonal_solver_xp_dp
    end interface
    
    public test_tridiagonal_solver, test_tridiagonal_solver_xp
    
    contains
    
    !#####################################################
    subroutine test_tridiagonal_solver_sp(matrix, n, rhs, sol, err)
    real(SP), allocatable, intent(in)       :: matrix(:,:)
    integer, intent(in)                     :: n
    real(SP), allocatable, intent(in)       :: rhs(:), sol(:)
    integer, intent(out)                    :: err
    
    real(SP), allocatable                   :: d(:), dl(:), du(:), local_sol(:)
    
    integer                                 :: i
    
    err = 0
    
    allocate(d(N),      &
        dl(N - 1),      &
        du(N - 1),      &
        local_sol(N),   &
        stat=alloc_status)
    if (alloc_status) then
        write(*,*) "ERROR: Failed to allocate required arrays...!"
        write(*,*) "       Try reducing the size."
        err = -100
        return
    end if
    
    do i = 1, n
        d(i) = matrix(i, i)
        if (i > 1) dl(i - 1) = matrix(i, i - 1)
        if (i < n) du(i) = matrix(i, i + 1)
    end do
    
    local_sol(:) = rhs(:)
    
    call cpu_time(tstart)
    write(*,*) "Solving the system in SP..."
    call sgtsv(n, 1, dl, d, du, local_sol, n, info)
    if (info == 0) then 
        write(*,*) "Solution OK!"
    else 
        write(*,*) "Solution failed!"
        err = info
        goto 999
    end if
    call cpu_time(tfinish)
    write(*, '("sgtsv time = ",f6.3," seconds.")') tfinish - tstart
    
    write(*,*) "Comparing results..."
    call compare(sol, local_sol, diffcount, maxabsdiff, 1.e-5)
    if (diffcount > 0) then
        write(*,*) "Found ", diffcount, " differences in ", N, " values!"
        write(*,*) "Max absolute difference: ", maxabsdiff
    else 
        write(*,*) "Results match within tolerance!"
    end if
    call pretty_write(local_sol, 'sgtsv_out.log')
    
999 write(*,*) "********************"
    
    deallocate(d, dl, du, local_sol)
    end subroutine test_tridiagonal_solver_sp
    
    !#####################################################
    subroutine test_tridiagonal_solver_dp(matrix, n, rhs, sol, err)
    real(DP), allocatable, intent(in)       :: matrix(:,:)
    integer, intent(in)                     :: n
    real(DP), allocatable, intent(in)       :: rhs(:), sol(:)
    integer, intent(out)                    :: err
    
    real(DP), allocatable                   :: d(:), dl(:), du(:), local_sol(:)
    
    integer                                 :: i
    
    err = 0
    
    allocate(d(N),      &
        dl(N - 1),      &
        du(N - 1),      &
        local_sol(N),   &
        stat=alloc_status)
    if (alloc_status) then
        write(*,*) "ERROR: Failed to allocate required arrays...!"
        write(*,*) "       Try reducing the size."
        err = -100
        return
    end if
    
    do i = 1, n
        d(i) = matrix(i, i)
        if (i > 1) dl(i - 1) = matrix(i, i - 1)
        if (i < n) du(i) = matrix(i, i + 1)
    end do
    
    local_sol(:) = rhs(:)
    
    call cpu_time(tstart)
    write(*,*) "Solving the system in DP..."
    call dgtsv(n, 1, dl, d, du, local_sol, n, info)
    if (info == 0) then 
        write(*,*) "Solution OK!"
    else 
        write(*,*) "Solution failed!"
        err = info
        goto 998
    end if
    call cpu_time(tfinish)
    write(*, '("dgtsv time = ",f6.3," seconds.")') tfinish - tstart
    
    write(*,*) "Comparing results..."
    call compare(sol, local_sol, diffcount, maxabsdiff_dp, 1.d-5)
    if (diffcount > 0) then
        write(*,*) "Found ", diffcount, " differences in ", N, " values!"
        write(*,*) "Max absolute difference: ", maxabsdiff_dp
    else 
        write(*,*) "Results match within tolerance!"
    end if
    call pretty_write(local_sol, 'dgtsv_out.log')
    
998 write(*,*) "********************"
    
    deallocate(d, dl, du, local_sol)
    end subroutine test_tridiagonal_solver_dp
    
    !#####################################################
    subroutine test_tridiagonal_solver_xp_sp(matrix, n, rhs, sol, err)
    real(SP), allocatable, intent(in)       :: matrix(:,:)
    integer, intent(in)                     :: n
    real(SP), allocatable, intent(in)       :: rhs(:), sol(:)
    integer, intent(out)                    :: err
    
    real(SP), allocatable                   :: d(:), dl(:), du(:), local_sol(:)
    real(SP), allocatable                   :: DF(:), DLF(:), DUF(:), DU2(:)
    integer, allocatable                    :: IPIV(:)
    real(SP)                                :: rcond
    real(SP)                                :: ferr(1), berr(1)
    real(SP), allocatable                   :: work(:)
    integer, allocatable                    :: iwork(:)
    
    integer                                 :: i
    
    err = 0
    
    allocate(d(N),      &
        dl(N - 1),      &
        du(N - 1),      &
        local_sol(N),   &
        stat=alloc_status)
    if (alloc_status) then
        write(*,*) "ERROR: Failed to allocate required arrays...!"
        write(*,*) "       Try reducing the size."
        err = -100
        return
    end if
    
    do i = 1, n
        d(i) = matrix(i, i)
        if (i > 1) dl(i - 1) = matrix(i, i - 1)
        if (i < n) du(i) = matrix(i, i + 1)
    end do
    
    local_sol(:) = rhs(:)
    
    allocate(DF(N),     &
        DLF(N-1),       &
        DUF(N-1),       &
        DU2(N-2),       &
        IPIV(N),        &
        work(3 * N),    &
        iwork(N),       &
        stat=alloc_status)
    if (alloc_status) then
        write(*,*) "ERROR: Failed to allocate required arrays...!"
        write(*,*) "       Try reducing the size."
        err = -100
        goto 997
    end if
    
    call cpu_time(tstart)
    write(*,*) "Solving the system in SP (expert mode)..."
    call sgtsvx	('N', 'N', n, 1, dl, d, du, DLF, DF, DUF, DU2, IPIV, rhs, n, &
        local_sol, n, rcond, ferr, berr, work, iwork, info)
    if (info == 0) then 
        write(*,*) "Solution OK!"
    else 
        write(*,*) "Solution failed!"
        err = info
        goto 996
    end if
    call cpu_time(tfinish)
    write(*, '("sgtsvx time = ",f6.3," seconds.")') tfinish - tstart
    write(*, '("Backward error = ", f12.7)') berr(1)
    write(*, '("Forward error = ", f12.7)') ferr(1)
    
    write(*,*) "Comparing results..."
    call compare(sol, local_sol, diffcount, maxabsdiff, 1.e-5)
    if (diffcount > 0) then
        write(*,*) "Found ", diffcount, " differences in ", N, " values!"
        write(*,*) "Max absolute difference: ", maxabsdiff
    else 
        write(*,*) "Results match within tolerance!"
    end if
    call pretty_write(local_sol, 'sgtsvx_out.log')
    
996 deallocate(DF, DLF, DUF, DU2, IPIV, work, iwork)
997 write(*,*) "********************"
    
    deallocate(d, dl, du, local_sol)
    end subroutine test_tridiagonal_solver_xp_sp
    
    !#####################################################
    subroutine test_tridiagonal_solver_xp_dp(matrix, n, rhs, sol, err)
    real(DP), allocatable, intent(in)       :: matrix(:,:)
    integer, intent(in)                     :: n
    real(DP), allocatable, intent(in)       :: rhs(:), sol(:)
    integer, intent(out)                    :: err
    
    real(DP), allocatable                   :: d(:), dl(:), du(:), local_sol(:)
    real(DP), allocatable                   :: DF(:), DLF(:), DUF(:), DU2(:)
    integer, allocatable                    :: IPIV(:)
    real(DP)                                :: rcond
    real(DP)                                :: ferr(1), berr(1)
    real(DP), allocatable                   :: work(:)
    integer, allocatable                    :: iwork(:)
    
    integer                                 :: i
    
    err = 0
    
    allocate(d(N),      &
        dl(N - 1),      &
        du(N - 1),      &
        local_sol(N),   &
        stat=alloc_status)
    if (alloc_status) then
        write(*,*) "ERROR: Failed to allocate required arrays...!"
        write(*,*) "       Try reducing the size."
        err = -100
        return
    end if
    
    do i = 1, n
        d(i) = matrix(i, i)
        if (i > 1) dl(i - 1) = matrix(i, i - 1)
        if (i < n) du(i) = matrix(i, i + 1)
    end do
    
    local_sol(:) = rhs(:)
    
    allocate(DF(N),     &
        DLF(N-1),       &
        DUF(N-1),       &
        DU2(N-2),       &
        IPIV(N),        &
        work(3 * N),    &
        iwork(N),       &
        stat=alloc_status)
    if (alloc_status) then
        write(*,*) "ERROR: Failed to allocate required arrays...!"
        write(*,*) "       Try reducing the size."
        err = -100
        goto 995
    end if
    
    call cpu_time(tstart)
    write(*,*) "Solving the system in DP (expert mode)..."
    call dgtsvx	('N', 'N', n, 1, dl, d, du, DLF, DF, DUF, DU2, IPIV, rhs, n, &
        local_sol, n, rcond, ferr, berr, work, iwork, info)
    if (info == 0) then 
        write(*,*) "Solution OK!"
    else 
        write(*,*) "Solution failed!"
        err = info
        goto 994
    end if
    call cpu_time(tfinish)
    write(*, '("dgtsvx time = ",f6.3," seconds.")') tfinish - tstart
    write(*, '("Backward error = ", f12.7)') berr(1)
    write(*, '("Forward error = ", f12.7)') ferr(1)
    
    write(*,*) "Comparing results..."
    call compare(sol, local_sol, diffcount, maxabsdiff_dp, 1.d-5)
    if (diffcount > 0) then
        write(*,*) "Found ", diffcount, " differences in ", N, " values!"
        write(*,*) "Max absolute difference: ", maxabsdiff_dp
    else 
        write(*,*) "Results match within tolerance!"
    end if
    call pretty_write(local_sol, 'dgtsvx_out.log')
    
994 deallocate(DF, DLF, DUF, DU2, IPIV, work, iwork)
995 write(*,*) "********************"
    
    deallocate(d, dl, du, local_sol)
    end subroutine test_tridiagonal_solver_xp_dp
    
    
    end module test