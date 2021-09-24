
    module comparison
    use custom_precision
    
    implicit none
    
    private
    
    real, parameter         :: TOLERANCE = 1.e-6
    
    interface compare
    procedure compare_sp
    procedure compare_dp
    end interface 
    
    public compare
    
    contains
    
    !####################################
    subroutine compare_sp(ref, chk, diffcount, maxabsdiff, tol)
    real, allocatable, intent(in)       :: ref(:), chk(:)
    integer, intent(out)                :: diffcount
    real, intent(out)                   :: maxabsdiff
    real, optional, intent(in)          :: tol
    
    integer                             :: i, n
    real                                :: wtol, diff
    
    diffcount = 0
    maxabsdiff = 0.
    n = size(ref, 1)
    
    if (n /= size(chk, 1)) then
        diffcount = -1
        return
    end if
    
    wtol = TOLERANCE
    if (present(tol)) wtol = tol
    
    do i = 1, n
        diff = abs(ref(i) - chk(i))
        if (diff > wtol) then 
            diffcount = diffcount + 1
            maxabsdiff = max(maxabsdiff, diff)
        end if
    end do
    
    end subroutine compare_sp
    
    !####################################
    subroutine compare_dp(ref, chk, diffcount, maxabsdiff, tol)
    real(DP), allocatable, intent(in)       :: ref(:), chk(:)
    integer, intent(out)                    :: diffcount
    real(DP), intent(out)                   :: maxabsdiff
    real(DP), optional, intent(in)          :: tol
    
    integer                                 :: i, n
    real(DP)                                :: wtol, diff
    
    diffcount = 0
    maxabsdiff = 0.
    n = size(ref, 1)
    
    if (n /= size(chk, 1)) then
        diffcount = -1
        return
    end if
    
    wtol = TOLERANCE
    if (present(tol)) wtol = tol
    
    do i = 1, n
        diff = abs(ref(i) - chk(i))
        if (diff > wtol) then 
            diffcount = diffcount + 1
            maxabsdiff = max(maxabsdiff, diff)
        end if
    end do
    
    end subroutine compare_dp
    
    end module comparison