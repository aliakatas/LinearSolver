
    module ascii_io
    
    use custom_precision
    
    implicit none
    
    private
    
    character(11)       :: float_format = '(F10.4,X)'
    integer             :: fid
    integer             :: irow, icol
    
    interface export_to_ascii
    procedure export_to_ascii_sp
    procedure export_to_ascii_dp
    procedure export_to_ascii_int
    end interface
    
    public export_to_ascii
    
    contains 
    
    !#############################################
    subroutine export_to_ascii_sp(fname, nrows, ncols, d, nodata, xll, yll, dx)
    character(*), intent(in)                :: fname
    integer, intent(in)                     :: nrows, ncols
    real(SP), allocatable, intent(in)       :: d(:,:) 
    real(SP), intent(in)                    :: nodata
    real(SP), intent(in)                    :: xll, yll, dx
    
    open(newunit=fid, file=fname, status='unknown')
    
    write(fid, '("ncols          ", I10)') ncols
    write(fid, '("nrows          ", I10)') nrows
    write(fid, '("xllcorner      ", F12.4)') xll
    write(fid, '("yllcorner      ", F12.4)') yll
    write(fid, '("cellsize       ", F12.4)') dx
    write(fid, '("NODATA_value   ", F12.4)') nodata
    
    do irow = nrows, 1, -1
        do icol = 1, ncols
            write(fid, float_format) d(irow, icol)
        end do
        write(fid, *) ''
    end do
    close(fid)
    end subroutine export_to_ascii_sp
    
    !#############################################
    subroutine export_to_ascii_dp(fname, nrows, ncols, d, nodata, xll, yll, dx)
    character(*), intent(in)                :: fname
    integer, intent(in)                     :: nrows, ncols
    real(DP), allocatable, intent(in)       :: d(:,:) 
    real(DP), intent(in)                    :: nodata
    real(DP), intent(in)                    :: xll, yll, dx
    
    open(newunit=fid, file=fname, status='unknown')
    
    write(fid, '("ncols          ", I10)') ncols
    write(fid, '("nrows          ", I10)') nrows
    write(fid, '("xllcorner      ", F12.4)') xll
    write(fid, '("yllcorner      ", F12.4)') yll
    write(fid, '("cellsize       ", F12.4)') dx
    write(fid, '("NODATA_value   ", F12.4)') nodata
    
    do irow = nrows, 1, -1
        do icol = 1, ncols
            write(fid, float_format) d(irow, icol)
        end do
        write(fid, *) ''
    end do
    close(fid)
    end subroutine export_to_ascii_dp
    
    !#############################################
    subroutine export_to_ascii_int(fname, nrows, ncols, d, nodata, xll, yll, dx)
    character(*), intent(in)                :: fname
    integer, intent(in)                     :: nrows, ncols
    integer, allocatable, intent(in)        :: d(:,:) 
    integer, intent(in)                     :: nodata
    real, intent(in)                        :: xll, yll, dx
    
    open(newunit=fid, file=fname, status='unknown')
    
    write(fid, '("ncols          ", I10)') ncols
    write(fid, '("nrows          ", I10)') nrows
    write(fid, '("xllcorner      ", F12.4)') xll
    write(fid, '("yllcorner      ", F12.4)') yll
    write(fid, '("cellsize       ", F12.4)') dx
    write(fid, '("NODATA_value   ", I10)') nodata
    
    do irow = nrows, 1, -1
        do icol = 1, ncols
            write(fid, '(I10,1X)') d(irow, icol)
        end do
        write(fid, *) ''
    end do
    close(fid)
    end subroutine export_to_ascii_int
    
    end module ascii_io