
    module pretty_print
    
    use custom_precision

    implicit none
    
    private
    character(11)       :: float_format = '(F10.4,X)'
    character(18)       :: separator = '-----------------'
    character(4)        :: separator_format = '(A)'
    integer             :: fid
    

    interface pretty_write
    procedure pretty_print_2D
    procedure pretty_print_1D
    procedure pretty_write_2D
    procedure pretty_write_1D
    procedure pretty_print_2D_dp
    procedure pretty_print_1D_dp
    procedure pretty_write_2D_dp
    procedure pretty_write_1D_dp
    end interface
    
    interface export_to_ascii
    procedure export_to_ascii_sp
    procedure export_to_ascii_dp
    procedure export_to_ascii_int
    end interface
    
    public pretty_write
    public export_to_ascii
    
    contains
    
    !#############################################
    subroutine pretty_print_2D(mat)
    real, allocatable, intent(in)       :: mat(:,:)

    integer                 :: irow, icol, nrows, ncols

    nrows = size(mat, 1)
    ncols = size(mat, 2)

    do irow = 1, nrows
        do icol = 1, ncols
            write(*, float_format, advance = 'no') mat(irow, icol)
        end do
        write(*,*)
    end do
    write(*, separator_format) separator
    end subroutine pretty_print_2D
    
    !#############################################
    subroutine pretty_print_1D(mat)
    real, allocatable, intent(in)       :: mat(:)

    integer                 :: irow, nrows

    nrows = size(mat, 1)

    do irow = 1, nrows
        write(*, float_format) mat(irow)
    end do
    write(*, separator_format) separator
    end subroutine pretty_print_1D
    
    !#############################################
    subroutine pretty_write_2D(mat, fname)
    real, allocatable, intent(in)       :: mat(:,:)
    character(*), intent(in)            :: fname

    integer                 :: irow, icol, nrows, ncols

    nrows = size(mat, 1)
    ncols = size(mat, 2)
    
    open(newunit=fid, file=fname, status='unknown')
    write(fid, *) nrows, ncols
    do irow = 1, nrows
        do icol = 1, ncols
            write(fid, float_format, advance = 'no') mat(irow, icol)
        end do
        write(fid,*)
    end do
    write(fid, separator_format) separator
    close(fid)
    end subroutine pretty_write_2D
    
    !#############################################
    subroutine pretty_write_1D(mat, fname)
    real, allocatable, intent(in)       :: mat(:)
    character(*), intent(in)            :: fname

    integer                 :: irow, nrows

    nrows = size(mat, 1)
    
    open(newunit=fid, file=fname, status='unknown')
    write(fid, *) nrows
    do irow = 1, nrows
        write(fid, float_format) mat(irow)
    end do
    write(fid, separator_format) separator
    close(fid)
    end subroutine pretty_write_1D
    
    !#############################################
    subroutine pretty_print_2D_dp(mat)
    real(DP), allocatable, intent(in)       :: mat(:,:)

    integer                 :: irow, icol, nrows, ncols

    nrows = size(mat, 1)
    ncols = size(mat, 2)

    do irow = 1, nrows
        do icol = 1, ncols
            write(*, float_format, advance = 'no') mat(irow, icol)
        end do
        write(*,*)
    end do
    write(*, separator_format) separator
    end subroutine pretty_print_2D_dp
    
    !#############################################
    subroutine pretty_print_1D_dp(mat)
    real(DP), allocatable, intent(in)       :: mat(:)

    integer                 :: irow, nrows

    nrows = size(mat, 1)

    do irow = 1, nrows
        write(*, float_format) mat(irow)
    end do
    write(*, separator_format) separator
    end subroutine pretty_print_1D_dp
    
    !#############################################
    subroutine pretty_write_2D_dp(mat, fname)
    real(DP), allocatable, intent(in)       :: mat(:,:)
    character(*), intent(in)            :: fname

    integer                 :: irow, icol, nrows, ncols

    nrows = size(mat, 1)
    ncols = size(mat, 2)
    
    open(newunit=fid, file=fname, status='unknown')
    write(fid, *) nrows, ncols
    do irow = 1, nrows
        do icol = 1, ncols
            write(fid, float_format, advance = 'no') mat(irow, icol)
        end do
        write(fid,*)
    end do
    write(fid, separator_format) separator
    close(fid)
    end subroutine pretty_write_2D_dp
    
    !#############################################
    subroutine pretty_write_1D_dp(mat, fname)
    real(DP), allocatable, intent(in)       :: mat(:)
    character(*), intent(in)            :: fname

    integer                 :: irow, nrows

    nrows = size(mat, 1)
    
    open(newunit=fid, file=fname, status='unknown')
    write(fid, *) nrows
    do irow = 1, nrows
        write(fid, float_format) mat(irow)
    end do
    write(fid, separator_format) separator
    close(fid)
    end subroutine pretty_write_1D_dp
    
    !#############################################
    subroutine export_to_ascii_sp(fname, nrows, ncols, d, nodata, xll, yll, dx)
    character(*), intent(in)                :: fname
    integer, intent(in)                     :: nrows, ncols
    real(SP), allocatable, intent(in)       :: d(:,:) 
    real(SP), intent(in)                    :: nodata
    real(SP), intent(in)                    :: xll, yll, dx
    
    integer                                 :: irow, icol
    
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
    end subroutine export_to_ascii_sp
    
    !#############################################
    subroutine export_to_ascii_dp(fname, nrows, ncols, d, nodata, xll, yll, dx)
    character(*), intent(in)                :: fname
    integer, intent(in)                     :: nrows, ncols
    real(DP), allocatable, intent(in)       :: d(:,:) 
    real(DP), intent(in)                    :: nodata
    real(DP), intent(in)                    :: xll, yll, dx
    
    integer                                 :: irow, icol
    
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
    end subroutine export_to_ascii_dp
    
    !#############################################
    subroutine export_to_ascii_int(fname, nrows, ncols, d, nodata, xll, yll, dx)
    character(*), intent(in)                :: fname
    integer, intent(in)                     :: nrows, ncols
    integer, allocatable, intent(in)        :: d(:,:) 
    integer, intent(in)                     :: nodata
    real, intent(in)                        :: xll, yll, dx
    
    integer                                 :: irow, icol
    
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
    end subroutine export_to_ascii_int

    end module pretty_print
