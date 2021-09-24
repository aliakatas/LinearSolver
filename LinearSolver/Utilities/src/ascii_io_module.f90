
    module ascii_io
    
    use custom_precision
    
    implicit none
    
    private
    
    integer, parameter  :: N_HEADER_LINES = 6
    character(11)       :: float_format = '(F10.4,X)'
    integer             :: fid
    integer             :: irow, icol, i
    character(13)       :: dummy_txt
    
    interface export_to_ascii
    procedure export_to_ascii_sp
    procedure export_to_ascii_dp
    procedure export_to_ascii_int
    end interface
    
    interface read_from_ascii_header
    procedure read_from_ascii_header_sp
    procedure read_from_ascii_header_dp
    end interface
    
    interface read_from_ascii_data
    procedure read_from_ascii_data_sp
    procedure read_from_ascii_data_dp
    procedure read_from_ascii_data_int
    end interface 
    
    public export_to_ascii
    public read_from_ascii_header
    public read_from_ascii_data
    
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
    
    !#############################################
    subroutine read_from_ascii_header_sp(fname, nrows, ncols, nodata, xll, yll, dx)
    character(*), intent(in)                :: fname
    integer, intent(out)                    :: nrows, ncols
    real(SP), intent(out)                   :: nodata
    real(SP), intent(out)                   :: xll, yll, dx
    
    open(newunit=fid, file=fname, status='unknown')
    
    read(fid, *) dummy_txt, ncols
    read(fid, *) dummy_txt, nrows
    read(fid, *) dummy_txt, xll
    read(fid, *) dummy_txt, yll
    read(fid, *) dummy_txt, dx
    read(fid, *) dummy_txt, nodata
    close(fid)
    end subroutine read_from_ascii_header_sp
    
    !#############################################
    subroutine read_from_ascii_header_dp(fname, nrows, ncols, nodata, xll, yll, dx)
    character(*), intent(in)                :: fname
    integer, intent(out)                    :: nrows, ncols
    real(DP), intent(out)                   :: nodata
    real(DP), intent(out)                   :: xll, yll, dx
    
    open(newunit=fid, file=fname, status='unknown')
    
    read(fid, *) dummy_txt, ncols
    read(fid, *) dummy_txt, nrows
    read(fid, *) dummy_txt, xll
    read(fid, *) dummy_txt, yll
    read(fid, *) dummy_txt, dx
    read(fid, *) dummy_txt, nodata
    close(fid)
    end subroutine read_from_ascii_header_dp
    
    !#############################################
    subroutine read_from_ascii_data_sp(fname, d, nrows, ncols)
    character(*), intent(in)                :: fname
    real(SP), allocatable, intent(out)      :: d(:,:)
    integer, intent(in)                     :: nrows, ncols
    
    open(newunit=fid, file=fname, status='unknown')
    
    do i = 1, N_HEADER_LINES
        read(fid, *) dummy_txt
    end do
    
    do irow = nrows, 1, -1
        read(fid, *) d(irow, :)     
    end do
    close(fid)
    end subroutine read_from_ascii_data_sp
    
    !#############################################
    subroutine read_from_ascii_data_dp(fname, d, nrows, ncols)
    character(*), intent(in)                :: fname
    real(DP), allocatable, intent(out)      :: d(:,:)
    integer, intent(in)                     :: nrows, ncols
    
    open(newunit=fid, file=fname, status='unknown')
    
    do i = 1, N_HEADER_LINES
        read(fid, *) dummy_txt
    end do
    
    do irow = nrows, 1, -1
        read(fid, *) d(irow, :)     
    end do
    close(fid)
    end subroutine read_from_ascii_data_dp
    
    !#############################################
    subroutine read_from_ascii_data_int(fname, d, nrows, ncols)
    character(*), intent(in)                :: fname
    integer, allocatable, intent(out)       :: d(:,:)
    integer, intent(in)                     :: nrows, ncols
    
    open(newunit=fid, file=fname, status='unknown')
    
    do i = 1, N_HEADER_LINES
        read(fid, *) dummy_txt
    end do
    
    do irow = nrows, 1, -1
        read(fid, *) d(irow, :)     
    end do
    close(fid)
    end subroutine read_from_ascii_data_int
    
    end module ascii_io