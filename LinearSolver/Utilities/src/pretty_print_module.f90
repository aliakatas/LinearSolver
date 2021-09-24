
    module pretty_print

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
    end interface
    
    public pretty_write
    
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

    end module pretty_print
