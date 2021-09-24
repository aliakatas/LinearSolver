
module pretty_print

implicit none

interface pretty_write
procedure pretty_print_2D
procedure pretty_print_1D
end interface


contains

subroutine pretty_print_2D(mat)
real, allocatable, intent(in)       :: mat(:,:)

integer                 :: irow, icol, nrows, ncols

nrows = size(mat, 1)
ncols = size(mat, 2)

do irow = 1, nrows
    do icol = 1, ncols
        write(*, '(F10.4,X)', advance = 'no') mat(irow, icol)
    end do
    write(*,*)
end do
write(*,*) '-----------------'
end subroutine pretty_print_2D

subroutine pretty_print_1D(mat)
real, allocatable, intent(in)       :: mat(:)

integer                 :: irow, nrows

nrows = size(mat, 1)

do irow = 1, nrows
    write(*, '(F10.4,X)') mat(irow)
end do
write(*,*) '-----------------'
end subroutine pretty_print_1D

end module pretty_print
