
program hello
    use initialisation
    use pretty_print
    implicit none

    real, allocatable       :: matrix(:,:), vector_original(:), vector_result(:), vector_calculated(:)
    integer                 :: N
    real                    :: st, fn

    N = 6

    allocate(matrix(N, N))
    allocate(vector_original(N))
    allocate(vector_result(N))
    allocate(vector_calculated(N))

    st = -5.
    fn = 9.

    call initialise_tridiagonal(matrix, N, 1., -2., 1.)
    call initialise_to_random(vector_original, N, st, fn)

    vector_result = matmul(matrix, vector_original)

    print *, "Hello World!"




    print *, "Done!"

end program

