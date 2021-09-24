
    module solver_preparation
    
    use custom_precision
    
    implicit none
    
    private
    
    integer                                     :: irow, icol, i 
    
    interface serialise_grid_nodes
    procedure serialise_grid_nodes_sp
    procedure serialise_grid_nodes_dp
    end interface
    
    interface map_vector_components_to_grid
    procedure map_vector_components_to_grid_sp
    procedure map_vector_components_to_grid_dp
    end interface
    
    interface fill_coefficient_matrix_poisson
    procedure fill_coefficient_matrix_poisson_sp
    procedure fill_coefficient_matrix_poisson_dp
    end interface 
    
    interface fill_coefficient_matrix_poisson_dirichlet_left
    procedure fill_coefficient_matrix_poisson_dirichlet_left_sp
    procedure fill_coefficient_matrix_poisson_dirichlet_left_dp
    end interface
    
    interface fill_coefficient_matrix_poisson_neumann_right
    procedure fill_coefficient_matrix_poisson_neumann_right_sp
    procedure fill_coefficient_matrix_poisson_neumann_right_dp
    end interface 
    
    interface fill_coefficient_matrix_poisson_dirichlet_bottom
    procedure fill_coefficient_matrix_poisson_dirichlet_bottom_sp
    procedure fill_coefficient_matrix_poisson_dirichlet_bottom_dp
    end interface 
    
    interface fill_coefficient_matrix_poisson_dirichlet_top
    procedure fill_coefficient_matrix_poisson_dirichlet_top_sp
    procedure fill_coefficient_matrix_poisson_dirichlet_top_dp
    end interface 
    
    public get_linear_index
    public serialise_grid_nodes
    public map_vector_components_to_grid
    public fill_coefficient_matrix_poisson
    public fill_coefficient_matrix_poisson_dirichlet_left
    public fill_coefficient_matrix_poisson_neumann_right
    public fill_coefficient_matrix_poisson_dirichlet_bottom
    public fill_coefficient_matrix_poisson_dirichlet_top
    
    contains
    
    !#####################################
    pure function get_row_id(i, stride) result(irow)
    integer, intent(in)         :: i, stride
    integer                     :: irow
    
    irow = mod(i, stride)
    end function get_row_id
    
    !#####################################
    pure function get_column_id(i, stride) result(icol)
    integer, intent(in)         :: i, stride
    integer                     :: icol
    
    icol = 1 + i / stride
    end function get_column_id
    
    !#####################################
    pure function get_linear_index(irow, icol, stride) result(id)
    integer, intent(in)         :: irow, icol, stride
    integer                     :: id
    
    id = (icol - 1) * stride + irow
    end function get_linear_index
    
    !#####################################
    subroutine serialise_grid_nodes_sp(grid, vector, nrows, ncols)
    real(SP), allocatable, intent(in)           :: grid(:,:)
    real(SP), allocatable, intent(inout)        :: vector(:)
    integer, intent(in)                         :: nrows, ncols
    
    i = 1
    do icol = 1, ncols
        do irow = 1, nrows
            vector(i) = grid(irow, icol)
            i = i + 1
        end do
    end do
    end subroutine serialise_grid_nodes_sp
    
    !#####################################
    subroutine serialise_grid_nodes_dp(grid, vector, nrows, ncols)
    real(DP), allocatable, intent(in)           :: grid(:,:)
    real(DP), allocatable, intent(inout)        :: vector(:)
    integer, intent(in)                         :: nrows, ncols
    
    i = 1
    do icol = 1, ncols
        do irow = 1, nrows
            vector(i) = grid(irow, icol)
            i = i + 1
        end do
    end do
    end subroutine serialise_grid_nodes_dp
    
    !#####################################
    subroutine map_vector_components_to_grid_sp(vector, n, grid, nrows, ncols)
    real(SP), allocatable, intent(in)           :: vector(:)
    real(SP), allocatable, intent(inout)        :: grid(:,:)
    integer, intent(in)                         :: n, nrows, ncols
    
    do i = 1, n
        irow = get_row_id(i, nrows)
        icol = get_column_id(i, nrows)
        grid(irow, icol) = vector(i)
    end do
    end subroutine map_vector_components_to_grid_sp
    
    !#####################################
    subroutine map_vector_components_to_grid_dp(vector, n, grid, nrows, ncols)
    real(DP), allocatable, intent(in)           :: vector(:)
    real(DP), allocatable, intent(inout)        :: grid(:,:)
    integer, intent(in)                         :: n, nrows, ncols
    
    do i = 1, n
        irow = get_row_id(i, nrows)
        icol = get_column_id(i, nrows)
        grid(irow, icol) = vector(i)
    end do
    end subroutine map_vector_components_to_grid_dp
    
    !#####################################
    subroutine fill_coefficient_matrix_poisson_sp(mat, nrows, ncols, dx, dy)
    real(SP), allocatable, intent(inout)        :: mat(:,:)
    integer, intent(in)                         :: nrows, ncols
    real(SP), intent(in)                        :: dx, dy
    
    real(SP)                                    :: dx2, dy2
    integer                                     :: k, n
    
    dx2 = dx * dx
    dy2 = dy * dy
    mat(:,:) = 0.
    n = nrows * ncols
    
    do icol = 2, ncols - 1
        do irow = 2, nrows - 1
            k = get_linear_index(irow, icol, nrows)
            mat(k, k) = -((2. / dx2) + (2. / dy2))
            
            mat(k, k - 1) = 1. / dy2
            mat(k, k + 1) = 1. / dy2
            
            mat(k, k - nrows) = 1. / dx2
            mat(k, k + nrows) = 1. / dx2
        end do
    end do
    end subroutine fill_coefficient_matrix_poisson_sp
    
    !#####################################
    subroutine fill_coefficient_matrix_poisson_dp(mat, nrows, ncols, dx, dy)
    real(DP), allocatable, intent(inout)        :: mat(:,:)
    integer, intent(in)                         :: nrows, ncols
    real(DP), intent(in)                        :: dx, dy
    
    real(DP)                                    :: dx2, dy2
    integer                                     :: k, n
    
    dx2 = dx * dx
    dy2 = dy * dy
    mat(:,:) = 0.d0
    n = nrows * ncols
    
    do icol = 2, ncols - 1
        do irow = 2, nrows - 1
            k = get_linear_index(irow, icol, nrows)
            mat(k, k) = -((2.d0 / dx2) + (2.d0 / dy2))
            
            mat(k, k - 1) = 1.d0 / dy2
            mat(k, k + 1) = 1.d0 / dy2
            
            mat(k, k - nrows) = 1.d0 / dx2
            mat(k, k + nrows) = 1.d0 / dx2
        end do
    end do
    end subroutine fill_coefficient_matrix_poisson_dp
    
    !#####################################
    subroutine fill_coefficient_matrix_poisson_dirichlet_left_sp(mat, nrows, ncols)
    real(SP), allocatable, intent(inout)        :: mat(:,:)
    integer, intent(in)                         :: nrows, ncols
    
    integer                                     :: k
    
    icol = 1
    do irow = 1, nrows
        k = get_linear_index(irow, icol, nrows)
        mat(k, k) = 1.
    end do
    end subroutine fill_coefficient_matrix_poisson_dirichlet_left_sp
    
    !#####################################
    subroutine fill_coefficient_matrix_poisson_dirichlet_left_dp(mat, nrows, ncols)
    real(DP), allocatable, intent(inout)        :: mat(:,:)
    integer, intent(in)                         :: nrows, ncols
    
    integer                                     :: k
    
    icol = 1
    do irow = 1, nrows
        k = get_linear_index(irow, icol, nrows)
        mat(k, k) = 1.d0
    end do
    end subroutine fill_coefficient_matrix_poisson_dirichlet_left_dp
    
    !#####################################
    subroutine fill_coefficient_matrix_poisson_neumann_right_sp(mat, nrows, ncols, dx, dy)
    real(SP), allocatable, intent(inout)        :: mat(:,:)
    integer, intent(in)                         :: nrows, ncols
    real(SP), intent(in)                        :: dx, dy
    
    integer                                     :: k
    real(SP)                                    :: dx2, dy2
    
    dx2 = dx * dx
    dy2 = dy *dy
    
    icol = ncols
    do irow = 2, nrows - 1
        k = get_linear_index(irow, icol, nrows)
        mat(k, k) = -(7. / (2. * dx2) + (2. / dy2))
        mat(k, k - 1) = 1. / dy2
        mat(k, k + 1) = 1. / dy2
        mat(k, k - nrows) = 4. / dx2
        mat(k, k + nrows) = -1. / (2. * dx2)
    end do
    end subroutine fill_coefficient_matrix_poisson_neumann_right_sp
    
    !#####################################
    subroutine fill_coefficient_matrix_poisson_neumann_right_dp(mat, nrows, ncols, dx, dy)
    real(DP), allocatable, intent(inout)        :: mat(:,:)
    integer, intent(in)                         :: nrows, ncols
    real(DP), intent(in)                        :: dx, dy
    
    integer                                     :: k
    real(DP)                                    :: dx2, dy2
    
    dx2 = dx * dx
    dy2 = dy *dy
    
    icol = ncols
    do irow = 2, nrows - 1
        k = get_linear_index(irow, icol, nrows)
        mat(k, k) = -(7.d0 / (2.d0 * dx2) + (2.d0 / dy2))
        mat(k, k - 1) = 1.d0 / dy2
        mat(k, k + 1) = 1.d0 / dy2
        mat(k, k - nrows) = 4.d0 / dx2
        mat(k, k + nrows) = -1.d0 / (2.d0 * dx2)
    end do
    end subroutine fill_coefficient_matrix_poisson_neumann_right_dp
    
    !#####################################
    subroutine fill_coefficient_matrix_poisson_dirichlet_bottom_sp(mat, nrows, ncols)
    real(SP), allocatable, intent(inout)        :: mat(:,:)
    integer, intent(in)                         :: nrows, ncols
    
    integer                                     :: k
    
    irow = 1
    do icol = 1, ncols
        k = get_linear_index(irow, icol, nrows)
        mat(k, k) = 1.
    end do
    end subroutine fill_coefficient_matrix_poisson_dirichlet_bottom_sp
    
    !#####################################
    subroutine fill_coefficient_matrix_poisson_dirichlet_bottom_dp(mat, nrows, ncols)
    real(DP), allocatable, intent(inout)        :: mat(:,:)
    integer, intent(in)                         :: nrows, ncols
    
    integer                                     :: k
    
    irow = 1
    do icol = 1, ncols
        k = get_linear_index(irow, icol, nrows)
        mat(k, k) = 1.d0
    end do
    end subroutine fill_coefficient_matrix_poisson_dirichlet_bottom_dp
    
    !#####################################
    subroutine fill_coefficient_matrix_poisson_dirichlet_top_sp(mat, nrows, ncols)
    real(SP), allocatable, intent(inout)        :: mat(:,:)
    integer, intent(in)                         :: nrows, ncols
    
    integer                                     :: k
    
    irow = nrows
    do icol = 1, ncols
        k = get_linear_index(irow, icol, nrows)
        mat(k, k) = 1.
    end do
    end subroutine fill_coefficient_matrix_poisson_dirichlet_top_sp
    
    !#####################################
    subroutine fill_coefficient_matrix_poisson_dirichlet_top_dp(mat, nrows, ncols)
    real(DP), allocatable, intent(inout)        :: mat(:,:)
    integer, intent(in)                         :: nrows, ncols
    
    integer                                     :: k
    
    irow = nrows
    do icol = 1, ncols
        k = get_linear_index(irow, icol, nrows)
        mat(k, k) = 1.d0
    end do
    end subroutine fill_coefficient_matrix_poisson_dirichlet_top_dp
    
    end module solver_preparation