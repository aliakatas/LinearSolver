
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
    
    public serialise_grid_nodes
    public map_vector_components_to_grid
    
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
    subroutine serialise_grid_nodes_sp(grid, vector, nrows, ncols)
    real(SP), allocatable, intent(in)           :: grid(:,:)
    real(SP), allocatable, intent(out)          :: vector(:)
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
    real(DP), allocatable, intent(out)          :: vector(:)
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
    real(SP), allocatable, intent(out)          :: grid(:,:)
    integer, intent(in)                         :: n, nrows, ncols
    
    do i = 1, n
        irow = get_row_id(i, nrows)
        icol = get_column_id(i, ncols)
        grid(irow, icol) = vector(i)
    end do
    end subroutine map_vector_components_to_grid_sp
    
    !#####################################
    subroutine map_vector_components_to_grid_dp(vector, n, grid, nrows, ncols)
    real(DP), allocatable, intent(in)           :: vector(:)
    real(DP), allocatable, intent(out)          :: grid(:,:)
    integer, intent(in)                         :: n, nrows, ncols
    
    do i = 1, n
        irow = get_row_id(i, nrows)
        icol = get_column_id(i, ncols)
        grid(irow, icol) = vector(i)
    end do
    end subroutine map_vector_components_to_grid_dp
    
    end module solver_preparation