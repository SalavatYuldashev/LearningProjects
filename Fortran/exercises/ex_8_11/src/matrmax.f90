module matrmax
    use Environment
    implicit none

contains
    pure subroutine matr_max(A, maxim)
        implicit none
        integer   :: W, H
        real(R_), intent(in)  :: A(:,:)
        real(R_), intent(out) :: maxim
        integer  :: i, j
        
        W = ubound(A,1)
        H = ubound(A,2)
        maxim = A(1, 1)
		
        do i = 1, W
           do j = 1, H
              if (A(i,j) > maxim) then
                 maxim = A(i,j)                
              end if
           end do
        end do
    end subroutine matr_max
end module matrmax