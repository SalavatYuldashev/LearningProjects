program exercise_7_21
   use Environment
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, M = 0, N = 0, i, j, min_ind
   real(R_), allocatable   :: B(:,:), max_vals(:)
   real(R_)                :: temp
   
   open (file=input_file, newunit=In)
      read (In, *) M, N
      allocate (B(M, N))
      read (In, *) (B(i, :), i = 1, M)
   close (In)
   
   max_vals = maxval(B, dim=2)
   
   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, '(2(i0, 1x))') M, N
      write (Out, '('//N//'f8.2)') (B(i, :), i = 1, M)
      write (Out, '(a)') "Максимумы строк:"
      write (Out, '('//M//'f8.2)') max_vals
   close (Out)
   
   do i = 1, M-1
      min_ind = minloc(max_vals(i:), 1) + i - 1
      if (max_vals(i) > max_vals(min_ind)) then
        temp = max_vals(i)
        max_vals(i) = max_vals(min_ind)
        max_vals(min_ind) = temp
        do j = 1, N
           temp = B(i, j)
           B(i, j) = B(min_ind, j)
           B(min_ind, j) = temp
        end do
      end if
    end do
   
   open (file=output_file, encoding=E_, newunit=Out, position="append")
      write (Out, '(a)') "Матрица после сортировки:"
      write (Out, '('//N//'f8.2)') (B(i, :), i = 1, M)
   close (Out)
end program exercise_7_21
