program exercise_7_44
   use Environment
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, M = 0, N = 0, i
   real(R_), allocatable   :: B(:,:), max_vals_by_rows(:), max_vals_by_cols(:)
   
   open (file=input_file, newunit=In)
      read (In, *) M, N
      allocate (B(M, N))
      read (In, *) (B(i, :), i = 1, M)
   close (In)
   
   max_vals_by_cols = maxval(B, dim=1)
   max_vals_by_rows = maxval(B, dim=2)
   
   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, '(2(i0, 1x))') M, N
      write (Out, '('//N//'f8.2)') (B(i, :), i = 1, M)
      write (Out, '(a)') "Максимумы столбцов:"
      write (Out, '('//N//'f8.2)') max_vals_by_cols
      write (Out, '(a)') "Максимумы строк:"
      write (Out, '('//M//'f8.2)') max_vals_by_rows
   close (Out)
end program exercise_7_44
