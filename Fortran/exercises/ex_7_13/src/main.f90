program exercise_7_13
   use Environment
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, M = 0, N = 0, &
                              i, min_sum_ind, min_ind, max_ind 
   real(R_), allocatable   :: B(:,:), S(:)
   real(R_)                :: min_val, max_val
   
   open (file=input_file, newunit=In)
      read (In, *) M, N
      allocate (B(M, N))
      read (In, *) (B(i, :), i = 1, M)
   close (In)
   
   S = sum(B, dim=2)
   min_sum_ind = minloc(S, dim=1)
   min_ind = minloc(B(min_sum_ind, :), dim=1)
   max_ind = maxloc(B(min_sum_ind, :), dim=1)
   min_val = minval(B(min_sum_ind, :), dim=1)
   max_val = maxval(B(min_sum_ind, :), dim=1)
   
   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, '(2(i0, 1x))') M, N
      write (Out, '('//N//'f8.2)') (B(i, :), i = 1, M)
      write (Out, '(a)') "Суммы строк:"
      write (Out, '('//N//'f8.2)') S
      write (Out, '(a, i0)') "Строка с минимальной суммой: ", min_sum_ind
      write (Out, '(2(a, i0, 1x, a, f6.2, 1x))') "Индекс минимума: ", &
         min_ind, "Значение минимума: ", min_val, "Индекс максимума: ", &
         max_ind, "Значение максимума: ", max_val
   close (Out)
end program exercise_7_13
