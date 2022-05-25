program exercise_7_21
   use Environment
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, M = 0, N = 0, i
   integer, allocatable    :: min_inds(:), max_inds(:)
   real(R_), allocatable   :: B(:,:)
   real(R_)                :: min_val, max_val
   
   open (file=input_file, newunit=In)
      read (In, *) M, N
      allocate (B(M, N))
      read (In, *) (B(i, :), i = 1, M)
   close (In)
   
   max_val = maxval(B, mask=B .LT. 0)
   min_val = minval(B, mask=B .GT. 0)
   max_inds = pack(reshape([(i, i=1,size(B))], shape(B)), B==max_val)
   min_inds = pack(reshape([(i, i=1,size(B))], shape(B)), B==min_val)
   
   print *, max_inds
   
   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, '(2(i0, 1x))') M, N
      write (Out, '('//N//'f8.2)') (B(i, :), i = 1, M)
      write (Out, '(2(a, f8.2, 1x))') "Значение минимума: ", min_val, & 
         "Значение максимума: ", max_val
      write (Out, '(a)') "Индексы максимумов:"
      write (Out, '('//size(max_inds)//'(a, i0, a, i0, a))') ("(", max_inds(i) - (max_inds(i) / M) * M, &
         ", ", (max_inds(i) / M) + 1, ") ", i = 1,size(max_inds))
      write (Out, '(a)') "Индексы минимумов:"
      write (Out, '('//size(min_inds)//'(a, i0, a, i0, a))') ("(", min_inds(i) - (min_inds(i) / M) * M, &
	     ", ", (min_inds(i) / M) + 1, ") ", i = 1,size(min_inds))
   close (Out)
end program exercise_7_21
