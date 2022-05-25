program exercise_8_11
   use Environment
   use Matrix_IO
   use MatrMax

   implicit none
   
   integer                 :: Out = 0
   real(R_)                :: b_max, c_max, d_max
   character(*), parameter :: b_file = "../data/B.txt", c_file = "../data/C.txt", &
      d_file = "../data/D.txt", output_file = "output.txt"

   real(R_), allocatable   :: B(:, :), C(:, :), D(:, :)

   B = ReadMatrix(b_file)
   C = ReadMatrix(c_file)
   D = ReadMatrix(d_file)
   
   call matr_max(B, b_max)
   call matr_max(C, c_max)
   call matr_max(D, d_max)
   
   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, "(3(a, f0.2/))") "Максимум B = ", b_max, "Максимум C = ", c_max, "Максимум D = ", d_max
   close (Out)
   
   call OutputMatrix(output_file, b_file, B)
   call OutputMatrix(output_file, c_file, C)
   call OutputMatrix(output_file, d_file, D)
   
   open (file=output_file, encoding=E_, newunit=Out, position="append")
      write (Out, "(3(a, f0.2/))") "Произведение 1: ", b_max * c_max, &
	     "Произведение 2: ", b_max * d_max, "Произведение 3: ", c_max * d_max
   close (Out)
   
end program exercise_8_11