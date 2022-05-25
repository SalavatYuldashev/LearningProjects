! Copyright 2015 Fyodorov S. A.

program reference_lab_4
   use Environment
   use Matr_IO

   implicit none
   character(:), allocatable :: f1_file, f2_file, output_file
   integer  :: M, N

   type(sparse_matrix_item), pointer   :: S1 => Null()

   f1_file  = "../data/F1.txt"
   f2_file  = "F2.txt"
   output_file = "output.txt"
   
   S1 => Read_list(f1_file, M, N)
   
   if (Associated(S1)) then
      call Output_items_list(output_file, S1, "S1:", "rewind")
      call Unpack_to_matrix(output_file, S1, "S2:", "append", M, N)
      call Unpack_to_matrix(f2_file, S1, "S2:", "append", M, N)
   end if 
  
end program reference_lab_4
