! Copyright 2015 Fyodorov S. A.

program reference_lab_3
   use Environment
   use Group_Process
   use Group_IO

   implicit none
   character(:), allocatable :: f1_file, f2_file, output_file
   character(kind=CH_), parameter   :: MALE = Char(1052, CH_), PETERSBURGER = Char(1055, CH_)
   ! MALE = 4_"М"
   ! MALE = "М"
   type(student), pointer   :: S1 => Null(), S2 => Null()

   f1_file  = "../data/F1.txt"
   f2_file  = "../data/F2.txt"
   output_file = "output.txt"
   
   S1 => Read_list(f1_file, '(i1,a,a1)')
   S2 => Read_list(f2_file, '(a)')

   if (Associated(S1) .AND. Associated(S2)) then
      call Output_class_list(output_file, S1, "S1:", "rewind", 0)
      call Output_class_list(output_file, S2, "S2:", "append", 0)
	  
      call Insert(S1, S2)
      call Remove_List(S2)
      call Output_class_list(output_file, S1, "S1 после вставки:", "append", 1)
      if (Associated(S2)) then
         call Output_class_list(output_file, S2, "S2:", "append", 0)
      end if
   end if
end program reference_lab_3
