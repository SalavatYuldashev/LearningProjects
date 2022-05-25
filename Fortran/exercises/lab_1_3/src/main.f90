! Copyright 2015 Fyodorov S. A.
  
program reference_lab_1_3
   use Environment
   use Group_Process
   use Group_IO

   implicit none
   character(:), allocatable :: input_file, output_file, data_file
   character(kind=CH_), parameter :: MALE = Char(1052, CH_), PETERSBURGER = Char(1055, CH_)
   
   type(person)              :: Group(PEOPLE_AMOUNT), Res(MEN_AMOUNT)
   type(person), allocatable :: Peters(:)

   input_file  = "../data/class.txt"
   output_file = "output.txt"
   data_file   = "class.dat"
   
   call Create_data_file(input_file, data_file)
   
   Group = Read_class_list(data_file)

   call Output_class_list(output_file, Group, "Исходный список:", "rewind")

   Peters = Pack(Group, Group%Gender == MALE .AND. Group%Registration == PETERSBURGER)
   
   call Sort_result_list(Peters, Res)
	  
   call Output_result_list(output_file, Res, "Трое наиболее молодых петербуржца:", "append")

end program reference_lab_1_3
