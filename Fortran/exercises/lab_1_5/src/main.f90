! Copyright 2015 Fyodorov S. A.

program reference_lab_1_5
   use Environment
   use Group_Process
   use Group_IO

   implicit none
   character(:), allocatable :: input_file, output_file
   character(kind=CH_), parameter   :: MALE = Char(1052, CH_), PETERSBURGER = Char(1055, CH_)
   ! MALE = 4_"М"
   ! MALE = "М"
   type(person), pointer   :: Group_List => Null(), Peters_List => Null(), Res_List => Null()
   integer(I_)             :: Peters_Amount = 0

   input_file  = "../data/class.txt"
   output_file = "output.txt"
   
   Group_List => Read_class_list(input_file)

   if (Associated(Group_List)) then
      call Output_class_list(output_file, Group_List, "Исходный список:", "rewind")

      call Get_list_by_gender_and_city(Group_List, Peters_List, Peters_Amount, MALE, PETERSBURGER)
	  
      allocate(Res_List)
      allocate(Res_List%next)
      allocate(Res_List%next%next)
	  
      call Sort_result_list(Peters_List, Res_List, Peters_Amount)
	  
      if (Associated(Res_List)) &
         call Output_result_list(output_file, Res_List, &
            "Трое наиболее молодых петербуржца:", "append")
   end if

end program reference_lab_1_5
