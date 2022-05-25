program lab_2
   use Environment
   use Source_Process
   use Source_IO

   implicit none
   character(:), allocatable :: F1, F2, output_file
   integer                   :: Out = 0

   type(SourceLine), pointer :: A  => Null()
   type(SourceLine), pointer :: B  => Null()

   F1 = "../data/A.txt"
   F2 = "../data/B.txt"
   output_file = "output.txt"
   
   A => Read_Source(F1)
   B => Read_Source(F2)

   call Output_source(output_file, A, "A", "rewind")
   call Output_source(output_file, B, "B", "append")

   if (Associated(A) .and. Associated(B)) then
      open (file=output_file, encoding=E_, newunit=Out, position="append")
         write (Out, *) "Позиция подстроки: ", Substring_Position(A, B)
      close (Out)
   end if

end program lab_2
