! Copyright 2015 Fyodorov S. A.

program reference_lab_5
   use Environment
   use Expr_IO
   use Expr_Process
   use Stack_Impl

   implicit none
   character(:), allocatable :: e1_file, e2_file, e3_file, output_file, postfix, prefix
   logical :: b
   
   e1_file  = "../data/E1.txt"
   e2_file  = "../data/E2.txt"
   e3_file  = "../data/E3.txt"
   output_file = "output.txt"
   
   postfix = read_expr(e1_file)
   b = check_expr(postfix)
   if (b) then
      prefix = postfix_to_prefix(postfix)
   end if
   call write_expr(output_file, postfix, .TRUE., "postfix: ", "rewind")
   call write_expr(output_file, prefix, b, "prefix: ", "append")
   
   postfix = read_expr(e2_file)
   b = check_expr(postfix)
   if (b) then
      prefix = postfix_to_prefix(postfix)
   end if
   call write_expr(output_file, postfix, .TRUE., "postfix: ", "append")
   call write_expr(output_file, prefix, b, "prefix: ", "append")
   
   postfix = read_expr(e3_file)
   b = check_expr(postfix)
   if (b) then
      prefix = postfix_to_prefix(postfix)
   end if
   call write_expr(output_file, postfix, .TRUE., "postfix: ", "append")
   call write_expr(output_file, prefix, b, "prefix: ", "append")
   
end program reference_lab_5