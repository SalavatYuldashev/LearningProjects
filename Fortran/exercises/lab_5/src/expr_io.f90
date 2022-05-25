! Copyright 2015 Fyodorov S. A.

module Expr_IO
   use Environment
   use Stack_Impl

   integer, parameter :: EXPR_LEN = 50
 
contains

   ! Чтение выражения из файла
   function read_expr(Input_File) result(expr)
      character(*), intent(in)    :: Input_File
      character(EXPR_LEN)         :: expr
      character(:), allocatable   :: format
      integer                     :: In
    
      open (file=Input_File, encoding=E_, newunit=In)
         format = '(a)'
         read (in, format) expr
      close (In)
   end function read_expr
   
   ! Запись выражения в файл
   subroutine write_expr(Output_File, Expr, Flag, Expr_Name, Position)
      character(*), intent(in)         :: Output_File, Expr, Expr_Name, Position
      logical, intent(in)              :: flag
      character(:), allocatable        :: format
      integer                          :: Out
    
      open (file=Output_File, encoding=E_, newunit=Out, position=Position)
         format = '(a, 1x, a)'
         if (flag) then
            write (Out, format) Expr_Name, Expr
         else
            write (Out, format) Expr_Name, "error"
         end if
      close (Out)
   end subroutine write_expr

end module Expr_IO