! Copyright 2015 Fyodorov S. A.

module Expr_Process

   use Environment
   use Expr_IO
   use Stack_Impl

   implicit none

contains
   ! Проверка корректности постфиксной формы
   function check_expr(expr) result(check)
      character(*), intent(in)    :: expr
      logical                     :: check
      integer                     :: counter, i, c

      check = .TRUE.
      counter = 0
     
      do i = 1, len(expr)
         c = iachar(expr(i:i))
         if ((c >= 65) .AND. (c <= 90)) then
            counter = counter + 1
         else if ((c == 43) .OR. (c == 45) .OR. (c == 42) .OR. (c == 47)) then
            counter = counter - 2
            if (counter < 0) then
               check = .FALSE.
               return
            end if
         counter = counter + 1
         else if (c /= 32) then
            check = .FALSE.
         else
            check = (counter == 1)
            return
         end if
      end do
   end function check_expr
   
   ! Перевод постфиксной формы в префиксную
   function postfix_to_prefix(postfix_expr) result(prefix_expr)
      character(*), intent(in)    :: postfix_expr
      character(:), allocatable  :: prefix_expr, t1, t2
      integer :: c, i
      type(stack)   :: S
      type(node), pointer :: p1, p2
    
      do i = 1, len(postfix_expr)
         c = iachar(postfix_expr(i:i))
         if ((c >= 65) .AND. (c <= 90)) then
            call push(s, postfix_expr(i:i))
         else if ((c == 43) .OR. (c == 45) .OR. (c == 42) .OR. (c == 47)) then
            p2 => top(s)
            t2 = p2%value
            call pop(s)
            p1 => top(s)
            t1 = p1%value
            call pop(s)
            call push(s, postfix_expr(i:i)//t1//t2)
         end if
      end do
     
      p1 => top(s)
      prefix_expr = p1%value
   end function postfix_to_prefix
   
end module Expr_process