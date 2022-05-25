program exercise_2_12
   use Environment
   use IEEE_Arithmetic
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0
   real(R_)                :: x = 0, x1 = 0, x2 = 0, x3 = 0, y = 0, y1 = 0, y2 = 0, y3 = 0

   open (file=input_file, newunit=In)
      read (In, *) x1, y1, x2, y2, x3, y3, x
   close (In)
   
   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, "(7(a, f0.2/))") "x1 = ", x1, "y1 = ", y1, "x2 = ", x2, "y2 = ", y2, "x3 = ", x3, "y3 = ", y3, "x = ", x
   close (Out)
   
   y = F(x1, y1, x2, y2, x3, y3, x)

   open (file=output_file, encoding=E_, newunit=Out, position='append')
      write (Out, "('y = ', f0.2)") y
   close (Out)

contains
   ! Чистая функция.
   pure function F(x1, y1, x2, y2, x3, y3, x)
      real(R_) F, x1, y1, x2, y2, x3, y3, x
      intent(in) x1, y1, x2, y2, x3, y3, x

      block
         if (x>=x1 .and. x<=x2) then
            F = y1 + (x - x1) * (y2 - y1) / (x2 - x1)
         else if (x>=x2 .and. x<=x3) then
            F = y2 + (x - x2) * (y3 - y2) / (x3 - x2)
         else
            ! Присвоение не числа -- NaN.
            F = IEEE_Value(x, IEEE_Quiet_NaN)
         end if
      end block
   end function F
end program exercise_2_12
