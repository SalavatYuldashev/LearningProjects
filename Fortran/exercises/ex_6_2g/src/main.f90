program exercise_6_2g
   use Environment
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0
   real(R_)                :: ln_x = 0, x = 0, relerr = 0

   open (file=input_file, newunit=In)
      read (In, *) x, relerr
   close (In)
   
   ln_x = LnXImp(x, relerr)

   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, '(4(a, T16, "= ", e13.6/))') 'x', x, "Ln(x)", ln_x, "Fortran Ln(x)", Log(x), "Error", ln_x - Log(x)
   close (Out)

contains
   ! Чистая функция в императивном стиле.
   real(R_) pure function LnXImp(x, relerr) result(ln_x)
      real(R_)   x, relerr
      intent(in) x, relerr
      real(R_)   x_1, x_1p, x_p, old_ln_x
      integer    n
	  
      n    = 1
      x_1  = x - 1
      x_1p = x_1
      x_p  = x
	  
      do
        old_ln_x = ln_x
        ln_x     = ln_x + x_1p / (n * x_p)
        x_1p     = x_1p * x_1
        x_p      = x_p * x
        n        = n + 1
        if (Abs(ln_x - old_ln_x) < relerr) exit
      end do
      !print "('Число членов суммы: ', i0)", n / 2 + 1
      !print "('Число итераций: ', i0)", n / 2
   end function LnXImp
end program exercise_6_2g
