program exercise_5_11
   use Environment
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, N = 0
   real(R_)                :: MinEl, MaxEl
   real(R_), allocatable   :: Z(:)

   open (file=input_file, newunit=In)
      read (In, *) N
      allocate (Z(N))
      read (In, *) Z
   close (In)
   
   ! Размещение данных в НАЧАЛЕ работы программы,
   ! а не внутри подпрограммы при КАЖДОМ её вызове.
   call MinAndMax(Z, MinEl, MaxEl)

   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, "(i0)") N
      write (Out, "("//N//"f8.2)") Z
      write (Out, "('Минимум = ', f0.2)") MinEl
      write (Out, "('Максимум = ', f0.2)") MaxEl
   close (Out)

contains
   ! Чистая подпрограмма в регулярном стиле.
   pure subroutine MinAndMax(Z, MinEl, MaxEl)
      real(R_)    Z(:), MinEl, MaxEl
      intent(in)  Z
      intent(out) MinEl, MaxEl

      MinEl = MinVal(Z)
      MaxEl = MaxVal(Z)
   end subroutine MinAndMax
end program exercise_5_11
