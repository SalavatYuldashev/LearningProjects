program exercise_3_8
   use Environment
   
   implicit none
   character(*), parameter       :: input_file = "../data/input.txt", output_file = "output.txt"
   integer, parameter            :: N = 50
   integer                       :: In = 0, Out = 0, i = 0, j = 0
   real(R_)                      :: T = 0
   real(R_), dimension(1:N, 1:N) :: A

   open (file=input_file, newunit=In)
      do i = 1, N
         read(In, *) (A(i,j), j=1,N)
      end do 
   close (In)

   T = TraceImp(A)

   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, "(i0)") N
      write (Out, "("//N//"f8.2)") A
      write (Out, *)
      write (Out, "('След матрицы = ', f0.2)") T
   close (Out)

contains
   
   ! Чистая функция в императивном стиле.
   pure function TraceImp(A) result(Trace)
      real(R_)    Trace
      real(R_), dimension(1:N, 1:N) :: A
      intent(in)  A
      integer     i

      Trace = 0
      do concurrent (i = 1:N)
         Trace = Trace + A(i,i)
      end do
   end function TraceImp
end program exercise_3_8
