program exercise_7_2b
   use Environment
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, N = 0
   real(R_), allocatable   :: Z(:)

   open (file=input_file, newunit=In)
      read (In, *) N
      allocate (Z(N))
      read (In, *) Z
   close (In)
   
   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, "(i0)") N
      write (Out, "("//N//"f8.2)") Z
      call Sort(Z, N)
      write (Out, "('Отсортированный массив по неубыванию:',/"//N//"f8.2)") Z
   close (Out)

contains
   ! Чистая подпрограмма в регулярном стиле.
   pure subroutine Sort(Z, N)
      real(R_)    Z(:), temp
      integer     N, i, j
      intent(in)  N
      intent(inout)  Z

      do i = 1, N - 1
        do j = i + 1, N
          if (Z(i) > Z(j)) then
            temp = Z(j)
            Z(j) = Z(i)
            Z(i) = temp
          end if
        end do
      end do
   end subroutine Sort
end program exercise_7_2b
