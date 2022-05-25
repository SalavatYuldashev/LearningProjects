module Source_Process
   ! Модуль с ЧИСТЫМИ процедурами обработки данных.
   use Environment
   use Source_IO

   implicit none

contains

   ! Индекс вхождения подстроки B в строку A
   pure function Substring_Position(A, B) result(pos)
      integer                       :: pos
      type(SourceLine), intent(in)  :: A
      type(SourceLine), intent(in)  :: B
      integer                       :: i, j

      do i = 1, len(A%string) - len(B%string)
         pos = i
         do j = 1, len(B%string)
            if(A%string(i+j-1:i+j-1) /= B%string(j:j)) then
               pos = 0
            end if
         end do
         if (pos > 0) then
            exit
         end if
      end do  
   end function Substring_Position

end module Source_process
