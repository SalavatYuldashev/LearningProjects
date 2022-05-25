! Copyright 2015 Fyodorov S. A.

module Matr_IO
   use Environment

   implicit none

   ! Структура данных для хранения данных об элементе разреженной матрицы.
   type sparse_matrix_item
      integer  :: value, row, col
      type(sparse_matrix_item), pointer :: next => Null()
   end type sparse_matrix_item

contains
   ! Чтение разреженной матрицы в список
   function Read_list(Input_File, M, N) result(Items_List)
      type(sparse_matrix_item), pointer     :: Items_List
      character(*), intent(in)   :: Input_File
      integer, intent(out)       :: M, N 
      integer  In, Row, Col
      integer, allocatable :: Arr(:)
      character(50) :: format
	  
      Row = 1
      Col = 1
      open (file=Input_File, encoding=E_, newunit=In)
         read (In, *) M, N
         format  = '(' // N - 1 //'(i1, 1x), i1))'
         allocate(Arr(N))
         Items_List => Read_item(In, Format, Arr, Row, Col, M, N)
      close (In)
   end function Read_list

   ! Чтение следующего элемента матрицы.
   recursive function Read_item(In, Format, Arr, Row, Col, M, N) result(Item)
      type(sparse_matrix_item), pointer   :: Item
      integer, intent(in)         :: In, M, N
      integer, intent(inout)      :: Row, Col
      integer  IO, i
      character(*), intent(in)    :: Format
      integer, intent(inout)      :: arr(N)
      logical                     :: isNotNull
	  
      if (Row > M) then
         Item => Null()
         return
      end if
	  
      if (Col == 1) then
         read (in, format, iostat=IO) (arr(i), i = 1, N)
         call Handle_IO_status(IO, "reading line from file")
      else
         IO = 0
      end if 
	  
      isNotNull = arr(Col) /= 0
	  
      if (isNotNull) then
         allocate(Item)
         Item%value = arr(Col)
         Item%row = Row
         Item%col = Col
      end if
	  
      if ((IO == 0) .and. (Row <= M)) then
          if (Col == N) then
             Col = 1
             Row = Row + 1
          else
             Col = Col + 1
          end if
	      
          if (isNotNull) then
             print *, "Read element. Row = ", Item%row, " Column = ", Item%col, " Value = ", Item%value 
             Item%next => Read_item(In, Format, Arr, Row, Col, M, N)
          else
             print *, "Read null element."
             Item => Read_item(In, Format, Arr, Row, Col, M, N)
          end if
      end if
   end function Read_item
   
   ! Распаковка обратно в матрицу
   subroutine Unpack_to_matrix(Output_File, Items_List, List_Name, Position, M, N)
      character(*), intent(in)   :: Output_File, Position, List_Name
      type(sparse_matrix_item), intent(in)  :: Items_List
      integer, intent(in) :: M, N
      integer, allocatable :: Arr(:)
      integer  :: Out, Row, Col
      
      open (file=Output_File, encoding=E_, position=Position, newunit=Out)
         write (out, '(/a)') List_Name
         Row = 1
         Col = 1
         allocate(Arr(N))
         call Unpack_item(Out, Items_List, Arr, Row, Col, M, N)
      close (Out)
   end subroutine Unpack_to_matrix
   
   ! Распаковка одного элемента
   recursive subroutine Unpack_item(Out, Item, Arr, Row, Col, M, N)
      type(sparse_matrix_item), intent(in)   :: Item      
      integer  :: IO
      integer, intent(in)         :: Out
      integer, intent(in)         :: M, N
      integer, intent(inout)      :: Row, Col, Arr(N)
      character(:), allocatable   :: format
      logical                     :: isNotNull
	  
      if (Row > M) then
         return
      end if
	  
      isNotNull = (Row == Item%row) .AND. (Col == Item%col)
	  
      if (isNotNull) then
         Arr(Col) = Item%value
      else
         Arr(Col) = 0
      end if
	  
      if (Col == N) then
         format = '(' // N - 1 //'(i1, 1x), i1))'
         write (Out, format, iostat=IO) Arr
         call Handle_IO_status(IO, "writing item")
         Col = 1
         Row = Row + 1
      else
         Col = Col + 1
      end if
	  
      if (isNotNull .AND. Associated(Item%next)) then
         call Unpack_item(Out, Item%next, Arr, Row, Col, M, N)
      else
         call Unpack_item(Out, Item, Arr, Row, Col, M, N)
      end if
   end subroutine Unpack_item 
   
   ! Вывод списка из разреженной матрицы
   subroutine Output_items_list(Output_File, Items_List, List_Name, Position)
      character(*), intent(in)   :: Output_File, Position, List_Name
      type(sparse_matrix_item), intent(in)  :: Items_List
      integer  :: Out
      
      open (file=Output_File, encoding=E_, position=Position, newunit=Out)
         write (out, '(/a)') List_Name
         call Output_item(Out, Items_List)
      close (Out)
   end subroutine Output_items_list

   ! Вывод одного элемента списка
   recursive subroutine Output_item(Out, Item)
      type(sparse_matrix_item), intent(in)   :: Item      
      integer  :: IO
      integer, intent(in)         :: Out
      character(:), allocatable  :: format
	  
      format = '(3(a, i0))'
      write (Out, format, iostat=IO) "(", Item%row, ",", Item%col, "):",  Item%value
      call Handle_IO_status(IO, "writing item")
      if (Associated(Item%next)) &
         call Output_item(Out, Item%next)
   end subroutine Output_item 
   
end module Matr_IO 