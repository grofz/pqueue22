  program check
  use pqueue_mod, only : pqueue_t
  use implement_mod, only : myelement_t
  implicit none

  type(pqueue_t) :: q
  type(myelement_t) :: e, e2, e3, e4, e5, erem
  type(myelement_t)  :: epeek

  e  = myelement_t('nord', 10)
  e2 = myelement_t('south', 12)
  e3 = myelement_t('west', 2)
  e4 = myelement_t('east', 20)
  e5 = myelement_t('EAST', 20)

  call q % insert(e)
  call q % insert(e2)
  call q % insert(e5)
  call q % insert(e3)
  call q % insert(e4)

  call print_array()

  print *, "Peek:"
  call q % peek(epeek)
      print '("Item =",a," priority=",i0)', epeek % item, epeek % ipriority 

  print *, "Top:"
  call q % top(erem)
      print '("Item =",a," priority=",i0)', erem % item, erem % ipriority 
  call q % top(erem)
      print '("Item =",a," priority=",i0)', erem % item, erem % ipriority 
  call q % top(erem)
      print '("Item =",a," priority=",i0)', erem % item, erem % ipriority 
  call q % top(erem)
      print '("Item =",a," priority=",i0)', erem % item, erem % ipriority 
  call print_array()

  contains
    subroutine print_array
      type(myelement_t) :: eout
      integer :: i
      if (q % n == 0) print *, 'Empty'
      do i = 1, q % n
        eout = q % array(i)
        print '("Item =",a," priority=",i0)', eout % item, eout % ipriority 
      enddo
      print *, '==='
    end subroutine print_array
  
  end program check
