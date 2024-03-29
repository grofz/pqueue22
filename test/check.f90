  program check
    integer :: i
    real :: s0, s1
    logical :: ival
    interface
      subroutine test1()
      end subroutine

      subroutine test2(n, ival)
        implicit none
        integer, intent(in) :: n
        logical :: ival
      end subroutine
    end interface

    print '(a)','==== TEST 1 ===='
    call test1()

    write(*,'(a)',advance='no') 'Enter size ? '
    read(*,*) i
    ival = .true.
    if (i>9999) ival = .false.
    print '(a)','==== TEST 2 ===='
    call cpu_time(s0)
    call test2(i,ival)
    call cpu_time(s1)
    print *, 'Time = ',s1-s0, (s1-s0)/i
  end program check



  subroutine test1
    use pqueue_mod, only : pqueue_t, element_t
    use implement_mod, only : myelement_t
    use iso_fortran_env, only : fid => output_unit
    implicit none

    type(pqueue_t) :: q
    type(myelement_t), target :: e(10) 
    class(element_t), allocatable  :: erem
    integer :: i

    ! Sample data - insert
    e(1) = myelement_t('1nord', 10)
    e(2) = myelement_t('2south', 12)
    e(3) = myelement_t('3west', 2)
    e(4) = myelement_t('4east', 20)
    e(5) = myelement_t('5EAST', 20)
    e(6) = myelement_t('6bottom', 50)
    e(7) = myelement_t('6top', 2)
    e(8) = myelement_t('8july', 21)
    e(9) = myelement_t('9march', 15)
    e(10) = myelement_t('10summer', 5)
    do i=1, 10
      e(i) % original => e(i)
    enddo

    ! constructor (first half)...
    q = pqueue_t(e(1:5))
    print *, 'valid = ', q % validate()
    print *, 'indices ', e(1:5) % index
    call print_array()
    print *

    ! ...and insert (second half)
    do i=6,10
      call q % insert(e(i))
    enddo
    print *, 'valid = ', q % validate()
    print *, 'indices ', e(1:size(e)) % index
    call print_array()
    print *

    ! test update 
    print *, 'Update ', e(3) % item
    e(3) % ipriority = 40
    call q % update(e(3), e(3))
    print *, 'valid = ', q % validate()
    print *, 'indices ', e(1:size(e)) % index
    call print_array()

    print *, 'Update ', e(3) % item
    e(3) % ipriority = 4
    call q % update(e(3), e(3))
    print *, 'valid = ', q % validate()
    print *, 'indices ', e(1:size(e)) % index
    call print_array()

    ! test remove
    print *, 'Remove ', e(1) % item
    call q % remove(e(1))
    print *, 'valid = ', q % validate()
    print *, 'indices ', e(1:size(e)) % index
    call print_array()

    ! test peek
    print *, "Peek:"
    !epeek2 = q % peek() ! not working, must define assignment
    !epeek = q % peek()
    associate(epeek2=>q%peek())
      select type(epeek2)
      class is (myelement_t)
        print '("Item =",a," priority=",i0)', epeek2 % item, epeek2 % ipriority 
      end select
    end associate
    print *, 'valid = ', q % validate()

    print *, "Top:"
    do i=1,6
      call q % top(erem)
      select type(erem)
      class is (myelement_t)
        print '("Item =",a," priority=",i0)', erem % item, erem % ipriority 
      end select
      print *, 'valid = ', q % validate()
      print *, 'indices ', e(1:size(e)) % index
      call print_array()
    enddo

  contains
    subroutine print_array
      integer :: ii
      if (q % size() == 0) write(fid,*) 'Empty'
      do ii = 1, q % size()
        associate (eout => q % peek(ii))
          select type(eout)
          class is (myelement_t)
            call eout % display(fid)
        !print '("Item =", ," priority=",i0)', eout % item, eout % ipriority 
          end select
        end associate
      enddo
      write(fid,*)
    end subroutine print_array
  end subroutine test1


  subroutine test2(n, ival)
    use pqueue_mod, only : pqueue_t, element_t
    use implement_mod, only : myelement_t
    use iso_fortran_env, only : fid => output_unit
    implicit none
    integer, intent(in) :: n
    logical :: ival
    integer :: i, last_priority
    type(myelement_t), target :: e(n)
    class(element_t), allocatable :: eout
    type(pqueue_t) :: q
    real :: rnd(n)

    call random_number(rnd)
    e % ipriority = int(rnd*n)

    do i=1,n
      e(i) % original => e(i)
      !call q % insert(e(i))
    enddo
    q = pqueue_t(e)
    print *, 'size =', q % size(), ' valid =', q % validate()

    last_priority = huge(last_priority)
    do i=1,n/2
      call q % top(eout)
      if (ival) then
        if (.not. q % validate()) error stop 'validation error'
      endif
      select type(eout)
      type is (myelement_t)
        if (eout % ipriority > last_priority) error stop 'wrong priority'
        last_priority = eout % ipriority
      end select
    enddo
    print *, 'size =', q % size(), ' valid =', q % validate()

    do i=1,n
      if (e(i) % index == -1) cycle
      e(i) % ipriority = - e(i) % ipriority
      call q % update(e(i),e(i))
      if (ival) then
        if (.not. q % validate()) error stop 'validation error'
      endif
    enddo
    print *, 'size =', q % size(), ' valid =', q % validate()

    do i=1,n/2-mod(n,2)
      call q % top(eout)
      if (ival) then
        if (.not. q % validate()) error stop 'validation error'
      endif
      select type(eout)
      type is (myelement_t)
        if (eout % ipriority > last_priority) error stop 'wrong priority'
        last_priority = eout % ipriority
      end select
    enddo
    print *, 'size =', q % size(), ' valid =', q % validate()

  end subroutine test2
  
