! Implementation of priority queue.
! Ref: Advanced algorithms and data structures, 2021 (Chapter 2)
!
! See: test/implement.f90 for an example of a concrete data type

  module pqueue_mod
    implicit none
    private
    public element_t, pqueue_t

    integer, parameter :: ERR_EMPTY = 1, ERR_OK = 0

    ! Abstract container where data are stored:
    ! # "priority" method must be implemented
    ! # "updateindex" and "element2index" are used for queue
    !   "remove" and "update" methods only, so their implementation is
    !   optional.
    type, abstract :: element_t
    contains
      procedure(priority), deferred :: priority
      procedure(updateindex), deferred :: updateindex
      procedure(element2index), deferred :: element2index
    end type

    abstract interface
      ! get the priority of the element
      integer function priority(this) 
        import element_t
        class(element_t), intent(in) :: this
      end function

      ! advise the element about its index in the internal array
      subroutine updateindex(this, index)
        import element_t
        class(element_t), intent(inout) :: this
        integer, intent(in) :: index
      end subroutine

      ! get the index in the internal array
      integer function element2index(this) result(index)
        import element_t
        class(element_t), intent(in) :: this
      end function
    end interface



    ! Private wrapping type to allow polymorphic variable at the lhs
    ! of the implicit assignment (assignment implementation is optional)
    type :: element_ptr
      private
      class(element_t), allocatable :: p
    end type



    ! Priority queue implemented using a binary heap
    type :: pqueue_t
      private
      type(element_ptr), allocatable :: array(:)
      integer :: n = 0, nalloc = 0
    contains
      procedure :: top       
      procedure :: peek
      procedure :: insert
      procedure :: remove
      procedure :: update
      procedure :: size => pqueue_size
      procedure :: validate
    end type

    interface pqueue_t
      module procedure pqueue_init
    end interface

  contains

    function pqueue_init(elements) result(this)
      type(pqueue_t) :: this
      class(element_t), intent(in) :: elements(:)
!
! Make new priority queue from an array of elements
!
      integer :: i

      this % n = size(elements)
      this % nalloc = this % n
      allocate(this % array(this % nalloc))
      do i = 1, this % n
        this % array(i) % p = elements(i)
        call this % array(i) % p % updateindex(i)
      enddo

      ! heapify: check and correct heap property for all non-leaf nodes
      do i = this % n / 2, 1, -1
        call pushDown(this, i)
      enddo
    end function pqueue_init



    subroutine top(this, element, ierr)
      class(pqueue_t), intent(inout) :: this
      class(element_t), intent(out), allocatable :: element
      integer, optional, intent(out) :: ierr
 !
 ! Remove element with the highest priority from the queue
 !
      integer :: ierr0

      if (this % n < 1) then
        ierr0 = ERR_EMPTY
        if (present(ierr)) then
          ierr = ierr0
          return
        else
          error stop 'top on empty queue'
        endif
      endif
      if (present(ierr)) ierr = ERR_OK

      element = this % array(1) % p
      call this % array(1) % p % updateindex(-1)

      ! replace removed element by the tail element
      if (this % n /= 1) then
        this % array(1) = this % array(this % n)
        call this % array(1) % p % updateindex(1)
      endif
      this % n = this % n - 1

      ! repair heap
      if (this % n > 1) call pushDown(this, 1)

      ! free space for an empty queue
      if (this % n == 0) then
        deallocate(this % array)
        this % nalloc = 0
      endif
    end subroutine top



    pure function peek(this, index) result(element) 
      class(pqueue_t), intent(in) :: this
      integer, intent(in), optional :: index
      class(element_t), allocatable  :: element
 !
 ! return element with the highest priority but leave the queue unchanged
 ! optional "index" alows to iterate and peek through the whole queue
 !
      if (this % n < 1) error stop 'peek on empty queue'
      if (.not. present(index)) then
        element = this % array(1) % p
      else
        if (index > this % n .or. index < 1) &
            error stop 'peek - index out of bounds'
        element = this % array(index) % p
      endif
    end function peek



    subroutine insert(this, element)
      class(pqueue_t), intent(inout) :: this
      class(element_t), intent(in) :: element
 !
 ! insert element to the queue
 !
      type(element_ptr), allocatable :: newarray(:)
      integer :: i

      ! if queue is empty
      if (.not. allocated(this % array)) then
        this % nalloc = 1
        allocate(this % array(this % nalloc))
      endif

      ! add additional space if array is full
      if (this % n == this % nalloc) then
        this % nalloc = this % nalloc * 2
        allocate(newarray(this % nalloc))
        do i = 1, this % n
          newarray(i) = this % array(i)
        enddo
        call move_alloc(newarray, this % array)
      endif

      this % n = this % n + 1
      this % array(this % n) % p = element
      call this % array(this % n) % p % updateindex(this % n)
      call bubbleUp(this, this % n)
    end subroutine insert



    subroutine remove(this, element)
      class(pqueue_t), intent(inout) :: this
      class(element_t), intent(in) :: element
!
! remove element from the queue
!
      integer :: i

      i = element % element2index()
      if (i < 1) then
        print *, 'remove - element not found, doing nothing'
        return
      endif

      if (i > this % n) &
          error stop 'remove - element2index value out of bounds'

      call this % array(i) % p % updateindex(-1)

      if (i /= this % n) then
        this % array(i) = this % array(this % n)
        call this % array(i) % p % updateindex(i)
        this % n = this % n - 1
        call pushDown(this, i)
      else
        this % n = this % n - 1
      endif

      ! free space for an empty queue
      if (this % n == 0) then
        deallocate(this % array)
        this % nalloc = 0
      endif
    end subroutine remove



    subroutine update(this, element, newelement)
      class(pqueue_t), intent(inout) :: this
      class(element_t), intent(in) :: element, newelement
!
! update element in the queue (priority can change)
!
      integer :: old_priority, new_priority, i

      i = element % element2index()
      if (i < 1) then
        print *, 'update - element not found, doing nothing'
        return
      endif

      if (i > this % n) &
          error stop 'update - element2index value out of bounds'

      old_priority = this % array(i) % p % priority()
      call this % array(i) % p % updateindex(-1)
      this % array(i) % p = newelement
      new_priority = this % array(i) % p % priority()
      call this % array(i) % p % updateindex(i)

      if (new_priority > old_priority) then
        call bubbleUp(this, i)
      elseif (new_priority < old_priority) then
        call pushDown(this, i)
      endif
    end subroutine update



    pure function pqueue_size(this)
!
! get size of the queue
!
      integer :: pqueue_size
      class(pqueue_t), intent(in) :: this
      pqueue_size = this % n
    end function pqueue_size



!
! Helper functions for binary heap
!
    subroutine bubbleUp(this, index)
      class(pqueue_t), intent(inout) :: this
      integer, intent(in) :: index

      class(element_t), allocatable :: ecur
      integer :: icur, ipar

      icur = index
      ecur = this % array(icur) % p

      do
        if (icur == 1) exit
        ipar = icur / 2
        if (this%array(ipar)%p % priority() >= ecur % priority()) exit
        this % array(icur) = this % array(ipar)
        call this % array(icur)%p % updateindex(icur)
        icur = ipar
      enddo
      this % array(icur)%p = ecur
      call this % array(icur)%p % updateindex(icur)
    end subroutine bubbleUp



    subroutine pushDown(this, index)
      class(pqueue_t), intent(inout) :: this
      integer, intent(in) :: index

      class(element_t), allocatable :: ecur
      integer :: icur, ilch, irch, ich

      icur = index
      ecur = this % array(icur) % p

      do
        if (icur*2 > this % n) exit 

        ! select a child with higher priority
        if (icur*2 + 1 > this % n) then
          ich = icur*2
        else
          ilch = icur*2
          irch = icur*2 + 1
          if (this % array(ilch)%p % priority() > &
              this % array(irch)%p % priority()) then
            ich = ilch
          else
            ich = irch
          endif
        endif

        if (this % array(ich) % p % priority() <= ecur % priority()) exit
        this % array(icur) = this % array(ich)
        call this % array(icur) % p % updateindex(icur)
        icur = ich
      enddo

      this % array(icur) % p = ecur
      call this % array(icur) % p % updateindex(icur)
    end subroutine pushDown



    logical function validate(this) result(isvalid)
      class(pqueue_t), intent(in) :: this
!
! Assert that the priority of every parent node in the heap is not
! lower than the priority of any of its children nodes.
!
      integer :: ipa, ich

      isvalid = .true.
      if (.not. allocated(this % array)) then
        if (this % n /= 0) isvalid = .false.
        return
      endif

      do ipa=1, this % n / 2
        ich = 2*ipa
        if (this % array(ich) % p % priority() > &
            this % array(ipa) % p % priority()) then
          isvalid = .false.
          exit
        endif
        ich = 2*ipa+1
        if (ich > this % n) cycle
        if (this % array(ich) % p % priority() > &
            this % array(ipa) % p % priority()) then
          isvalid = .false.
          exit
        endif
      enddo
      if (.not. isvalid) print *, 'error - validation failed'
    end function validate

  end module pqueue_mod
