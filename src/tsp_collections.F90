module tsp_collections

  use tsp_data_structures
  use tsp_utilities
  use tsp_time_series_manager
  use tsp_table_manager
  use tsp_datetime_class
  use tsp_equations_interpreter

  implicit none

  type T_TS_COMPARISON
    character (len=MAXNAMELENGTH) :: sObservedSeries = ""
    character (len=MAXNAMELENGTH) :: sModeledSeries = ""
    character (len=MAXNAMELENGTH) :: sObservationGroup = ""
    character (len=1024) :: sWeightsEquation = ""
    real (kind=T_SGL), dimension(:), allocatable :: rWeightValue
  end type T_TS_COMPARISON

  type T_TABLE_COMPARISON
    character (len=MAXNAMELENGTH) :: sObservedTable = ""
    character (len=MAXNAMELENGTH) :: sModeledTable = ""
    character (len=MAXNAMELENGTH) :: sObservationGroup = ""
    character (len=1024) :: sWeightsEquation = ""
    real (kind=T_SGL), dimension(:), allocatable :: rWeightValue
  end type T_TABLE_COMPARISON

  integer (kind=T_INT), parameter, private :: BLACK = 0
  integer (kind=T_INT), parameter, private :: RED = 1

  integer (kind=T_INT), parameter, private :: LEFT = 0
  integer (kind=T_INT), parameter, private :: RIGHT =1

  type :: T_NODE
    type (T_NODE), pointer :: pRight => null()
    type (T_NODE), pointer :: pLeft => null()
    type (T_NODE), pointer :: pParent => null()
    integer (kind=T_INT) :: iColor = RED

    character (len=256) :: sNodeName = ""
    type (T_TIME_SERIES), pointer :: pTS => null()
    type (T_TABLE), pointer :: pTable => null()
  end type T_NODE

  type TIME_SERIES_COLLECTION
    integer (kind=T_INT) :: iNumTimeSeries  = 0
    integer (kind=T_INT) :: iNumTables      = 0
    integer (kind=T_INT) :: iNumTSComparisons = 0
    integer (kind=T_INT) :: iNumTableComparisons = 0

    type (T_NODE), pointer :: pRoot => null()
    type (T_NODE), pointer :: pCurrent => null()
    type (T_TIME_SERIES), dimension(:), pointer :: pTS => null()
    type (T_TIME_SERIES), pointer :: pTS_Head => null()
    type (T_TS_COMPARISON), dimension(:), allocatable :: tTSComparison
    type (T_TABLE), dimension(:), allocatable :: tTable
    type (T_TABLE), pointer :: pTable_Head => null()
    type (T_TABLE_COMPARISON), dimension(:), allocatable :: tTableComparison

  contains

!    procedure :: add_ts_sub, add_table_sub
!    procedure :: addSeries => add_node_sub
!    procedure :: addTable => add_node_sub
    procedure :: tsCompare => add_ts_comparison_sub
    procedure :: tableCompare => add_table_comparison_sub
!    generic :: add => add_ts_link_sub, add_table_link_sub
!    procedure :: clear => remove_all_sub
    procedure :: calculate => calc_values_from_equation_fn
    procedure :: clear => clear_node_contents_sub
    procedure :: newTimeBase => conform_ts_sub
!    procedure :: removeNode => delete_node_from_tree_sub
    procedure :: removeNode => delete_top_down_sub
    procedure :: removeTS => remove_ts_link_sub
    procedure :: removeTable => remove_table_link_sub
    procedure :: insert => add_node_sub
!    procedure :: rotateLeft => rotate_node_left_sub
!    procedure :: rotateRight => rotate_node_right_sub
    procedure :: rotateLeft => rotate_node_left_sub
    procedure :: rotateRight => rotate_node_right_sub
    procedure :: rebalanceTree => rebalance_tree_after_insert_sub
    procedure :: restructureTree => restructure_tree_after_deletion_sub
    procedure :: traverse => traverse_entire_tree_sub
    procedure, public  :: summarize => summarize_sub
    procedure, private :: summarizeSeries => traverse_tree_summarize_series_sub
    procedure, private :: summarizeTable => traverse_tree_summarize_table_sub
    procedure :: describe => describe_ts_sub
    procedure :: makeDot => make_dot
    procedure :: getNode => find_node_in_tree_fn
    procedure :: getSeries => get_ts_pointer_node_fn
    procedure :: getSeriesComparison => get_ts_comparison_pointer_fn
    procedure :: getTableComparison => get_table_comparison_pointer_fn
    procedure :: getTable => get_table_pointer_node_fn
    procedure :: listTS => list_output_ts_sub
    procedure :: pestWriteTSComparison => pest_write_ts_comparison_sub
    procedure :: pestWriteTableComparison => pest_write_table_comparison_sub
    procedure :: listTable => list_output_table_sub
    procedure :: datesEqual => are_datetime_stamps_identical_fn
    procedure :: datesEqualByName => are_datetime_stamps_identical_names_fn

  end type TIME_SERIES_COLLECTION

contains

!------------------------------------------------------------------------------

subroutine delete_top_down_sub(this, sNodename)

   class(TIME_SERIES_COLLECTION) :: this
   character(len=*) :: sNodename

   ! [ LOCALS ]
   type (T_NODE), pointer        :: pHead => null()
   type (T_NODE), pointer        :: pG, pG_Link, pG_NotLink  ! pointers to grandparent
   type (T_NODE), pointer        :: pP, pP_Link, pP_NotLink  ! pointers to parent
   type (T_NODE), pointer        :: pQ, pQ_Link, pQ_NotLink  ! pointers to iterator
   type (T_NODE), pointer        :: pS, pS_Link, pS_NotLink  ! general node pointer
   type (T_NODE), pointer        :: pF          ! pointer to found item
   type (T_NODE), pointer        :: pTemp       ! general temp pointer
   integer (kind=T_INT) :: iDir, iLast, iDir2, iDir3, iDir4
   logical (kind=T_LOGICAL) :: lFound

   ! initialize direction index
   iDir = RIGHT; iDir2 = RIGHT
   lFound = lFALSE

   if( .not. associated(this%pRoot) ) return

   ! initialize dummy node
   if(.not. associated(pHead) ) allocate(pHead)

   ! set up helpers
   pHead%sNodename = "FALSE_NODE"
   pHead%pRight => this%pRoot
   pQ => pHead
   pG => null(); pG_Link => null(); pG_NotLink => null()
   pP => null(); pP_Link => null(); pP_NotLink => null()
   pS => null(); pS_Link => null(); pS_NotLink => null()
   pQ_Link => null(); pQ_NotLink => null()
   pF => null()
   pTemp => null()

   do

     ! find the child of the current node, in the direction that takes
     ! us closer to the target
     pQ_Link => follow_link(pQ, iDir)

     ! if child is null, we've reached a leaf; end loop
     if(.not. associated(pQ_Link) ) exit

     ! update helpers
     iLast = iDir
     pG => pP      ! grandparent points to parent
     pP => pQ      ! parent points to current node

     ! update pQ by following left or right pointer link
     pQ => pQ_Link  ! current node moved down the tree
     iDir = get_direction( LLT(pQ%sNodename, sNodename) )

     ! once we know direction we should move in, update helper pointers
     pQ_Link => follow_link(pQ, iDir)
     pQ_NotLink => follow_link(pQ, flip_direction(iDir))

     ! save found node
     if( associated(pQ) ) then
       if( str_compare(pQ%sNodename, sNodename) ) then
         pF => pQ
       endif
     endif

     ! push red node down
     if( get_node_color(pQ) == BLACK .and. get_node_color(pQ_Link) == BLACK ) then
       if( get_node_color(pQ_NotLink) == RED ) then            ! other child is RED
         select case (iLast)
           case(LEFT)
!   call this%makeDot(pQ, pG, pF, pP, pS, this%pRoot, __LINE__)
             pP%pLeft => node_single_rotation_fn(pQ, iDir)
             pP => pP%pLeft
!   call this%makeDot(pQ, pG, pF, pP, pS, this%pRoot, __LINE__)
           case(RIGHT)
!   call this%makeDot(pQ, pG, pF, pP, pS, this%pRoot, __LINE__)
             pP%pRight => node_single_rotation_fn(pQ, iDir)
             pP => pP%pRight
!   call this%makeDot(pQ, pG, pF, pP, pS, this%pRoot, __LINE__)
         end select

       else if ( get_node_color(pQ_NotLink) == BLACK ) then     ! other child is BLACK

         ! find the SIBLING to the CURRENT NODE
         pS => follow_link(pP, flip_direction(iLast) )

         if( associated(pS) ) then                              ! sibling is NOT NULL

           ! find the children of the sibling
           pS_Link => follow_link(pS, iLast)
           pS_NotLink => follow_link(pS, flip_direction(iLast))

           ! color flip if both of sibling's children are BLACK
           if( get_node_color(pS%pLeft) == BLACK &
              .and. get_node_color(pS%pRight) == BLACK ) then
             call set_node_color(pP, BLACK)       ! set parent to BLACK
             call set_node_color(pS, RED)         ! set sibling to RED
             call set_node_color(pQ, RED)         ! set current to RED

           elseif(associated(pG) ) then
!        call dump(pQ, pG, pF, pP, pS, this%pRoot, iDir, iLast, __LINE__)

             iDir2 = get_direction(associated(pG%pRight, pP) )

             if( get_node_color(pS_Link) == RED ) then
                pTemp => node_double_rotation_fn(pP, iLast)
                call set_pointer(pG, pTemp, iDir2)
                nullify(pTemp)

             elseif( get_node_color(pS_NotLink) == RED ) then
                pTemp => node_single_rotation_fn(pP, iLast)
                call set_pointer(pG, pTemp, iDir2)
                nullify(pTemp)

             endif

             ! ensure correct node coloring
             call set_node_color(pQ, RED)

             ! get helper pointer to the child
             pG_Link => follow_link(pG, iDir2)
             call set_node_color(pG_Link, RED)
             if(associated(pG_Link) ) then
               call set_node_color(pG_Link%pLeft, BLACK)
               call set_node_color(pG_Link%pRight, BLACK)
             endif

           endif
         endif       ! sibling node is NOT NULL
       endif      ! other child is RED/BLACK
     endif     ! push red node down

   enddo

   ! Replace and remove, if found
   if( associated(pF) ) then

     ! vacate any existing data series or tables in FOUND node
     call this%clear(pF)

     ! swap node names
     pF%sNodename = pQ%sNodename

     ! point at the data formerly associated with the "leaf"
     if(associated(pQ%pTS) ) pF%pTS => pQ%pTS
     nullify(pQ%pTS)
     if(associated(pQ%pTable) ) pF%pTable => pQ%pTable
     nullify(pQ%pTable)

     iDir3 = get_direction(associated(pP%pRight, pQ) )
     iDir4 = get_direction(.not. associated(pQ%pLeft) )

     ! rewire the connections following removal of a "leaf"
     select case(iDir3)
       case(LEFT)
         pP%pLeft => follow_link(pQ, iDir4)
       case(RIGHT)

       pP%pRight => follow_link(pQ, iDir4)
       case default
         call assert(lFALSE, "Logic error in case select", trim(__FILE__),__LINE__)
     end select

     if(associated(pQ) ) then
       nullify(pQ%pRight)
       nullify(pQ%pLeft)
       deallocate(pQ)
     endif
     nullify(pQ)

     lFound = lTRUE

   endif

   ! update root and make it black
   this%pRoot => pHead%pRight
   call set_node_color(this%pRoot, BLACK)

   call warn( lFound, "Failed to find entity "//quote(sNodename)//". Nothing deleted." )

   if (associated(pHead) ) deallocate(pHead)

!   call this%makeDot(pQ, pG, pF, pP, pS, this%pRoot, __LINE__)

end subroutine delete_top_down_sub

!------------------------------------------------------------------------------

subroutine set_pointer(pNode, pNewPointer, iDirection)

  type (T_NODE), pointer :: pNode
  type (T_NODE), pointer :: pNewPointer
  integer (kind=T_INT) :: iDirection

  select case (iDirection)

    case (LEFT)

      if(associated(pNode) ) pNode%pLeft => pNewPointer

    case (RIGHT)

      if(associated(pNode) ) pNode%pRight => pNewPointer

    case default

      call assert(lFALSE, "Faulty logic in routine", trim(__FILE__), __LINE__)

  end select

  nullify (pNewPointer)

end subroutine set_pointer

!------------------------------------------------------------------------------

function follow_link(pNode, iDirection)   result(pLink)

  type (T_NODE), pointer :: pNode
  integer (kind=T_INT) :: iDirection
  type (T_NODE), pointer :: pLink

  if (associated(pNode) ) then

    select case(iDirection)

      case(LEFT)
        pLink => pNode%pLeft
      case(RIGHT)
        pLink => pNode%pRight
      case default
        call assert(lFALSE, "Logic error following link", trim(__FILE__),__LINE__)
    end select

  else

    pLink => null()

  endif

end function follow_link

!------------------------------------------------------------------------------

subroutine insert_top_down_sub(this, pNewNode)

   class(TIME_SERIES_COLLECTION) :: this
   type (T_NODE), pointer        :: pNewNode

   ! [ LOCALS ]
   type (T_NODE), pointer        :: pFalseRoot
   type (T_NODE), pointer        :: pG, pT      ! pointers to grandparent and parent
   type (T_NODE), pointer        :: pP, pQ      ! pointers to parent and iterator
   type (T_NODE), pointer        :: pTemp
   integer (kind=T_INT) :: iDir, iDir2, iLast

   ! initialize direction index
   iDir = LEFT
   iLast = LEFT

   nullify(pG, pT, pP, pQ, pFalseRoot, pTemp)

   ! if we're given a null pointer as a new node, return without doing anything!
   if(.not. associated(pNewNode) ) then
!     call dump(pQ, pG, pT, pP, pTemp, this%pRoot, iDir, iLast, __LINE__)
     return
   endif

   if( .not. associated(this%pRoot ) ) then
     ! empty tree case
     this%pRoot => pNewNode
     if(.not. associated(pFalseRoot) ) allocate(pFalseRoot)
     pFalseRoot%pRight => this%pRoot

!     call dump(pQ, pG, pT, pP, pTemp, this%pRoot, iDir, iLast, __LINE__)

   else

     ! set up helper pointers
     if(.not. associated(pFalseRoot) ) allocate(pFalseRoot)
     pFalseRoot%pRight => this%pRoot
     pT => pFalseRoot
     pT%pRight => this%pRoot
     pQ => this%pRoot
     nullify(pG)
     nullify(pP)

!     call dump(pQ, pG, pT, pP, pTemp, this%pRoot, iDir, iLast, __LINE__)

     do

       if(.not. associated(pQ) ) then

         ! insert new node at bottom
         pQ => pNewNode

         ! if the newly inserted node is null, get out!
         if(.not. associated(pQ) ) exit

         call set_child(pP, iDir, pNewNode)
!         call make_dot(this, pQ=pQ, pG=pG, pF=pT, pP=pP, pRoot=this%pRoot, iLine_=__LINE__)

       elseif(get_node_color(pQ%pLeft) == RED &
              .and. get_node_color(pQ%pRight) == RED ) then

!         call make_dot(this, pQ=pQ, pG=pG, pF=pT, pP=pP, pRoot=this%pRoot, iLine_=__LINE__)

         ! color flip
         call set_node_color(pQ, RED)
         call set_node_color(pQ%pLeft, BLACK)
         call set_node_color(pQ%pRight, BLACK)

!         call make_dot(this, pQ=pQ, pG=pG, pF=pT, pP=pP, pRoot=this%pRoot, iLine_=__LINE__)

       endif

       ! Fix red violation: child and parent cannot both be red
       if( get_node_color(pQ) == RED .and. get_node_color(pP) == RED ) then
         pTemp => get_right_pointer(pT)
         iDir2 = get_direction(associated(pTemp, pG) )

         pTemp => get_child(pP, iLast)
         if(associated(pQ, pTemp ) ) then

           ! set great-grandparent child to grandparent node after rotation
           call set_child(pT, iDir2, node_single_rotation_fn( pG, flip_direction(iLast) ) )
!           call make_dot(this, pQ=pQ, pG=pG, pF=pT, pP=pP, pRoot=this%pRoot, iLine_=__LINE__)

         else

             call set_child(pT, iDir2, node_double_rotation_fn( pG, flip_direction(iLast) ) )
!             call make_dot(this, pQ=pQ, pG=pG, pF=pT, pP=pP, pRoot=this%pRoot, iLine_=__LINE__)

         endif

       endif

!       ! if after the rotations we have a null pointer at the current node, cycle and
       ! allow pNewNode to be placed appropriately
!       if (.not. associated(pQ) ) cycle

       if(associated(pQ)) then
         if(str_compare(pQ%sNodename, pNewNode%sNodename) ) then
!           call make_dot(this, pQ=pQ, pG=pG, pF=pT, pP=pP, pRoot=this%pRoot, iLine_=__LINE__)
           exit
         endif
       endif

       iLast = iDir
       if(associated(pQ)) iDir = get_direction( LLT(pQ%sNodename, pNewNode%sNodename) )

       if (associated(pG) )  pT => pG
       pG => pP
       pP => pQ

       pQ => get_child(pQ, iDir)
!       call make_dot(this, pQ=pQ, pG=pG, pF=pT, pP=pP, pRoot=this%pRoot, iLine_=__LINE__)

     enddo

   endif

   ! update root
   this%pRoot => pFalseRoot%pRight

   ! ensure root node is black
   call set_node_color(this%pRoot, BLACK)
!   call make_dot(this, pQ=pQ, pG=pG, pF=pT, pP=pP, pRoot=this%pRoot, iLine_=__LINE__)

   deallocate(pFalseRoot)

end subroutine insert_top_down_sub

!------------------------------------------------------------------------------

function node_single_rotation_fn(pRoot, iDirection)    result(pSave)

  type (T_NODE), pointer :: pRoot
  integer (kind=T_INT) :: iDirection
  type (T_NODE), pointer :: pSave

  pSave => null()

  if(associated(pRoot) ) then

    select case(iDirection)

      case(LEFT)

        pSave => pRoot%pRight
        if(associated(pSave) ) then
          pRoot%pRight => pSave%pLeft
          pSave%pLeft => pRoot
        endif

      case(RIGHT)

        pSave => pRoot%pLeft
        if(associated(pSave) ) then
          pRoot%pLeft => pSave%pRight
          pSave%pRight => pRoot
        endif

      case default

        call assert(lFALSE,"Unhandled select case item: iDirection = "//trim(asChar(iDirection)), &
          trim(__FILE__), __LINE__)

    end select

    call set_node_color(pRoot, RED)
    call set_node_color(pSave, BLACK)


  endif

end function node_single_rotation_fn

!------------------------------------------------------------------------------

function node_double_rotation_fn(pRoot, iDirection)    result(pSave)

  type (T_NODE), pointer :: pRoot
  integer (kind=T_INT) :: iDirection
  type (T_NODE), pointer :: pSave

  nullify(pSave)

  if(associated(pRoot) ) then

    select case(iDirection)

      case(LEFT)

        pRoot%pRight &
          => node_single_rotation_fn(pRoot%pRight,flip_direction(iDirection))
        pSave => node_single_rotation_fn(pRoot, iDirection)

      case(RIGHT)

        pRoot%pLeft &
          => node_single_rotation_fn(pRoot%pLeft,flip_direction(iDirection))
        pSave => node_single_rotation_fn(pRoot, iDirection)

      case default

        call assert(lFALSE,"Unhandled select case item: iDirection = "//trim(asChar(iDirection)), &
          trim(__FILE__), __LINE__)

    end select

  endif

end function node_double_rotation_fn

!------------------------------------------------------------------------------

recursive subroutine traverse_entire_tree_sub(this, pCurrent)

   class(TIME_SERIES_COLLECTION) :: this
   type (T_NODE), pointer        :: pCurrent

   if (associated (pCurrent%pLeft) ) then           !! Take the left-hand path . . .
      call this%traverse(pCurrent%pLeft)
   end  if

   print *, quote(pCurrent%sNodeName)

   if (associated (pCurrent%pRight) ) then          !! Take the right-hand path.
      call this%traverse(pCurrent%pRight)
   end  if

end subroutine traverse_entire_tree_sub

!------------------------------------------------------------------------------

subroutine make_dot(this, pQ, pG, pF, pP, pS, pRoot, iLine_)

  class(TIME_SERIES_COLLECTION) :: this
   type (T_NODE), pointer, optional     :: pG      ! pointers to grandparent and parent
   type (T_NODE), pointer, optional     :: pP, pQ  ! pointers to parent and iterator and root
   type (T_NODE), pointer, optional     :: pS, pRoot ! pointer to sibling, root
   type (T_NODE), pointer, optional     :: pF      ! found node
   integer (kind=T_INT), optional       :: iLine_

  integer (kind=T_INT) :: LU_DOT
  integer (kind=T_INT) :: iNullCount
  character(len=24) :: sDateStr, sDateStrPretty
  character(len=256) :: sFilenamePrefix
  character(len=256) :: sCommandLineText
  character(len=24) :: sLine
  character(len=4) :: sImageNum

  if(present(iLine_) ) then
    sLine = "line_"//trim(asChar(iLine_))//"__"
  else
    sLine = ""
  endif

  iIMAGE_NUM = iIMAGE_NUM + 1
  write(sImageNum,fmt="(i04.4)") iIMAGE_NUM

  iNullCount = 0
  call GetSysTimeDate(sDateStr,sDateStrPretty)
  sFilenamePrefix = trim("tsproc_tree_structure_"//sImageNum//"_"//trim(sLine) &
      //trim(adjustl(sDateStr)))

  open(unit=newunit(LU_DOT), file=trim(sFilenamePrefix)//".dot", status = 'REPLACE')
  write(LU_DOT, fmt="(a)") 'digraph BST {'
  write(LU_DOT, fmt="(a)") '  size="20.0,4.0";'
  write(LU_DOT, fmt="(a)") '  ratio="fill";'
  write(LU_DOT, fmt="(a)") '  ordering="out";'

  if(.not. associated(this%pRoot) ) then
    write(LU_DOT, fmt="(a)") ''
  elseif((.not. associated(this%pRoot%pRight) ) &
     .and. (.not. associated(this%pRoot%pLeft) ) ) then

     ! define the node shape and color
     if(this%pRoot%iColor == RED) then
       write(LU_DOT, fmt="(a)") '    '//trim(this%pRoot%sNodename) &
         //' [fontname=sans,fontsize=12,shape=box,style=filled,color=red1, fontcolor=white ];'
     elseif(this%pRoot%iColor == BLACK) then
       write(LU_DOT, fmt="(a)") '    '//trim(this%pRoot%sNodename) &
         //' [fontname=sans,fontsize=12,shape=box,style=filled,color=black, fontcolor=white ];'
     else
       write(LU_DOT, fmt="(a)") '    '//trim(this%pRoot%sNodename) &
         //' [fontname=sans,fontsize=12,shape=box,style=filled,color=cyan, fontcolor=white ];'
     endif

    call null_pointer(this%pRoot)
    call null_pointer(this%pRoot)

  else
    call traverse_tree_connect_nodes_sub(this%pRoot)
  endif

  write(LU_DOT, fmt="(a)") '}'

  flush(LU_DOT)
  close(LU_DOT)

  ! issue command line directive to make a *.QDF from the *.dot file
  sCommandLineText = "dot -Tpdf "//trim(sFilenamePrefix)//".dot" &
    //" -o "//trim(sFilenamePrefix)//".pdf"
  call system(trim(sCommandLineText) )

contains

!   recursive subroutine traverse_tree_make_nodes_sub(pCurrent, iCount)
!
!      type (T_NODE), pointer        :: pCurrent
!      integer (kind=T_INT) :: iCount
!
!      if (associated (pCurrent%pLeft) ) then           !! Take the left-hand path . . .
!         call traverse_tree_make_nodes_sub(pCurrent%pLeft, iCount)
!      end  if
!
!       write(LU_DOT, fmt="(a)") 'node'//trim(asChar(iCount))//'[label = ' &
!          //'"<f0> |<f1> '//trim(pCurrent%sNodename)//'|<f2> "];'
!      iCount = iCount + 1
!
!      if (associated (pCurrent%pRight) ) then          !! Take the right-hand path.
!         call traverse_tree_make_nodes_sub( pCurrent%pRight, iCount)
!      endif
!
!   end subroutine traverse_tree_make_nodes_sub

  subroutine null_pointer(pNode)

    type (T_NODE), pointer        :: pNode

    iNullCount = iNullCount + 1
    write(LU_DOT, fmt="(a)") '    null'//trim(asChar(iNullCount) )//' [shape=point, color=black];'
    write(LU_DOT, fmt="(a)") '    '//quote(pNode%sNodename)//' -> null' &
       //trim(asChar(iNullCount) )//';'

  end subroutine null_pointer

  subroutine set_pointer_target(pNode, sLabel)

    type (T_NODE), pointer        :: pNode
    character (len=*) :: sLabel

     if(associated(pNode) ) then
       write(LU_DOT, fmt="(a)") '    '//quote(sLabel)//' -> ' &
          //quote(pNode%sNodename)//';'
     else
       call null_pointer(pNode)
     endif

  end subroutine set_pointer_target


  subroutine set_node_color_priv(pNode)

    type (T_NODE), pointer        :: pNode

    ! define the node shape and color
    if(pNode%iColor == RED) then
      write(LU_DOT, fmt="(a)") '    '//quote(pNode%sNodename) &
        //' [fontname=sans,fontsize=12,shape=box,style=filled,color=red1, fontcolor=white ];'
    elseif(pNode%iColor == BLACK) then
      write(LU_DOT, fmt="(a)") '    '//quote(pNode%sNodename) &
        //' [fontname=sans,fontsize=12,shape=box,style=filled,color=black, fontcolor=white ];'
    else
      write(LU_DOT, fmt="(a)") '    '//quote(pNode%sNodename) &
        //' [fontname=sans,fontsize=12,shape=box,style=filled,color=cyan, fontcolor=white ];'
    endif

  end subroutine set_node_color_priv

  subroutine set_pointer_color(sLabel, sColor, sTextColor_)

    character (len=*) :: sLabel
    character (len=*) :: sColor
    character (len=*), optional :: sTextColor_

    ! [ LOCALS ]
    character (len=12) :: sTextColor

    if(present(sTextColor_) ) then
      sTextColor = sTextColor_
    else
      sTextColor = "white"
    endif

    ! define the node shape and color
    ! note: see http://joe.milbourn.org.uk/notes/Graphviz:__Add_edges_without_changing_node_rank/
    write(LU_DOT, fmt="(a)") '    '//quote(sLabel) &
      //' [fontname=sans,fontsize=12,shape=egg,style=filled,color=' &
      //trim(sColor)//', fontcolor='//trim(sTextColor)//', constraint=false ];'

  end subroutine set_pointer_color

subroutine add_pointers(sNodename)

  character(len=*), intent(in) :: sNodename

  if(present(pRoot) ) then
    if(associated(pRoot) ) then
      if(str_compare(sNodename,pRoot%sNodename) ) then
        call set_pointer_color("pRoot", "coral", "black")
        call set_pointer_target(pRoot,"pRoot")
      endif
    endif
  endif

  if(present(pQ) ) then
    if(associated(pQ) ) then
      if(str_compare(sNodename,pQ%sNodename) ) then
        call set_pointer_color("pQ", "navy")
        call set_pointer_target(pQ,"pQ")
      endif
    endif
  endif

  if(present(pG) ) then
    if(associated(pG) ) then
      if(str_compare(sNodename,pG%sNodename) ) then
        call set_pointer_color("pG", "green", "black")
        call set_pointer_target(pG,"pG")
      endif
    endif
  endif

  if(present(pF) ) then
    if(associated(pF) ) then
      if(str_compare(sNodename,pF%sNodename) ) then
        call set_pointer_color("pF_pT", "forestgreen", "yellow")
        call set_pointer_target(pF,"pF_pT")
      endif
    endif
  endif

  if(present(pS) ) then
    if(associated(pS) ) then
      if(str_compare(sNodename,pS%sNodename) ) then
        call set_pointer_color("pS", "skyblue")
        call set_pointer_target(pS,"pS")
      endif
    endif
  endif

  if(present(pP) ) then
    if(associated(pP) ) then
      if(str_compare(sNodename,pP%sNodename) ) then
        call set_pointer_color("pP", "purple")
        call set_pointer_target(pP,"pP")
      endif
    endif
  endif

end subroutine add_pointers


  recursive subroutine traverse_tree_connect_nodes_sub(pCurrent)

     type (T_NODE), pointer        :: pCurrent

     if (associated (pCurrent%pLeft) ) then           !! Take the left-hand path . . .
        call traverse_tree_connect_nodes_sub( pCurrent%pLeft)
     end  if

     ! define the node shape and color
     call set_node_color_priv(pCurrent)


     if(associated(pCurrent%pLeft) ) then
       write(LU_DOT, fmt="(a)") '    '//quote(pCurrent%sNodename)//' -> ' &
          //quote(pCurrent%pLeft%sNodename)//';'
     else
       iNullCount = iNullCount + 1
       write(LU_DOT, fmt="(a)") '    null'//trim(asChar(iNullCount) )//' [shape=point, color=black];'
       write(LU_DOT, fmt="(a)") '    '//quote(pCurrent%sNodename)//' -> null' &
          //trim(asChar(iNullCount) )//';'
     endif

     if(associated(pCurrent%pRight) ) then
       write(LU_DOT, fmt="(a)") '    '//quote(pCurrent%sNodename)//' -> ' &
          //quote(pCurrent%pRight%sNodename)//';'
     else
       iNullCount = iNullCount + 1
       write(LU_DOT, fmt="(a)") '    null'//trim(asChar(iNullCount) )//' [shape=point, color=black];'
       write(LU_DOT, fmt="(a)") '    '//quote(pCurrent%sNodename)//' -> null' &
          //trim(asChar(iNullCount) )//';'
     endif

     call add_pointers(pCurrent%sNodename)

     if (associated (pCurrent%pRight) ) then          !! Take the right-hand path.
        call traverse_tree_connect_nodes_sub( pCurrent%pRight)
     endif

  end subroutine traverse_tree_connect_nodes_sub

end subroutine make_dot

!------------------------------------------------------------------------------

recursive subroutine traverse_tree_summarize_series_sub(this, pCurrent)

   class(TIME_SERIES_COLLECTION) :: this
   type (T_NODE), pointer        :: pCurrent

   ! [ LOCALS ]
   integer (kind=T_INT) :: iMemoryInBytes

   if (associated (pCurrent%pLeft) ) then           !! Take the left-hand path . . .
      call this%summarizeSeries(pCurrent%pLeft)
   end  if

   if(associated(pCurrent%pTS)) then
     iMemoryInBytes = sizeof(pCurrent%pTS) + sizeof(pCurrent%pTS%tData)
     write(LU_STD_OUT,fmt="(a18,a10,'-',a10,i10, f11.2,f11.2,f11.2,3x,i6,'k')") &
       pCurrent%pTS%sSeriesName, &
       pCurrent%pTS%tStartDate%prettyDate(), &
       pCurrent%pTS%tEndDate%prettyDate(), &
       count(pCurrent%pTS%tData%lValid), &
       MINVAL(pCurrent%pTS%tData%rValue, pCurrent%pTS%tData%lValid), &
       SUM(pCurrent%pTS%tData%rValue, pCurrent%pTS%tData%lValid) &
           / count(pCurrent%pTS%tData%lValid), &
       MAXVAL(pCurrent%pTS%tData%rValue, pCurrent%pTS%tData%lValid), &
       iMemoryInBytes / 1024
   endif

   if (associated (pCurrent%pRight) ) then          !! Take the right-hand path.
      call this%summarizeSeries(pCurrent%pRight)
   end  if

end subroutine traverse_tree_summarize_series_sub

!------------------------------------------------------------------------------

recursive subroutine traverse_tree_summarize_table_sub(this, pCurrent)

   class(TIME_SERIES_COLLECTION) :: this
   type (T_NODE), pointer        :: pCurrent

   ! [ LOCALS ]
   integer (kind=T_INT) :: iMemoryInBytes

   if (associated (pCurrent%pLeft) ) then           !! Take the left-hand path . . .
      call this%summarizeTable(pCurrent%pLeft)
   end  if

   if(associated(pCurrent%pTable)) then
     iMemoryInBytes = sizeof(pCurrent%pTable) + sizeof(pCurrent%pTable%tTableData)
     write(LU_STD_OUT,fmt="(a18,a10,'-',a10, t47, a)") &
       pCurrent%pTable%sSeriesName, &
       pCurrent%pTable%tStartDate%prettyDate(), &
       pCurrent%pTable%tEndDate%prettyDate(), &
       TABLE_TYPE(pCurrent%pTable%iTableType)
   endif

   if (associated (pCurrent%pRight) ) then          !! Take the right-hand path.
      call this%summarizeTable(pCurrent%pRight)
   end  if

end subroutine traverse_tree_summarize_table_sub

!------------------------------------------------------------------------------

!! This function procedure searches a binary search tree for the given element KEY.  It
!! returns the location of the element, or a null pointer if the element could not be found.
function find_node_in_tree_fn (this,sNodeName)   result (sPosition)
   !! INCOMING:   Key    = the given value to be matched with an element of the tree.
   !!       Root  = a pointer to the root of the tree to be searched.
   !! RETURNS: A pointer to the element (if Key is found in the tree), or a null pointer (if not found)

   class(TIME_SERIES_COLLECTION) :: this
   character (len=*), intent(in)   :: sNodeName
   type (T_NODE), pointer          :: sPosition

   this%pCurrent => this%pRoot
   nullify(sPosition)

   do
      if (.not. associated(this%pCurrent ) ) then
         exit
      end if

      if ( LGT(this%pCurrent%sNodeName, sNodeName) ) then                !! Take the left path.
!      print *, quote(this%pCurrent%sNodeName)//" (tree) > (target) ",quote(sNodeName)
         this%pCurrent => this%pCurrent%pLeft
      else if (LLT(this%pCurrent%sNodeName, sNodeName) ) then           !! Take the right path.
!      print *, quote(this%pCurrent%sNodeName)//" (tree) < (target) ",quote(sNodeName)
         this%pCurrent => this%pCurrent%pRight
      else                                        !! The desired element was found.
         sPosition => this%pCurrent
!      print *, quote(this%pCurrent%sNodeName)//" (tree) == (target) ",quote(sNodeName)
         return
      end  if
   end  do

   nullify (sPosition)                             !! The element did not exist in the tree.
   call this%makeDot()
   call assert(lFALSE,"Failed to find series "//quote(sNodeName)//" in tree structure.", &
     trim(__FILE__), __LINE__)

end function find_node_in_tree_fn

!------------------------------------------------------------------------------

!! This procedure rebalances and re-colors the nodes of a Red-Black sorted binary tree,
!! following an insertion.  The node pointed at by X has just been inserted into the tree,
!! and is the node where one of the 4 properties of Red-Black trees may have been violated.
!! After a breach of Rule 1 has been rectified, X is adjusted to point at its grandparent,
!! where tests for a breach of the rules is again carried out, and so on for each grandparent
!! in turn.  If Rules 2 or 3 are breached, one or two rotations of a branch of the tree are
!! carried out (about the current node X), and the procedure terminates.
!! When this procedure terminates, the root node is black, and the tree is quasi-balanced,
!! and the four properties of Red-Black trees are satisfied:
!!
!! Rule 1. Every node is either red or black;
!! Rule 2. Every null pointer is considered to be pointing to an imaginary black node;
!! Rule 3. If a node is red, then both its children are black; and
!! Rule 4. Every direct path from the root to a leaf contains the same number of black nodes.

  subroutine rebalance_tree_after_insert_sub(this)
      !! INCOMINQ: pCurrent = a pointer to a (red) node that has been inserted in the tree.
      class(TIME_SERIES_COLLECTION) :: this

      ! [ LOCALS ]
      type (T_NODE), pointer         :: pX, pY
      logical                      :: lRedUncle
      logical                      :: lIterating

      pX => this%pCurrent

      !! We execute this loop, and re-execute it,
      !! when pX and its parent are both red.
      do
         !! Begin barrage of tests to figure out whether we should even be here...
         !! Cannot iterate when we are at the root.
         lIterating = .not. associated (pX, this%pRoot)
         !! Current node must be red...
         if (lIterating) lIterating = (pX%iColor == RED)
         !! There must be a parent . . . .
         if (lIterating) lIterating = associated(pX%pParent)
         !! Qarent must must be red . . . .
         if (lIterating) lIterating = (pX%pParent%iColor == RED)
         !! . . . and there must be a grandparent.
         if (lIterating) lIterating = associated (pX%pParent%pParent)

         !! We enter this loop when pX and its parent are both red.
         !!  *AND* when its grandparent exists...
         if (.not. lIterating) exit

         !! If true, the parent is a left node of pX's grandparent.
         if (associated (pX%pParent, pX%pParent%pParent%pLeft) ) then

            !! Get the address of the uncle.
            pY => pX%pParent%pParent%pRight
            ! test to make sure uncle isn't a null pointer, and that uncle is RED
            lRedUncle = associated(pY)
            if (lRedUncle)  lRedUncle = (pY%iColor == RED)

            !! CASE 1.  If TRUE, There is an uncle.  pX and its parent and
            !! uncle are all red.  Fix violation of Rule 3.
            if (lRedUncle) then
               !! The parent must be black.
               pX%pParent%iColor = BLACK
               !! The uncle must be black.
               pY%iColor = BLACK
               !! The grandparent must be red.
               pX%pParent%pParent%iColor = RED
               !! Move 2 places up the tree, to the grandparent.
               pX => pX%pParent%pParent
            else !! The uncle is black, or is non-existent.

               !! CASE 2. Move up the tree
               if (associated (pX, pX%pParent%pRight) ) then
                  pX => pX%pParent
                  call this%rotateLeft(pX)
               end  if
               !! CASE 3.
               pX%pParent%iColor = BLACK
               pX%pParent%pParent%iColor = RED
               call this%rotateRight(pX%pParent%pParent)
            end  if
         !! This segment is the mirror image of the code for the "then" part,
         !! with the words Right and Left interchanged.
         else  !! The parent is a right node of pX's grandparent.
            !! Qet the address of the uncle.
            pY => pX%pParent%pParent%pLeft
            lRedUncle = associated(pY)
            if (lRedUncle) lRedUncle = (pY%iColor == RED)

            !! CASE 1.
            if (lRedUncle) then
               !! The parent must be black.
               pX%pParent%iColor = BLACK
               !! The uncle must be black.
               pY%iColor = BLACK
               !! The grandparent must be red.
               pX%pParent%pParent%iColor = RED
               !! Move 2 places up the tree, to the grandparent.
               pX => pX%pParent%pParent
            else !! pX and its parent are red, but its uncle is black
                 !! or is missing.  Fix violation of Rule 3.
               if (associated (pX, pX%pParent%pLeft) ) then !! CASE 2.
                  pX => pX%pParent                   !! Move up the tree.
                  call this%rotateRight(pX)
               end  if
               !! CASE 3.
               pX%pParent%iColor = BLACK
               pX%pParent%pParent%iColor = RED
               call this%rotateLeft(pX%pParent%pParent)
            end  if
         end  if
      end  do

      this%pRoot%iColor = BLACK                          !! Ensure that the root is black.
      this%pCurrent => this%pRoot

  end subroutine rebalance_tree_after_insert_sub

!------------------------------------------------------------------------------

!! This subroutine deletes a node from a Red-Black binary tree.
!! There are four main sections.  In the first section, the tree is checked to see if it
!! is empty, and if it is, the subroutine quits.
!! The second section deals with the case where the node to be deleted has two children.
!! The third section deals with the straight-forward case where the node to be deleted
!! has no children.
!! The fourth section deals with the case where the node to be deleted has one child.
  subroutine  delete_node_from_tree_sub(this, sNodename)
      !! INCOMING: pTarget points at the node to be deleted.
      class(TIME_SERIES_COLLECTION) :: this
      character (len=*), intent(in) :: sNodename

      ! [ LOCALS ]
      type (T_NODE), pointer         :: pChild, pX, pTarget

      pTarget => this%getNode(sNodename)

      pX => pTarget

!! Case 1:  the tree is empty.
      if (.not. associated (pX) ) then             !! The tree is empty.
         return
      end  if

!! Case 2:  the node has 2 children.
      !! We find the successor node, move the content of that node to the current node pX,
      !! and then delete the successor node.
      if (associated (pX%pLeft) .and. associated (pX%pRight) ) then !! Find the next-largest node.
                                                                                !! The next-largest node is found in the right
                                                                                !! subtree.
         pChild => pX%pRight

         do
            if (.not. associated (pChild%pLeft) ) then !! Seek the left-most node of the subtree.
               exit
            end  if
            pChild => pChild%pLeft
         end  do

         ! upon exit, pChild is the successor node to pX

!         pX%Item = pChild%Item                      !! Replace the Item to be deleted by its successor.
         call this%clear(pX)
         pX%sNodename = pChild%sNodename
         pX%pTS => pChild%pTS
         pX%pTable => pChild%pTable

         !! pChild is a node to be deleted.
         ! *** we're REASSIGNING pX here...
         pX => pChild                               !! The successor is now due for deletion.
                                                    !! This will be handled by Case 3 (the node is a
                                                    !! leaf) or by Case 4 (the node has one child).
      end if

      !! Now delete node pX.
!! Case 3:  the node has no children.
      if (.not. associated (pX%pRight) .and. .not. associated (pX%pLeft) ) then
                                                                                !! The node is a leaf.
         if (.not. associated (pX%pParent) ) then   !! Node pX is the root.
!            nullify (pX)
!            call this%deallocateNode(pX)
            call this%clear(pX)
            if (associated(pX) ) deallocate(pX)
            nullify (this%pRoot)
            return
         end  if
         if (pX%iColor == BLACK) then            !! If the color of the node to be deleted is black . . .
            call this%restructureTree(pX)
         end  if
         if (associated (pX, pX%pParent%pRight) ) then!! Chop off the leaf pX.
            nullify (pX%pParent%pRight)              !! Remove pX which is a right child..
         else
            nullify (pX%pParent%pLeft)               !! Remove pX which is a left child.
         end  if
!         nullify (pX)
            call this%clear(pX)
            if ( associated(pX) )  deallocate(pX)
         return
      end  if

!! Case 4:  the node has one child.  First find out which it is, and then splice it out.
      if (.not. associated (pX%pRight) ) then
         pChild => pX%pLeft                          !! It's a left child of pX.
      else
         pChild => pX%pRight                         !! It's a right child of pX.
      end  if

      if (.not. associated (pX%pParent) ) then      !! Node pX is the root.
         this%pRoot => pChild                            !! pX's only child becomes the root.
!         call this%deallocateNode(pX)
         call this%clear(pX)
         if ( associated(pX) ) deallocate(pX)
!         nullify (pX)                           !! Get rid of pX.
         nullify (this%pRoot%pParent)                    !! The root can't have a parent.
         this%pRoot%iColor = Black                       !! Color it black . . .
         return                                   !! . . . and quit.
      else                                        !! Node pX is not the root.       !! Splice out the node.
         !! Make the grandparent point at the child.
         if (associated (pX, pX%pParent%pRight) ) then!! Node pX is a right child.
            pX%pParent%pRight => pChild
         else                                     !! Node pX is a left child.
            pX%pParent%pLeft => pChild
         end  if

         pChild%pParent => pX%pParent                 !! Make the child point up at the grandparent.

      end  if

      !! The next segment will restructure the tree when the deleted node is black.  However,
      !! there is one particular case when the deleted note is black and its child is red.  In this
      !! situation, its child is merely re-colored black, thus restoring the deficiency of one black node.
      !! The following code indirectly achieves this: RESTRUCTURE_AFTER_DELETION is called,
      !! its main loop is not executed, and the subroutine terminates after re-coloring the child black.
      if (pX%iColor == BLACK) then               !! If the color of the node to be deleted is black . . .
         call this%restructureTree(pChild)
      end  if

!      call this%deallocateNode(pX)
      call this%clear(pX)
      if ( associated(pX) ) deallocate(pX)
      nullify(pX)

  end subroutine delete_node_from_tree_sub

!------------------------------------------------------------------------------

  subroutine clear_node_contents_sub(this, pNode)

    class(TIME_SERIES_COLLECTION) :: this
    type (T_NODE), pointer :: pNode

    ! [ LOCALS ]
    character (len=256) :: sDescription
    character (len=MAXNAMELENGTH) :: sSeriesname

    if(associated(pNode) ) then
      if(associated(pNode%pTS) ) then
        sDescription = pNode%pTS%sDescription
        sSeriesname = pNode%pTS%sSeriesname
        deallocate(pNode%pTS)
        this%iNumTimeSeries = this%iNumTimeSeries - 1
        call echolog("")
        call echolog("Deleted: "//quote(sSeriesname) )
        if(lVERBOSE) then
          call echolog("  [ "//trim(sDescription)//". ]" )
          call echolog("  [ There are now "//trim(asChar(this%iNumTimeSeries) ) &
            //" time series in memory. ]")
        endif
      endif

      if(associated(pNode%pTable) ) then
        sDescription = pNode%pTable%sDescription
        sSeriesname = pNode%pTable%sSeriesname
        deallocate(pNode%pTable)
        this%iNumTables = this%iNumTables - 1
        call echolog("")
        call echolog("Deleted: "//quote(sSeriesname) )
        if(lVERBOSE) then
          call echolog("  [ "//trim(sDescription)//". ]" )
          call echolog("  [ There are now "//trim(asChar(this%iNumTables) ) &
            //" tables in memory. ]")
        endif
      endif

    endif

  end subroutine clear_node_contents_sub

!------------------------------------------------------------------------------

!! This procedure re-colors nodes and/or restructures a Red-Black tree following a deletion.
!! This routine is entered when the deleted node was black.  (Node X is the child of the
!! deleted node).
!! The crux is to balance the two subtrees whose roots are node X and its brother.
!! Qeneral strategy:
!! (1) Simple case: If the child of the deleted node is red, change it to black and finish.
!! (2) Qeneral case:  Either:
!! (2a) Qerform a rotation about the parent, so as to bring a node into the subtree, to
!!      compensate for the deleted node.  In this case, color the new node black and finish.
!! (2b) Change a node of the sibling's subtree from black to red, and move up the tree.
!!       In this event, repeat step (2).
  subroutine restructure_tree_after_deletion_sub(this, pTarget)
      !! INCOMINQ: Current = points at the child of the deleted node.
      class(TIME_SERIES_COLLECTION) :: this
      type (T_NODE), pointer :: pTarget

      type (T_NODE), pointer         :: pX
      type (T_NODE), pointer         :: pBrother

      pX => pTarget

TREE_CLIMBINQ_LOOQ:                 &
      do
         if ( associated(pX, this%pRoot) .or. (pX%iColor == RED)) then
            exit                                  !! Quit when we are at the root, or if the node is red.
         end  if
         !! The crux balances the two subtrees whose roots are at node pX and its brother.
         if (associated (pX, pX%pParent%pLeft) ) then !! Node pX is a left child.
            pBrother => pX%pParent%pRight
            if (pBrother%iColor == RED) then
                                                                                !! Case 1: Rotate left about pX's parent, and re-colo
               pBrother%iColor = BLACK
               pX%pParent%iColor = RED
               call  this%rotateLeft(pX%pParent)
               pBrother => pX%pParent%pRight
            end  if                               !!  . . . and move on to apply case 2.

            if ( get_node_color(pBrother%pLeft) == BLACK .and.   &  !! If both children of the
               get_node_color(pBrother%pRight) == BLACK) then !! brother  are black . . .
               !! Case 2: Color the brother red, and move up the tree.
               pBrother%iColor = RED
               pX => pX%pParent                      !! Move up the tree.
            else
               if (get_node_color (pBrother%pRight) == BLACK) then
                                                                                !! Case 3: Rotate right about the brother, and re-co
                  if (associated(pBrother%pLeft) ) then
                     pBrother%pLeft%iColor = BLACK
                  end  if
                  pBrother%iColor = RED
                  call  this%rotateRight(pBrother)
                  pBrother => pX%pParent%pRight
               end  if                            !! . . . and go on to Case 4.

               !! Case 4: Rotate left about pX's parent, which brings an extra node to
               !! the subtree through pX, and which is then colored black.
               pBrother%iColor = pX%pParent%iColor
               pX%pParent%iColor = BLACK
               pBrother%pRight%iColor = BLACK
               call  this%rotateLeft(pX%pParent)       !! Brings a black node to the left
                                                                                !! subtree; pLeft & pRight subtrees are balanced.
               exit TREE_CLIMBINQ_LOOQ            !! Quit, rebalancing is complete.
            end  if
         else                                     !! Node pX is a right child.
            !! Same as the THEN clause, with "pRight" and "pLeft" interchanged.
            pBrother => pX%pParent%pLeft
            if (pBrother%iColor == RED) then
                                                                                !! Case 1: Rotate left about pX's parent, and re-colo
               pBrother%iColor = BLACK
               pX%pParent%iColor = RED
               call  this%rotateRight(pX%pParent)
               pBrother => pX%pParent%pLeft
            end  if                               !!  . . . and move on to apply case 2.

            if ( get_node_color(pBrother%pLeft) == BLACK .and.   &  !! If both children of the
               get_node_color(pBrother%pRight) == BLACK) then !! brother  are black . . .
               !! Case 2: Color the brother red, and move up the tree.
               pBrother%iColor = RED
               pX => pX%pParent                      !! Move up the tree.
            else
               if (get_node_color(pBrother%pLeft) == BLACK) then
                                                                                !! Case 3: Rotate left about the brother, and re-col
                  if (associated(pBrother%pRight) ) then
                     pBrother%pRight%iColor = BLACK
                  endif
                  pBrother%iColor = RED
                  call  this%rotateLeft(pBrother)
                  pBrother => pX%pParent%pLeft
               endif                            !! . . . and go on to Case 4.

               !! Case 4: Rotate right about pX's parent, which brings an extra node to
               !! the subtree through pX, and which is then colored black.
               pBrother%iColor = pX%pParent%iColor
               pX%pParent%iColor = BLACK
               pBrother%pLeft%iColor = BLACK
               call this%rotateRight(pX%pParent)      !! Brings a black node to the right
                                                                                !! subtree; pLeft & pRight subtrees are balanced.
               exit TREE_CLIMBINQ_LOOQ            !! Quit, rebalancing is complete.
            end  if
         end  if

      end  do  TREE_CLIMBINQ_LOOQ

      pX%iColor = BLACK                             !! Having encountered a red node, color it black and quit.

  end subroutine restructure_tree_after_deletion_sub

!------------------------------------------------------------------------------

  function get_right_pointer(pNode)   result(pMember)

    type (T_NODE), pointer            :: pNode
    type (T_NODE), pointer            :: pMember

    nullify(pMember)

    if(associated(pNode) ) then

      pMember => pNode%pRight

    else

      pMember => null()

    endif

  end function get_right_pointer

!------------------------------------------------------------------------------

  function get_left_pointer(pNode)   result(pMember)

    type (T_NODE), pointer            :: pNode
    type (T_NODE), pointer            :: pMember

    nullify(pMember)

    if(associated(pNode) ) then

      pMember => pNode%pLeft

    else

      pMember => null()

    endif

  end function get_left_pointer


  subroutine set_child(pNode, iDirection, pTarget)

    type (T_NODE), pointer  :: pNode
    integer (kind=T_INT)    :: iDirection
    type (T_NODE), pointer  :: pTarget


    if(associated(pNode) ) then

      if(iDirection == RIGHT) then

        pNode%pRight => pTarget

      else  ! direction is LEFT

        pNode%pLeft => pTarget

      endif

    else

      call warn(lFALSE, "Attempt to assign target to a null pointer!", &
          trim(__FILE__), __LINE__)

    endif

  end subroutine set_child

  function get_child(pNode, iDirection)   result(pChild)

    type (T_NODE), pointer  :: pNode
    integer (kind=T_INT)    :: iDirection
    type (T_NODE), pointer  :: pChild

    pChild => null()

    if(associated(pNode) ) then

      if(iDirection == RIGHT) then

        pChild => pNode%pRight

      else  ! direction is LEFT

        pChild => pNode%pLeft

      endif

    endif

  end function get_child

!------------------------------------------------------------------------------

  !! This function returns the color of the specified node, or black if the node does not exist.
  function get_node_color(pNode) result (iNodeColor)
    !! INCOMINQ: Node_Qtr = a pointer to the node whose color is to be obtained.
    type (T_NODE), pointer            :: pNode
    integer (kind=T_INT) :: iNodeColor

    if (associated (pNode)) then
      iNodeColor = pNode%iColor
    else
      iNodeColor = BLACK
    end  if

  end function get_node_color

!------------------------------------------------------------------------------

  function flip_direction(iDirection)  result(iFlippedDirection)

    integer (kind=T_INT) :: iDirection
    integer (kind=T_INT) :: iFlippedDirection

    if(iDirection == LEFT) then
      iFlippedDirection = RIGHT
    else
      iFlippedDirection = LEFT
    endif

  end function flip_direction

!------------------------------------------------------------------------------

  !! This function returns direction: right if true, left if false
  function get_direction(lCondition) result (iDirection)

    logical (kind=T_LOGICAL) :: lCondition
    integer (kind=T_INT) :: iDirection

    if(lCondition) then
      iDirection = RIGHT
    else
      iDirection = LEFT
    endif


  end function get_direction

!------------------------------------------------------------------------------

  subroutine dump(pQ, pG, pT, pP, pS, pRoot, iDir, iLast, iLine)

   type (T_NODE), pointer        :: pG, pT         ! pointers to grandparent and parent
   type (T_NODE), pointer        :: pP, pQ, pRoot  ! pointers to parent and iterator and root
   type (T_NODE), pointer :: pS
   integer (kind=T_INT) :: iDir
   integer (kind=T_INT) :: iLast
   integer (kind=T_INT) :: iLine

   ! [ LOCALS ]
   character(len=24) :: sDateStr, sDateStrPretty
   character (len=5), dimension(0:1) :: COLOR
   character (len=4), dimension(0:1) :: DIRECTION

   COLOR(RED) = "RED  "
   COLOR(BLACK) = "BLACK"

   DIRECTION(LEFT) = "<==="
   DIRECTION(RIGHT) = "===>"

   !!! branch taken under ifort
   call GetSysTimeDate(sDateStr,sDateStrPretty)
   print *, trim(sDateStr)//": ", iLine
   print *, "   iDir:  "//DIRECTION(iDir)
   print *, "   iLast: "//DIRECTION(iLast)

   if(associated(pRoot) ) then
     print *, "pRoot => "//quote(pRoot%sNodename)//"  {"//COLOR(pRoot%iColor)//"}"
     if(associated(pRoot%pLeft) ) print *, "   pRoot%pLeft => "//quote(pRoot%pLeft%sNodename)//"  {"//COLOR(pRoot%pLeft%iColor)//"}"
     if(associated(pRoot%pRight) ) print *, "   pRoot%pRight => "//quote(pRoot%pRight%sNodename)//"  {"//COLOR(pRoot%pRight%iColor)//"}"
   else
     print *, "pRoot => NULL"
   endif

   if(associated(pG) ) then
     print *, "pG => "//quote(pG%sNodename)//"  {"//COLOR(pG%iColor)//"}"
     if(associated(pG%pLeft) ) print *, "   pG%pLeft => "//quote(pG%pLeft%sNodename)//"  {"//COLOR(pG%pLeft%iColor)//"}"
     if(associated(pG%pRight) ) print *, "   pG%pRight => "//quote(pG%pRight%sNodename)//"  {"//COLOR(pG%pRight%iColor)//"}"
   else
     print *, "pG => NULL"
   endif

   if(associated(pT) ) then
     print *, "pT (pF if deleting) => "//quote(pT%sNodename)//"  {"//COLOR(pT%iColor)//"}"
     if(associated(pT%pLeft) ) print *, "   pT/pF%pLeft => "//quote(pT%pLeft%sNodename)//"  {"//COLOR(pT%pLeft%iColor)//"}"
     if(associated(pT%pRight) ) print *, "   pT/pF%pRight => "//quote(pT%pRight%sNodename)//"  {"//COLOR(pT%pRight%iColor)//"}"
   else
     print *, "pT/pF => NULL"
   endif

   if(associated(pP) ) then
     print *, "pP => "//quote(pP%sNodename)//"  {"//COLOR(pP%iColor)//"}"
     if(associated(pP%pLeft) ) print *, "   pP%pLeft => "//quote(pP%pLeft%sNodename)//"  {"//COLOR(pP%pLeft%iColor)//"}"
     if(associated(pP%pRight) ) print *, "   pP%pRight => "//quote(pP%pRight%sNodename)//"  {"//COLOR(pP%pRight%iColor)//"}"
   else
     print *, "pP => NULL"
   endif

   if(associated(pS) ) then
     print *, "pS => "//quote(pS%sNodename)//"  {"//COLOR(pS%iColor)//"}"
     if(associated(pS%pLeft) ) print *, "   pS%pLeft => "//quote(pS%pLeft%sNodename)//"  {"//COLOR(pS%pLeft%iColor)//"}"
     if(associated(pS%pRight) ) print *, "   pS%pRight => "//quote(pS%pRight%sNodename)//"  {"//COLOR(pS%pRight%iColor)//"}"
   else
     print *, "pS => NULL"
   endif

   if(associated(pQ) ) then
     print *, "pQ => "//quote(pQ%sNodename)//"  {"//COLOR(pQ%iColor)//"}"
     if(associated(pQ%pLeft) ) print *, "   pQ%pLeft => "//quote(pQ%pLeft%sNodename)//"  {"//COLOR(pQ%pLeft%iColor)//"}"
     if(associated(pQ%pRight) ) print *, "   pQ%pRight => "//quote(pQ%pRight%sNodename)//"  {"//COLOR(pQ%pRight%iColor)//"}"
   else
     print *, "pQ => NULL"
   endif

   print *, "****"

  end subroutine dump

!------------------------------------------------------------------------------

  !! This function sets the color of the specified node, if it exists.
  subroutine set_node_color(pNode, iNodecolor)
    !! INCOMINQ: Node_Qtr = a pointer to the node whose color is to be set.
    type (T_NODE), pointer    :: pNode
    integer (kind=T_INT) :: iNodeColor

    if (associated (pNode)) then
      pNode%iColor = iNodecolor
    endif

   end subroutine set_node_color

!------------------------------------------------------------------------------

  subroutine rotate_node_left_sub(this, pPivot)

    class(TIME_SERIES_COLLECTION) :: this
    type(T_NODE), pointer ::  pPivot
    type (T_NODE), pointer :: pX, pY

    ! set a pointer to the pPivot node (node about which rotation will take place)
    pX => pPivot

    ! pY is the right child of pX
    pY => pX%pRight

    ! swap left subtree of pY into right subtree of pX
    pX%pRight => pY%pLeft

    ! make pX the parent of pY's left subtree
    if(associated(pY%pLeft) ) pY%pLeft%pParent => pX

    ! pX's parent becomes pY's parent
    pY%pParent => pX%pParent
    if(.not. associated(pX%pParent) )then
      this%pRoot => pY
    elseif( associated(pX, pX%pParent%pLeft) ) then
      ! make left pointer of pX's parent point to pY
      pX%pParent%pLeft => pY
    else
      ! make right pointer of pX's parent point to pY
      pX%pParent%pRight => pY
    endif

    pY%pLeft => pX
    pX%pParent => pY

  end subroutine rotate_node_left_sub

!------------------------------------------------------------------------------

  subroutine rotate_node_right_sub(this, pPivot)

    class(TIME_SERIES_COLLECTION) :: this
    type(T_NODE), pointer ::  pPivot
    type (T_NODE), pointer :: pX, pY

    ! set a pointer to the pPivot node (node about which rotation will take place)
    pX => pPivot

    ! pY is the left child of pX
    pY => pX%pLeft

    ! swap right subtree of pY into left subtree of pX
    pX%pLeft => pY%pRight

    ! make pX the parent of pY's right subtree
    if(associated(pY%pRight) ) pY%pRight%pParent => pX

    ! pX's parent becomes pY's parent
    pY%pParent => pX%pParent
    if(.not. associated(pX%pParent) )then
      this%pRoot => pY
    elseif( associated(pX, pX%pParent%pRight) ) then
      ! make right pointer of pX's parent point to pY
      pX%pParent%pRight => pY
    else
      ! make left pointer of pX's parent point to pY
      pX%pParent%pLeft => pY
    endif

    pY%pRight => pX
    pX%pParent => pY

  end subroutine rotate_node_right_sub

!------------------------------------------------------------------------------

  subroutine add_ts_link_sub(this, pNewSeries)

    class(TIME_SERIES_COLLECTION) :: this
    type(T_TIME_SERIES), pointer ::  pNewSeries

    ! [ LOCALS ]
    type(T_TIME_SERIES), pointer :: pCurrentTS
    integer (kind=T_INT) :: iCount
    integer (kind=T_INT) :: iStat
    integer (kind=T_INT) :: i
    integer (kind=T_INT) :: iIteration

    pNewSeries%pNext => null()
    pNewSeries%pPrevious => null()
    pCurrentTS => null()

!    print *, "associated(this%pTS_Head): ",associated(this%pTS_Head)
!    if(associated(this%pTS_Head)) print *, quote(this%pTS_Head%sSeriesName)

    if(.not. associated(this%pTS_Head)) then  ! no series are stored yet; set head node to
                                              ! pNewSeries
      this%pTS_Head => pNewSeries
!      print *, "associated(this%pTS_Head): ",associated(this%pTS_Head)
      this%pTS_Head%pNext => null()
      this%pTS_Head%pPrevious => null()
      this%iNumTimeSeries = this%iNumTimeSeries + 1

      call echolog("  Added: "//trim(pNewSeries%sDescription)//".")
      call echolog("    [ date range: "//pNewSeries%tStartDate%listdate()//" to " &
         //pNewSeries%tEndDate%listdate()//" ]")
      call echolog("    [There is now "//trim(asChar(this%iNumTimeSeries))//" time series in memory.]")
      call echolog("")

    else

      iIteration = 0

      ! reset pCurrentTS to the head node

      this%pTS_Head%pPrevious => pNewSeries
      pNewSeries%pNext => this%pTS_Head
      this%pTS_Head => pNewSeries

      this%iNumTimeSeries = this%iNumTimeSeries + 1
      call echolog("  Added: "//trim(pNewSeries%sDescription)//".")
      call echolog("    [ date range: "//pNewSeries%tStartDate%listdate()//" to " &
         //pNewSeries%tEndDate%listdate()//" ]")
      call echolog("    [There are now "//trim(asChar(this%iNumTimeSeries))//" time series in memory.]")
      call echolog("")

!      do

!        iIteration = iIteration + 1
!        print *, trim(asChar(iIteration))//") "//quote(pCurrentTS%sSeriesName)
!        if(associated(pCurrentTS%pPrevious)) print *,quote(pCurrentTS%pPrevious%sSeriesName)//"<=="
!        if(associated(pCurrentTS%pNext)) print *,"                  ==>"//quote(pCurrentTS%pNext%sSeriesName)

        ! recurse through list until we come to the end of the line
!        if(associated(pCurrentTS%pNext) ) then
!          pCurrentTS => pCurrentTS%pNext
!          cycle
!        else
          ! update forward-pointing and backward-pointing pointers
!          if(associated(pNewSeries%pPrevious)) then
!            call warn(lFALSE,"pNewSeries is being reallocated")
!            print *, " (was: "//quote(pNewSeries%pPrevious%sSeriesName)//")"
!          endif
!          pCurrentTS%pNext => pNewSeries
!          pNewSeries%pPrevious => pCurrentTS
!          pNewSeries%pNext => null()
!          this%iNumTimeSeries = this%iNumTimeSeries + 1
!          call echolog("  Added series "//quote(pNewSeries%sSeriesName)//". " &
!            //"There are now "//trim(asChar(this%iNumTimeSeries))//" time series in memory.")
!          call echolog("")
!                  print *, trim(asChar(iIteration))//") "//quote(pCurrentTS%sSeriesName)
!          print *, "NEW SERIES ADDED"
!          print *, "  current series:"//quote(pCurrentTS%sSeriesName)
!          if(associated(pCurrentTS%pPrevious)) print *,quote(pCurrentTS%pPrevious%sSeriesName)//"<=="
!          if(associated(pCurrentTS%pNext)) print *,"                  ==>"//quote(pCurrentTS%pNext%sSeriesName)
!
!          print *, "  new series:"//quote(pNewSeries%sSeriesName)
!          if(associated(pNewSeries%pPrevious)) print *,"  "//quote(pNewSeries%pPrevious%sSeriesName)//"<=="
!          if(associated(pNewSeries%pNext)) print *,"                    ==>"//quote(pNewSeries%pNext%sSeriesName)

!          exit
!        endif
!      enddo

    endif

    nullify(pNewSeries)
    nullify(pCurrentTS)

  end subroutine add_ts_link_sub

!------------------------------------------------------------------------------

  subroutine add_node_sub(this, pNewSeries, pNewTable)

    class(TIME_SERIES_COLLECTION) :: this
    type(T_TIME_SERIES), pointer, optional ::  pNewSeries
    type(T_TABLE), pointer, optional ::  pNewTable

    ! [ LOCALS ]
    type(T_NODE), pointer :: pParentOfCurrent
    type(T_NODE), pointer :: pNewNode

    nullify(pParentOfCurrent)

    ! set root and current pointer to root
    this%pCurrent => this%pRoot

    allocate(pNewNode)

    ! set the node pointer pTS to the series to be added
    if(present(pNewSeries) ) then
      if(associated(pNewSeries) ) then
        pNewNode%pTS => pNewSeries
        pNewNode%sNodeName = trim(adjustl(pNewSeries%sSeriesName) )
        this%iNumTimeSeries = this%iNumTimeSeries + 1
        call echolog("  Added: "//quote(pNewSeries%sSeriesname) )
        if(lVERBOSE) then
          call echolog("  [ "//trim(pNewSeries%sDescription)//". ]")
          call echolog("  [ date range: "//pNewSeries%tStartDate%listdate()//" to " &
             //pNewSeries%tEndDate%listdate()//" ]")
          call echolog("  [ There are now "//trim(asChar(this%iNumTimeSeries))//" time series in memory. ]")
          call echolog("")
        endif
      endif
    endif

    if(present(pNewTable) ) then
      if(associated(pNewTable) ) then
        pNewNode%pTable => pNewTable
        pNewNode%sNodeName = trim(adjustl(pNewTable%sSeriesName) )
        this%iNumTables = this%iNumTables + 1
        call echolog("  Added: "//quote(pNewTable%sSeriesname) )
        if(lVERBOSE) then
          call echolog("  [ "//trim(pNewTable%sDescription)//". ]")
          call echolog("  [ date range: "//pNewTable%tStartDate%listdate()//" to " &
             //pNewTable%tEndDate%listdate()//" ]")
          call echolog("  [ There are now "//trim(asChar(this%iNumTables))//" tables in memory. ]")
          call echolog("")
        endif
      endif
    endif

    pNewNode%iColor = RED

    call insert_top_down_sub(this, pNewNode)

      ! find a place in the tree to graft the new node to;
      ! travel from root to a node containing a null pointer

!         do
!
!           if(.not. associated(this%pCurrent) ) exit
!
!           pParentOfCurrent => this%pCurrent
!
!           ! if current node name > NEW node name, take LEFT path
!           if( LGT(this%pCurrent%sNodeName, pNewNode%sNodeName) ) then
!             this%pCurrent => this%pCurrent%pLeft
!
!           ! if current node name < NEW node name, take RIGHT path
!           elseif( LLT(this%pCurrent%sNodeName, pNewNode%sNodeName) ) then
!             this%pCurrent => this%pCurrent%pRight
!
!           else
!             call assert(lFALSE,"Series name "//quote(pNewNode%sNodeName) &
!               //" has already been used. ", &
!               trim(__FILE__),__LINE__)
!             exit
!           endif
!
!         enddo
!         ! upon exit, we have found a node whose left or right pointer field is null
!         ! pCurrent is NULL at this point as well
!
!         pNewNode%pParent => pParentOfCurrent
!         pNewNode%iColor = RED
!
!         if(.not. associated(pParentOfCurrent) ) then  ! we're at the root; color it BLACK
!
!           pNewNode%iColor = BLACK
!
!           this%pRoot => pNewNode
!         elseif( LLT(pNewNode%sNodeName,pParentOfCurrent%sNodeName) ) then
!           pParentOfCurrent%pLeft => pNewNode
!         else
!           pParentOfCurrent%pRight => pNewNode
!         endif
!
!         ! update global pointer to new node
!         this%pCurrent => pNewNode
!         nullify(pNewNode)
!
!  !     call this%makeDot()
!
!
!       ! must rebalance tree following an addition
!        call this%rebalanceTree()
!
!      call this%makeDot()

  end subroutine add_node_sub

!------------------------------------------------------------------------------

  subroutine add_table_link_sub(this, pNewTable)

    class(TIME_SERIES_COLLECTION) :: this
    type(T_TABLE), pointer ::  pNewTable

    ! [ LOCALS ]
    type(T_TABLE), pointer :: pCurrentTable => null()
    integer (kind=T_INT) :: iCount
    integer (kind=T_INT) :: iStat
    integer (kind=T_INT) :: i

    if(associated(this%pTable_Head)) then
      pCurrentTable => this%pTable_Head
      do
        ! recurse through list until we come to the end of the line
        if(associated(pCurrentTable%pNext) ) then
          pCurrentTable => pCurrentTable%pNext
          cycle
        else
          ! update forward-pointing and backward-pointing pointers
          pCurrentTable%pNext => pNewTable
          pNewTable%pPrevious => pCurrentTable
          pNewTable%pNext => null()
          this%iNumTables = this%iNumTables + 1
          call echolog("  Added table "//quote(pNewTable%sSeriesName)//". " &
            //"There are now "//trim(asChar(this%iNumTables))//" tables in memory.")
          call echolog("")

          exit
        endif
      enddo
    else
      this%pTable_Head => pNewTable
      this%pTable_Head%pNext => null()
      this%pTable_Head%pPrevious => null()
      this%iNumTables = this%iNumTables + 1
      pCurrentTable => this%pTable_Head

      call echolog("  Added table "//quote(pNewTable%sSeriesName)//". " &
        //"There is now "//trim(asChar(this%iNumTables))//" table in memory.")
      call echolog("")

    endif

!    nullify(pNewTable)
    nullify(pCurrentTable)

  end subroutine add_table_link_sub

!------------------------------------------------------------------------------

!   subroutine add_ts_sub(this, pTS)
!
!     ! add tTS to the COLLECTION of time series objects (this)
!
!     class(TIME_SERIES_COLLECTION) :: this
!     type(T_TIME_SERIES), pointer ::  pTS
!
!     ! [ LOCALS ]
!     type(T_TIME_SERIES), dimension(:), pointer :: pTempTS
!     integer (kind=T_INT) :: iCount
!     integer (kind=T_INT) :: iStat
!     integer (kind=T_INT) :: i
!
!     print *, "ENTERED the ADD_TS_SUB"
!
!     if(associated(this%pTS)) then
!
!       ! get the number of time series currently in time series container
!       iCount = size(this%pTS)
!
!       print *, "iCount = ",iCount
!       ! check to see whether this name has been used already
!       do i = 1,iCount
!         print *, quote(this%pTS(i)%sSeriesName)
!         call Assert(.not. str_compare(this%pTS(i)%sSeriesName, pTS%sSeriesName), &
!          "Series name "//trim(pTS%sSeriesName)//" has already been used", &
!          trim(__FILE__), __LINE__)
!       end do
!
! !      ! allocate memory for size of current TS + 1
!       allocate(pTempTS(iCount + 1),stat=iStat)
!       call Assert(iStat==0, "Unable to allocate temporary memory for time series", &
!         TRIM(__FILE__), __LINE__)
!
!       ! copy current TS objects
!       pTempTS = this%pTS
!       pTempTS%tData = this%pTS%tData
!
!       ! deallocate TS objects collection
!       deallocate(this%pTS, stat=iStat)
!       call Assert(iStat==0, "Unable to deallocate memory for time series", &
!         TRIM(__FILE__), __LINE__)
!
!       move_alloc(pTempTS, this%pTS)
!
!       ! allocate TS objects collection to include room for new object
! !      allocate(this%pTS(iCount + 1), stat=iStat)
! !      call Assert(iStat==0, "Unable to allocate memory for time series", &
! !        TRIM(__FILE__), __LINE__)
!
! !      this%pTS(1:iCount) = pTempTS
! !      this%pTS(1:iCount)%pData = tTempTS(1:iCount)%pData
!
!       ! ensure that the minimum and maximum date range fields are populated
!       ! before adding the TS object to the collection
! !      call tTS%findDateMinAndMax()
!
!       this%pTS(iCount+1) = pTS
!       this%pTS(iCount+1)%tData = pTS%tData
!
! !      deallocate(this%pTS, stat=iStat)
! !      call Assert(iStat==0, "Unable to deallocate memory for time series", &
! !        TRIM(__FILE__), __LINE__)
!
! !      this%pTS => tTempTS
!
!     else
!
!       allocate(this%pTS(1),stat=iStat)
!       call Assert(iStat==0, "Unable to allocate memory for time series", &
!         TRIM(__FILE__), __LINE__)
!
!       ! ensure that the minimum and maximum date range fields are populated
!       ! before adding the TS object to the collection
! !      call tTS%findDateMinAndMax()
!
!       this%pTS = pTS
!
! !      if(associated(this%pTS(1)%pData)) deallocate(this%pTS(1)%pData)
! !      allocate(this%pTS(1)%pData(size(tTS%pData)))
!
!       this%pTS(1)%tData = pTS%tData
!
!     endif
!
!     this%iNumTimeSeries = this%iNumTimeSeries + 1
!
!     nullify(pTS)
!
!   end subroutine add_ts_sub

!------------------------------------------------------------------------------

  subroutine add_table_sub(this, tTable)

    ! add tTS to the COLLECTION of time series objects (this)

    class(TIME_SERIES_COLLECTION) :: this
    type(T_TABLE) ::  tTable

    ! [ LOCALS ]
    type(T_TABLE), dimension(:), allocatable :: tTempTable
    integer (kind=T_INT) :: iCount
    integer (kind=T_INT) :: iStat
    integer (kind=T_INT) :: i

    if(allocated(this%tTable)) then

      ! get the number of tables currently in time series container
      iCount = size(this%tTable)

      ! check to see whether this name has been used already
      do i = 1,iCount
        call Assert(.not. str_compare(this%tTable(i)%sSeriesName, tTable%sSeriesName), &
         "Table name "//trim(tTable%sSeriesName)//" has already been used", &
         trim(__FILE__), __LINE__)
      end do

      ! allocate memory for size of current table
      allocate(tTempTable(iCount),stat=iStat)
      call Assert(iStat==0, "Unable to allocate temporary memory for table", &
        TRIM(__FILE__), __LINE__)

      ! make a copy of all previous table objects
      tTempTable = this%tTable

      ! deallocate table objects collection
      deallocate(this%tTable, stat=iStat)
      call Assert(iStat==0, "Unable to deallocate memory for table", &
        TRIM(__FILE__), __LINE__)

      ! allocate table objects collection to include room for new object
      allocate(this%tTable(iCount + 1), stat=iStat)
      call Assert(iStat==0, "Unable to allocate memory for table", &
        TRIM(__FILE__), __LINE__)

      this%tTable(1:iCount) = tTempTable

      this%tTable(iCount+1) = tTable

    else

      allocate(this%tTable(1),stat=iStat)
      call Assert(iStat==0, "Unable to allocate memory for table", &
        TRIM(__FILE__), __LINE__)

      this%tTable(1) = tTable

      if(allocated(this%tTable(1)%tTableData)) deallocate(this%tTable(1)%tTableData)
      allocate(this%tTable(1)%tTableData(size(tTable%tTableData)))

      this%tTable(1)%tTableData = tTable%tTableData

    endif

    this%iNumTables = this%iNumTables + 1

  end subroutine add_table_sub

!------------------------------------------------------------------------------

  subroutine remove_ts_link_sub(this, sSeriesName)

    class(TIME_SERIES_COLLECTION) :: this
    character (len=*) :: sSeriesName

    ! [ LOCALS ]
    type(T_TIME_SERIES), pointer :: pCurrent
    type(T_TIME_SERIES), pointer :: pPrevious
    type(T_TIME_SERIES), pointer :: pNext
    logical (kind=T_LOGICAL) :: lMatch
    integer (kind=T_INT) :: iStat
    integer (kind=T_INT) :: i

    lMatch = lFALSE

    if(associated(this%pTS_Head)) then

      ! start at head of linked list
      pCurrent => this%pTS_Head
      do

        if(.not. associated(pCurrent) ) then
          exit

        elseif(str_compare(pCurrent%sSeriesName, sSeriesName) ) then

          ! reset pointers away/around object to be deleted
          pPrevious => pCurrent%pPrevious
          if(associated(pPrevious)) then
            pPrevious%pNext => pCurrent%pNext
          else
            ! if the pPrevious pointer is null, it means we're at the
            ! head of the list; need to redefine the head of the list
            ! within the TS data structure
            this%pTS_Head => pCurrent%pNext
          endif

          pNext => pCurrent%pNext
          if(associated(pNext)) pNext%pPrevious => pCurrent%pPrevious
          deallocate(pCurrent%tData, stat=iStat)
          call Assert(iStat==0, "Problem deallocating memory while removing a time series", &
            trim(__FILE__), __LINE__)
          deallocate(pCurrent, stat=iStat)
          call Assert(iStat==0, "Problem deallocating memory while removing a time series", &
            trim(__FILE__), __LINE__)

          lMatch = lTRUE
          this%iNumTimeSeries = this%iNumTimeSeries - 1

          call echolog("Removed series "//quote(sSeriesName)//". " &
            //trim(asChar(this%iNumTimeSeries))//" series remaining.")

          exit
        else
          pCurrent => pCurrent%pNext
        endif
      enddo

      call warn(lMatch, "Unable to find series "//quote(sSeriesName)//" for removal", &
        trim(__FILE__), __LINE__)
    else
      call warn(lFALSE,"There are no series objects in memory. Unable to remove series " &
        //quote(sSeriesName), trim(__FILE__), __LINE__)
    endif


!    pCurrent => this%pTS_Head
!
!      if(associated(pCurrent)) then
!        do
!          if(.not. associated(pCurrent%pNext) ) then
!            exit
!          else
!            pCurrent => pCurrent%pNext
!          endif
!        enddo
!      endif

  end subroutine remove_ts_link_sub

!------------------------------------------------------------------------------

  subroutine remove_table_link_sub(this, sSeriesName)

    class(TIME_SERIES_COLLECTION) :: this
    character (len=*) :: sSeriesName

    ! [ LOCALS ]
    type(T_TABLE), pointer :: pCurrent
    type(T_TABLE), pointer :: pPrevious
    type(T_TABLE), pointer :: pNext
    logical (kind=T_LOGICAL) :: lMatch
    integer (kind=T_INT) :: iStat
    integer (kind=T_INT) :: i

    lMatch = lFALSE

    if(associated(this%pTable_Head)) then

      ! start at head of linked list
      pCurrent => this%pTable_Head
      do

        if(.not. associated(pCurrent) ) then
          exit

        elseif(str_compare(pCurrent%sSeriesName, sSeriesName) ) then

          ! reset pointers away/around object to be deleted
          pPrevious => pCurrent%pPrevious
          if(associated(pPrevious)) then
            pPrevious%pNext => pCurrent%pNext
          else
            ! if the pPrevious pointer is null, it means we're at the
            ! head of the list; need to redefine the head of the list
            ! within the TS data structure
            this%pTable_Head => pCurrent%pNext
          endif

          pNext => pCurrent%pNext
          if(associated(pNext)) pNext%pPrevious => pCurrent%pPrevious
          deallocate(pCurrent%tTableData, stat=iStat)
          call Assert(iStat==0, "Problem deallocating memory while removing a table", &
            trim(__FILE__), __LINE__)
          deallocate(pCurrent, stat=iStat)
          call Assert(iStat==0, "Problem deallocating memory while removing a table", &
            trim(__FILE__), __LINE__)

          lMatch = lTRUE
          this%iNumTables = this%iNumTables - 1

          call echolog("Removed table "//quote(sSeriesName)//". " &
            //trim(asChar(this%iNumTables))//" table remaining.")

          exit
        else
          pCurrent => pCurrent%pNext
        endif
      enddo

      call warn(lMatch, "Unable to find series "//quote(sSeriesName)//" for removal", &
        trim(__FILE__), __LINE__)
    else
      call warn(lFALSE,"There are no series objects in memory. Unable to remove series " &
        //quote(sSeriesName), trim(__FILE__), __LINE__)
    endif

  end subroutine remove_table_link_sub

!------------------------------------------------------------------------------

  subroutine remove_ts_sub(this, sSeriesName)

    ! remove tTS from a COLLECTION of time series objects (this)

    class(TIME_SERIES_COLLECTION) :: this
    character (len=*) :: sSeriesName

    ! [ LOCALS ]
    type(T_TIME_SERIES), dimension(:), pointer :: pTempTS
    integer (kind=T_INT) :: iCount
    integer (kind=T_INT) :: iStat
    integer (kind=T_INT) :: i, j
    logical (kind=T_LOGICAL) :: lMatch

    if(associated(this%pTS)) then

      ! get the number of time series currently in time series container
      iCount = size(this%pTS)

!      print *, "iCount: ", iCount

      ! check to see whether a time series with this name exists
      lMatch = lFALSE
      do i = 1,iCount
        if(str_compare(this%pTS(i)%sSeriesName, sSeriesName)) then
          lMatch = lTRUE
          exit
        endif
      end do

      call warn(lMatch, "No time series with the name "//quote(sSeriesName)// &
         " could be found")
      if(.not. lMatch) return

      ! allocate memory for size of TS collection MINUS one time series
      allocate(pTempTS(iCount-1),stat=iStat)
      call Assert(iStat==0, "Unable to allocate temporary memory for time series", &
        TRIM(__FILE__), __LINE__)

      j = 0
      ! make a copy of objects we wish to keep
      do i = 1,iCount
!        print *, "#:",i
        if(str_compare(this%pTS(i)%sSeriesName, sSeriesName)) cycle
        j = j + 1
        pTempTS(j) = this%pTS(i)
!        print *, i, "keeping "//trim(this%pTS(i)%sSeriesname)
      enddo

      ! deallocate TS objects collection
      do i=1,size(this%pTS)
        deallocate( this%pTS(i)%tData, stat=iStat )
      enddo
      call Assert(iStat==0, "Unable to deallocate memory for time series data", &
        TRIM(__FILE__), __LINE__)

      deallocate(this%pTS, stat=iStat)
      call Assert(iStat==0, "Unable to deallocate memory for time series", &
        TRIM(__FILE__), __LINE__)


      ! allocate TS objects collection to include room for new object
      allocate(this%pTS(iCount - 1), stat=iStat)
      call Assert(iStat==0, "Unable to allocate memory for time series", &
        TRIM(__FILE__), __LINE__)

      this%pTS = pTempTS
      this%iNumTimeSeries = this%iNumTimeSeries - 1


!      deallocate(this%pTS, stat=iStat)
!      call Assert(iStat==0, "Unable to deallocate memory for time series", &
!        TRIM(__FILE__), __LINE__)

!      this%pTS => pTempTS

      call writelog("  <deleted time series "//quote(sSeriesName)//">")

    else

      call warn(lFALSE, "There are no time series objects available for deletion")

    endif

    deallocate(pTempTS, stat=iStat)
    call Assert(iStat==0, "Unable to deallocate memory for time series", &
        TRIM(__FILE__), __LINE__)

  end subroutine remove_ts_sub

!------------------------------------------------------------------------------

  subroutine remove_table_sub(this, sSeriesName)

    ! remove tTable from a COLLECTION of time series objects (this)

    class(TIME_SERIES_COLLECTION) :: this
    character (len=*) :: sSeriesName

    ! [ LOCALS ]
    type(T_TABLE), dimension(:), allocatable :: pTempTable
    integer (kind=T_INT) :: iCount
    integer (kind=T_INT) :: iStat
    integer (kind=T_INT) :: i, j
    logical (kind=T_LOGICAL) :: lMatch

    if(allocated(this%tTable)) then

      ! get the number of time series currently in time series container
      iCount = size(this%tTable)

      ! check to see whether this name exists in the collection
      lMatch = lFALSE
      do i = 1,iCount
        if(str_compare(this%tTable(i)%sSeriesName, sSeriesName)) then
          lMatch = lTRUE
          exit
        endif
      end do

      call warn(lMatch, "No table with the name "//quote(sSeriesName)// &
         " could be found")
      if(.not. lMatch) return

      if(iCount > 1) then
        ! allocate memory for size of TABLE collection MINUS one table
        allocate(pTempTable(iCount-1),stat=iStat)
        call Assert(iStat==0, "Unable to allocate temporary memory for table", &
          TRIM(__FILE__), __LINE__)

        j = 0
        ! make a copy of objects we wish to keep
        do i = 1,iCount
          if(str_compare(this%tTable(i)%sSeriesName, sSeriesName)) cycle
          j = j + 1
          pTempTable(j) = this%tTable(i)
        enddo

        ! deallocate Table objects collection
        deallocate(this%tTable, stat=iStat)
        call Assert(iStat==0, "Unable to deallocate memory for time series", &
          TRIM(__FILE__), __LINE__)

        ! allocate Table objects collection to include room for new object
        allocate(this%tTable(iCount - 1), stat=iStat)
        call Assert(iStat==0, "Unable to allocate memory for table collection", &
          TRIM(__FILE__), __LINE__)

        this%tTable = pTempTable
        this%iNumTables = this%iNumTables - 1

      else ! only one table exists; delete it...nothing else to copy

        ! deallocate Table objects collection
        deallocate(this%tTable, stat=iStat)
        call Assert(iStat==0, "Unable to deallocate memory for time series", &
          TRIM(__FILE__), __LINE__)

      endif

      call writelog("  <deleted table "//quote(sSeriesName)//">")

    else

      call warn(lFALSE, "There are no table objects available for deletion")

    endif

    if(allocated(pTempTable)) deallocate(pTempTable, stat=iStat)
    call Assert(iStat==0, "Unable to deallocate memory for temporary table objects", &
        TRIM(__FILE__), __LINE__)

  end subroutine remove_table_sub

!------------------------------------------------------------------------------

  subroutine remove_all_sub(this)

    class (TIME_SERIES_COLLECTION) :: this

    ! [ LOCALS ]
    integer (kind=T_INT) :: iSize     ! holds size of subobject arrays
    integer (kind=T_INT) :: iDataSize ! holds size of associated data structures
    integer (kind=T_INT) :: i         ! loop counter

    if(associated(this%pTS)) then
      iSize = size(this%pTS)
      do i=1,iSize
        if(allocated(this%pTS(i)%tData)) deallocate(this%pTS(i)%tData)
      enddo
      deallocate(this%pTS)
    endif

    if(allocated(this%tTable)) then
      iSize = size(this%tTable)
      do i=1,iSize
        if(allocated(this%tTable(i)%tTableData)) deallocate(this%tTable(i)%tTableData)
      enddo
      deallocate(this%tTable)
    endif

  end subroutine remove_all_sub

!------------------------------------------------------------------------------

  function get_ts_pointer_fn(this, sSeriesName)    result( pTS )

    ! get values associated with tTS from a COLLECTION of time series objects (this)
    class(TIME_SERIES_COLLECTION) :: this
    character (len=*) :: sSeriesName
    type(T_TIME_SERIES), pointer :: pTS

    ! [ LOCALS ]
    integer (kind=T_INT) :: iCount
    integer (kind=T_INT) :: iStat
    integer (kind=T_INT) :: i, j
    logical (kind=T_LOGICAL) :: lMatch

    if(associated(this%pTS)) then

      ! get the number of time series currently in time series container
      iCount = size(this%pTS)

      ! scan to find a series with this name
      lMatch = lFALSE
      do i = 1,iCount
        if(str_compare(this%pTS(i)%sSeriesName, sSeriesName)) then
          lMatch = lTRUE
          exit
        endif
      end do

      call Assert(lMatch, "No time series with the name "//quote(sSeriesName)// &
         " could be found",trim(__FILE__),__LINE__)

      pTS => this%pTS(i)

    endif

  end function get_ts_pointer_fn

!------------------------------------------------------------------------------

  function get_ts_pointer_link_fn(this, sSeriesName)    result( pTS )

    ! get values associated with tTS from a COLLECTION of time series objects (this)
    class(TIME_SERIES_COLLECTION) :: this
    character (len=*) :: sSeriesName
    type(T_TIME_SERIES), pointer :: pTS

    ! [ LOCALS ]
    integer (kind=T_INT) :: iCount
    integer (kind=T_INT) :: iStat
    integer (kind=T_INT) :: i, j
    logical (kind=T_LOGICAL) :: lMatch
    type(T_TIME_SERIES), pointer :: pCurrent => null()

    lMatch = lFALSE

    if(associated(this%pTS_Head)) then

      ! start at head of linked list
      pCurrent => this%pTS_Head
      do
        if(.not. associated(pCurrent) ) exit

        if(str_compare(pCurrent%sSeriesName, sSeriesName) ) then
          lMatch = lTRUE
          pTS => pCurrent
          exit
        elseif(.not. associated(pCurrent%pNext) ) then
          exit
        else
          pCurrent => pCurrent%pNext
        endif
      enddo

      if(.not. lMatch) then
        call this%summarize()
        call assert(lFALSE,"Unable to find series "//quote(sSeriesName)//".", &
        trim(__FILE__), __LINE__)
      endif
!      call assert(lMatch, "Unable to find series "//quote(sSeriesName)//".", &
!        trim(__FILE__), __LINE__)
    else
      call warn(lFALSE,"There are no series objects in memory. Unable to obtain pointer to series " &
        //quote(sSeriesName), trim(__FILE__), __LINE__)

      pTS => null()

    endif

    nullify(pCurrent)

  end function get_ts_pointer_link_fn

!------------------------------------------------------------------------------

  function get_ts_pointer_node_fn(this, sSeriesName)    result( pTS )

    class(TIME_SERIES_COLLECTION) :: this
    character (len=*) :: sSeriesName
    type(T_TIME_SERIES), pointer :: pTS

    ! [ LOCALS ]
    type (T_NODE), pointer :: pNode

    pNode => this%getNode(sSeriesName)

    if(associated(pNode%pTS) ) then
      pTS => pNode%pTS
    else
       call assert(lFALSE, "Failed to find the series named "//quote(sSeriesName), &
        trim(__FILE__), __LINE__)
    endif

    nullify(pNode)

  end function get_ts_pointer_node_fn

!------------------------------------------------------------------------------

  function get_table_pointer_link_fn(this, sSeriesName)    result( pTable )

    ! get a pointer to a table from a COLLECTION of time series objects (this)
    class(TIME_SERIES_COLLECTION) :: this
    character (len=*) :: sSeriesName
    type(T_TABLE), pointer :: pTable

    ! [ LOCALS ]
    integer (kind=T_INT) :: iCount
    integer (kind=T_INT) :: iStat
    integer (kind=T_INT) :: i, j
    logical (kind=T_LOGICAL) :: lMatch
    type(T_TABLE), pointer :: pCurrent => null()

    lMatch = lFALSE

    if(associated(this%pTable_Head)) then

      ! start at head of linked list
      pCurrent => this%pTable_Head
      do
        if(.not. associated(pCurrent) ) exit

        if(str_compare(pCurrent%sSeriesName, sSeriesName) ) then
          lMatch = lTRUE
          pTable => pCurrent
          exit
        elseif(.not. associated(pCurrent%pNext) ) then
          exit
        else
          pCurrent => pCurrent%pNext
        endif
      enddo

      call warn(lMatch, "Unable to find table "//quote(sSeriesName)//".", &
        trim(__FILE__), __LINE__)
    else
      call warn(lFALSE,"There are no table objects in memory. Unable to obtain pointer to table " &
        //quote(sSeriesName), trim(__FILE__), __LINE__)

      pTable => null()

    endif

    nullify(pCurrent)

  end function get_table_pointer_link_fn

!------------------------------------------------------------------------------

  function get_table_pointer_node_fn(this, sSeriesName)    result( pTable )

    class(TIME_SERIES_COLLECTION) :: this
    character (len=*) :: sSeriesName
    type(T_TABLE), pointer :: pTable

    ! [ LOCALS ]
    type (T_NODE), pointer :: pNode

    pNode => this%getNode(sSeriesName)

    if(associated(pNode%pTable) ) then
      pTable => pNode%pTable
    else
      call assert(lFALSE, "Failed to find the table named "//quote(sSeriesName), &
        trim(__FILE__), __LINE__)
    endif

  end function get_table_pointer_node_fn

!------------------------------------------------------------------------------

  function get_ts_comparison_pointer_fn(this, sObservedSeries, sModeledSeries)    result( tTSComparison )

    ! get values associated with tTS from a COLLECTION of time series objects (this)
    class(TIME_SERIES_COLLECTION) :: this
    character (len=*) :: sObservedSeries
    character (len=*) :: sModeledSeries
    type(T_TS_COMPARISON), pointer :: tTSComparison

    ! [ LOCALS ]
    integer (kind=T_INT) :: iCount
    integer (kind=T_INT) :: iStat
    integer (kind=T_INT) :: i, j
    logical (kind=T_LOGICAL) :: lMatch

    if(allocated(this%tTSComparison)) then

      ! get the number of time series currently in time series comparison container
      iCount = size(this%tTSComparison)

      ! check to see whether these series have been compared already
      lMatch = lFALSE
      do i = 1,iCount
        if(str_compare(this%tTSComparison(i)%sObservedSeries, sObservedSeries) &
            .and. str_compare(this%tTSComparison(i)%sModeledSeries, sModeledSeries) ) then
          lMatch = lTRUE
          exit
        endif
      end do

      call Assert(lMatch, "No time series comparison between the series "//quote(sObservedSeries)// &
         " and "//quote(sModeledSeries)//" could be found",trim(__FILE__),__LINE__)

!      tTSComparison => this%tTSComparison(i)
      tTSComparison => null()

    endif

  end function get_ts_comparison_pointer_fn

!------------------------------------------------------------------------------

  function get_table_comparison_pointer_fn(this, sObservedTable, sModeledTable) &
                                                            result( pTableComparison )

    ! get values associated with tTS from a COLLECTION of time series objects (this)
    class(TIME_SERIES_COLLECTION) :: this
    character (len=*) :: sObservedTable
    character (len=*) :: sModeledTable
    type(T_TABLE_COMPARISON), pointer :: pTableComparison

    ! [ LOCALS ]
    integer (kind=T_INT) :: iCount
    integer (kind=T_INT) :: iStat
    integer (kind=T_INT) :: i, j
    logical (kind=T_LOGICAL) :: lMatch

    if(allocated(this%tTableComparison)) then

      ! get the number of tables currently in table comparison container
      iCount = size(this%tTableComparison)

      ! check to see whether these tables have been compared already
      lMatch = lFALSE
      do i = 1,iCount
        if(str_compare(this%tTableComparison(i)%sObservedTable, sObservedTable) &
            .and. str_compare(this%tTableComparison(i)%sModeledTable, sModeledTable) ) then
          lMatch = lTRUE
          exit
        endif
      end do

      call Assert(lMatch, "No table comparisons between the series "//quote(sObservedTable)// &
         " and "//quote(sModeledTable)//" could be found",trim(__FILE__),__LINE__)

!      pTableComparison => this%tTableComparison(i)
      pTableComparison => null()

    endif

  end function get_table_comparison_pointer_fn

!------------------------------------------------------------------------------

  function get_table_pointer_fn(this, sSeriesName)    result( pTable )

    ! get values associated with tTable from a COLLECTION of table objects (this)
    class(TIME_SERIES_COLLECTION) :: this
    character (len=*) :: sSeriesName
    type(T_TABLE), pointer :: pTable

    ! [ LOCALS ]
    integer (kind=T_INT) :: iCount
    integer (kind=T_INT) :: iStat
    integer (kind=T_INT) :: i, j
    logical (kind=T_LOGICAL) :: lMatch

    if(allocated(this%tTable)) then

      ! get the number of table entries currently in the table collection
      iCount = size(this%tTable)

      ! find the matching table entry
      lMatch = lFALSE
      do i = 1,iCount
        if(str_compare(this%tTable(i)%sSeriesName, sSeriesName)) then
          lMatch = lTRUE
          exit
        endif
      end do

      call Assert(lMatch, "No table with the name "//quote(sSeriesName)// &
         " could be found",trim(__FILE__),__LINE__)

!      pTable => this%tTable(i)
      pTable => null()

    endif

  end function get_table_pointer_fn

!------------------------------------------------------------------------------

  subroutine summarize_sub(this)

    class(TIME_SERIES_COLLECTION) :: this

    ! [ LOCALS ]
    integer (kind=T_INT) :: iCount
    integer (kind=T_INT) :: i, j
    integer (kind=T_INT) :: iMemoryInBytes
    integer (kind=T_INT) :: iSum
    real (kind=T_SGL) :: rSSE
    type (T_TIME_SERIES), pointer :: pObservedSeries => null()
    type (T_TIME_SERIES), pointer :: pModeledSeries => null()
    type(T_TIME_SERIES), pointer :: pCurrentTS => null()
    type (T_NODE), pointer :: pCurrent => null()
    type (T_TABLE), pointer :: pObservedTable => null()
    type (T_TABLE), pointer :: pModeledTable => null()
    type (T_TABLE), pointer :: pCurrentTable => null()

    write(LU_STD_OUT,fmt="(/,/,a,/)") '*** TSPROC TIME SERIES and TABLE OBJECTS CURRENTLY IN MEMORY ***'

    if(this%iNumTimeSeries > 0) then
      pCurrent => this%pRoot
      write(LU_STD_OUT, &
        fmt="('SERIES_NAME',t24,'DATE RANGE',t45,'COUNT',t58,'MIN',t68,'MEAN', &
             t80,'MAX',t86,'MEMORY(kb)',/)")
      call this%summarizeSeries(pCurrent)
    else
      write(LU_STD_OUT,fmt="(a,/)") '     ===> No TIME SERIES objects currently in memory <==='
    endif

!      do
!
!        if(allocated(pCurrentTS%tData)) then
!          iMemoryInBytes = sizeof(pCurrentTS) + sizeof(pCurrentTS%tData)
!          write(LU_STD_OUT,fmt="(a18,a10,'-',a10,i10, f11.2,f11.2,f11.2,3x,i6,'k')") &
!            pCurrentTS%sSeriesName, &
!            pCurrentTS%tStartDate%prettyDate(), &
!            pCurrentTS%tEndDate%prettyDate(), &
!            size(pCurrentTS%tData), &
!            MINVAL(pCurrentTS%tData%rValue), &
!            SUM(pCurrentTS%tData%rValue)/ size(pCurrentTS%tData), &
!            MAXVAL(pCurrentTS%tData%rValue), &
!            iMemoryInBytes / 1024
!            iSum = iSum + size(pCurrentTS%tData)
!        else
!          iMemoryInBytes = sizeof(pCurrentTS)
!
!          write(LU_STD_OUT,fmt="(a,t20,51x,i6,'k')") &
!            pCurrentTS%sSeriesName, iMemoryInBytes / 1024
!
!        endif

!        ! recurse through list until we come to the end of the line
!        if(associated(pCurrentTS%pNext) ) then
!          pCurrentTS => pCurrentTS%pNext
!          cycle
!        else
!          exit
!        endif
!      enddo


!     if(.not. associated(this%pTS)) then
!       write(LU_STD_OUT,fmt="(a,/)") '     ===> No TIME SERIES objects currently in memory <==='
!
!     else
!
!       iCount = size(this%pTS)
!
!       iSum = 0
!       do i=1,iCount
!
!         if(allocated(this%pTS(i)%tData)) then
!           iMemoryInBytes = sizeof(this%pTS(i)) + sizeof(this%pTS(i)%tData)
!           write(LU_STD_OUT,fmt="(a18,a10,'-',a10,i10, f11.2,f11.2,f11.2,3x,i6,'k')") &
!             this%pTS(i)%sSeriesName, &
!             this%pTS(i)%tStartDate%prettyDate(), &
!             this%pTS(i)%tEndDate%prettyDate(), &
!             size(this%pTS(i)%tData), &
!             MINVAL(this%pTS(i)%tData%rValue), &
!             SUM(this%pTS(i)%tData%rValue)/ size(this%pTS(i)%tData), &
!             MAXVAL(this%pTS(i)%tData%rValue), &
!             iMemoryInBytes / 1024
!             iSum = iSum + size(this%pTS(i)%tData)
!         else
!           iMemoryInBytes = sizeof(this%pTS(i))
!
!           write(LU_STD_OUT,fmt="(a,t20,51x,i6,'k')") &
!             this%pTS(i)%sSeriesName, iMemoryInBytes / 1024
!
!         endif
!       end do
!
!       write(LU_STD_OUT, fmt="(/,a,/)") trim(asChar(iSum))//" total records in memory."
!
!     endif

!     if(.not. allocated(this%tTable)) then
!       write(LU_STD_OUT,fmt="(/,a,/)") '     ===> No TABLE objects currently in memory <==='
!
!     else
!
!       iCount = size(this%tTable)
!
!       write(LU_STD_OUT, &
!         fmt="(/,'SERIES_NAME',t24,'DATE RANQE',t45,'TABLE TYQE',/)")
!
!
!       do i=1,iCount
!
!         if(allocated(this%tTable(i)%tTableData)) then
!           iMemoryInBytes = sizeof(this%tTable(i)) + sizeof(this%tTable(i)%tTableData)
!           write(LU_STD_OUT,fmt="(a18,a10,'-',a10, t47, a)") &
!             this%tTable(i)%sSeriesName, &
!             this%tTable(i)%tStartDate%prettyDate(), &
!             this%tTable(i)%tEndDate%prettyDate(), &
!             TABLE_TYPE(this%tTable(i)%iTableType)
!
!         else
!
!
!         endif
!       end do
!     endif

    if(this%iNumTables > 0) then
      pCurrent => this%pRoot
      write(LU_STD_OUT, &
        fmt="(/,'SERIES_NAME',t24,'DATE RANGE',t45,'TABLE TYPE',/)")
      call this%summarizeTable(pCurrent)
    else
      write(LU_STD_OUT,fmt="(/,a,/)") '     ===> No TABLE objects currently in memory <==='
    endif

!
!
!     if(.not. associated(this%pTable_Head)) then
!       write(LU_STD_OUT,fmt="(/,a,/)") '     ===> No TABLE objects currently in memory <==='
!
!     else
!
!
!       write(LU_STD_OUT, &
!         fmt="(/,'SERIES_NAME',t24,'DATE RANGE',t45,'TABLE TYPE',/)")
!
!
!       pCurrentTable => this%pTable_Head
!
!       do
!
!         if(allocated(pCurrentTable%tTableData)) then
!           iMemoryInBytes = sizeof(pCurrentTable) + sizeof(pCurrentTable%tTableData)
!           write(LU_STD_OUT,fmt="(a18,a10,'-',a10, t47, a)") &
!             pCurrentTable%sSeriesName, &
!             pCurrentTable%tStartDate%prettyDate(), &
!             pCurrentTable%tEndDate%prettyDate(), &
!             TABLE_TYPE(pCurrentTable%iTableType)
!
!         else
!           write(LU_STD_OUT,fmt="(a18,a10,'-',a10, t47, a)") &
!             pCurrentTable%sSeriesName, '***', '***', '***'
!
!         endif
!
!         ! recurse through list until we come to the end of the line
!         if(associated(pCurrentTable%pNext) ) then
!           pCurrentTable => pCurrentTable%pNext
!           cycle
!         else
!           exit
!         endif
!
!       end do
!     endif


    write(LU_STD_OUT,fmt="(/,/,a,/)") '*** TSPROC TIME SERIES and TABLE COMPARISON OBJECTS CURRENTLY IN MEMORY ***'
    write(LU_STD_OUT, &
      fmt="(/,'OBSERVED SERIES',t19,'MODELED SERIES',t38,'DATE RANGE',t60,'COUNT'," &
         //"t76,'RESIDUAL',/)")

    if(.not. allocated(this%tTSComparison)) then
      write(LU_STD_OUT,fmt="(a)") &
        '     ===> No TIME SERIES COMPARISON objects currently in memory <==='

    else

      iCount = size(this%tTSComparison)

      do i=1,iCount

        pObservedSeries => this%getSeries(this%tTSComparison(i)%sObservedSeries)
        pModeledSeries => this%getSeries(this%tTSComparison(i)%sModeledSeries)
        rSSE = 0.

        do j=1,size(pObservedSeries%tData%rValue)

          rSSE = rSSE + ((pObservedSeries%tData(j)%rValue &
                 - pModeledSeries%tData(j)%rValue ) **2 &
                 * this%tTSComparison(i)%rWeightValue(j)**2)

        enddo

        write(LU_STD_OUT,fmt="(a18,1x,a18,1x,a10,'-',a10, i10, g16.8)") &
          this%tTSComparison(i)%sObservedSeries, &
          this%tTSComparison(i)%sModeledSeries, &
          pObservedSeries%tStartDate%prettyDate(), &
          pObservedSeries%tEndDate%prettyDate(), &
          size(pObservedSeries%tData), rSSE

      end do
    endif

    write(LU_STD_OUT, &
      fmt="(/,'OBSERVED TABLE',t19,'MODELED TABLE',t38,'DATE RANGE',t60,'COUNT'," &
         //"t76,'RESIDUAL',/)")
    if(.not. allocated(this%tTableComparison)) then
      write(LU_STD_OUT,fmt="(a,/)") &
        '     ===> No TABLE COMPARISON objects currently in memory <==='

    else

      iCount = size(this%tTableComparison)

      do i=1,iCount

        pObservedTable => this%getTable(this%tTableComparison(i)%sObservedTable)
        pModeledTable => this%getTable(this%tTableComparison(i)%sModeledTable)
        rSSE = 0.

        if(pObservedTable%iTableType == iETABLE) then

          ! calculate SSE using the third element of "sValue" (i.e. fraction of days exceeded)
          do j=1,size(pObservedTable%tTableData)
            rSSE = rSSE + (( asReal(pObservedTable%tTableData(j)%sValue(3)) &
                   - asReal(pModeledTable%tTableData(j)%sValue(3)) ) **2 &
                   * this%tTableComparison(i)%rWeightValue(j)**2)
          enddo

        elseif(pObservedTable%iTableType == iSTABLE) then

          ! ignore the first 8 entries of the S table in calculating SSE
          do j=9,size(pObservedTable%tTableData)
            rSSE = rSSE + (( asReal(pObservedTable%tTableData(j)%sValue(1)) &
                   - asReal(pModeledTable%tTableData(j)%sValue(1)) ) **2 &
                   * this%tTableComparison(i)%rWeightValue(j)**2)
          enddo

        else

          do j=1,size(pObservedTable%tTableData)
            rSSE = rSSE + (( asReal(pObservedTable%tTableData(j)%sValue(1)) &
                   - asReal(pModeledTable%tTableData(j)%sValue(1)) ) **2 &
                   * this%tTableComparison(i)%rWeightValue(j)**2)
          enddo

        endif

        write(LU_STD_OUT,fmt="(a18,1x,a18,1x,a10,'-',a10, i10, g16.8)") &
          this%tTableComparison(i)%sObservedTable, &
          this%tTableComparison(i)%sModeledTable, &
          pObservedTable%tStartDate%prettyDate(), &
          pObservedTable%tEndDate%prettyDate(), &
          size(pObservedTable%tTableData), rSSE

      end do
    endif


  end subroutine summarize_sub

!------------------------------------------------------------------------------

  subroutine describe_ts_sub(this,sSeriesName)

    class(TIME_SERIES_COLLECTION) :: this
    character(len=*), intent(in) :: sSeriesName

    ! [ LOCALS ]
    type(T_TIME_SERIES), pointer :: pTS
    integer (kind=T_INT) :: i
    integer (kind=T_INT) :: iMemoryInBytes

    pTS => this%getSeries(sSeriesName)

    write(LU_STD_OUT,fmt="(/,a,/)") trim(pTS%sDescription)

    if(allocated(pTS%tData)) then
    write(LU_STD_OUT, &
      fmt="('SERIES_NAME',t24,'DATE RANGE',t45,'COUNT',t58,'MIN',t68,'MEAN', &
           t80,'MAX',t86,'MEMORY(kb)')")
      iMemoryInBytes = sizeof(pTS) + sizeof(pTS%tData)
      write(LU_STD_OUT,fmt="(a18,a10,'-',a10,i10, f11.2,f11.2,f11.2,3x,i6,'k',/)") &
        pTS%sSeriesName, &
        pTS%tStartDate%prettyDate(), &
        pTS%tEndDate%prettyDate(), &
        count(pTS%tData%lValid), &
        MINVAL(pTS%tData%rValue,pTS%tData%lValid), &
        SUM(pTS%tData%rValue,pTS%tData%lValid)/ count(pTS%tData%lValid), &
        MAXVAL(pTS%tData%rValue,count(pTS%tData%lValid)), &
        iMemoryInBytes / 1024

    else
      iMemoryInBytes = sizeof(pTS)
       write(LU_STD_OUT,fmt="(a,t20,51x,i6,'k')") &
        pTS%sSeriesName, iMemoryInBytes / 1024

    endif


  end subroutine describe_ts_sub

!------------------------------------------------------------------------------
  subroutine pest_write_ts_comparison_sub(this, sObservedSeries, sModeledSeries, iLU)

    class(TIME_SERIES_COLLECTION) :: this
    character (len=*) :: sObservedSeries
    character (len=*) :: sModeledSeries
    integer (kind=T_INT), optional :: iLU

    ! [ LOCALS ]
    type(T_TS_COMPARISON), pointer :: tTSComparison
    type(T_TIME_SERIES), pointer :: pObservedSeries
    integer (kind=T_INT) :: LU
    character (len=256) :: sFormatString
    integer (kind=T_INT) :: i

    if(present(iLU)) then
      LU = iLU
    else
      LU = LU_STD_OUT
    endif

    tTSComparison => this%getSeriesComparison(sObservedSeries, sModeledSeries)
    pObservedSeries => this%getSeries(sObservedSeries)

    sFormatString = "(1x,a"//trim(asChar(MAXNAMELENGTH) )//",3x,g16.8,3x,g16.8,3x,a)"

    ! add observation numbers to the end of the observation name
    do i=1,size(pObservedSeries%tData)

       write(LU,fmt=trim(sFormatString)) &
          trim(pObservedSeries%sSeriesName)//"_"//trim(asChar(i) ), &
          pObservedSeries%tData(i)%rValue, tTSComparison%rWeightValue(i), &
          tTSComparison%sObservedSeries

    enddo

    nullify(tTSComparison, pObservedSeries)

  end subroutine pest_write_ts_comparison_sub

!------------------------------------------------------------------------------

  subroutine list_output_ts_sub(this, sSeriesName, iLU)

    class(TIME_SERIES_COLLECTION) :: this
    character(len=*), intent(in) :: sSeriesName
!    character(len=*), intent(in), optional :: sDateFormat
    integer (kind=T_INT), intent(in), optional :: iLU

    ! [ LOCALS ]
    type(T_TIME_SERIES), pointer :: pTS
    integer (kind=T_INT) :: i
    integer (kind=T_INT) :: LU
!    character(len=20) :: sDateFmt

    if(present(iLU)) then
      LU = iLU
    else
      LU = LU_STD_OUT
    endif

!    if(present(sDateFormat)) then
!      sDateFmt = trim(sDateFormat)
!    else
!      sDateFmt = "MM/DD/YYYY"
!    endif

    pTS => this%getSeries(sSeriesName)

    ! now that we have a pointer to a specific time series, call the
    ! type-bound method for that time series to do the actual output
    call pTS%list(LU)

  end subroutine list_output_ts_sub

!------------------------------------------------------------------------------

  subroutine list_output_table_sub(this, sSeriesName, sDateFormat, iLU)

    class(TIME_SERIES_COLLECTION) :: this
    character(len=*), intent(in) :: sSeriesName
    character(len=*), intent(in), optional :: sDateFormat
    integer (kind=T_INT), intent(in), optional :: iLU

    ! [ LOCALS ]
    type(T_TABLE), pointer :: pTable
    integer (kind=T_INT) :: i
    integer (kind=T_INT) :: LU
    character(len=20) :: sDateFmt

    if(present(iLU)) then
      LU = iLU
    else
      LU = LU_STD_OUT
    endif

    if(present(sDateFormat)) then
      sDateFmt = trim(sDateFormat)
    else
      sDateFmt = "MM/DD/YYYY"
    endif

    pTable => this%getTable(sSeriesName)

    call pTable%list(sDateFormat, iLU)

    nullify(pTable)

  end subroutine list_output_table_sub


!------------------------------------------------------------------------------

  subroutine pest_write_table_comparison_sub(this, sObservedSeries, sModeledSeries, iLU)

    class(TIME_SERIES_COLLECTION) :: this
    character (len=*) :: sObservedSeries
    character (len=*) :: sModeledSeries
    integer (kind=T_INT), optional :: iLU

    ! [ LOCALS ]
    type(T_TABLE_COMPARISON), pointer :: pTableComparison
    type(T_TABLE), pointer :: pTable
    integer (kind=T_INT) :: LU
    character (len=256) :: sFormatString
    integer (kind=T_INT) :: i

    if(present(iLU)) then
      LU = iLU
    else
      LU = LU_STD_OUT
    endif

    pTableComparison => this%getTableComparison(sObservedSeries, sModeledSeries)
    pTable => this%getTable(sObservedSeries)

    sFormatString = "(1x,a"//trim(asChar(MAXNAMELENGTH) )//",3x,a20,3x,g16.8,3x,a)"

    ! add observation numbers to the end of the observation name
    do i=1,size(pTable%tTableData)

       write(LU,fmt=trim(sFormatString)) &
          trim(pTable%sSeriesName)//"_"//trim(asChar(i) ), &
          pTable%tTableData(i)%sValue, pTableComparison%rWeightValue(i), &
          trim(pTable%sSeriesName)

    enddo

    nullify(pTableComparison, pTable)

  end subroutine pest_write_table_comparison_sub

!------------------------------------------------------------------------------

  subroutine conform_ts_sub(this, sSeriesname, sTimeBaseName, sNewSeriesName)

    class(TIME_SERIES_COLLECTION) :: this
    character(len=256), intent(in) :: sSeriesName
    character(len=256), intent(in) :: sTimeBaseName
    character(len=256), intent(in), optional :: sNewSeriesName

    ! [ LOCALS ]
    type (T_TIME_SERIES), pointer :: pTS, pTimeBaseTS
    type (T_TIME_SERIES), pointer :: pNewSeries
!    type (T_TIME_SERIES), allocatable, target :: pTempSeries
    integer (kind=T_INT) :: iStat

    real (kind=T_SGL), dimension(:), allocatable :: rX1, rX2, rY1, rY2
    real (kind=T_DBL) :: rOffset

   character (len=256) :: sNewName

   call Assert(len_trim(sSeriesname) > 0, &
     "internal error: must provide 'sSeriesname'", trim(__FILE__),__LINE__)
   call Assert(len_trim(sTimeBasename) > 0, &
     "internal error: must provide 'sTimeBasename'", trim(__FILE__),__LINE__)

   ! create a new series name if one is not provided
   if(present(sNewSeriesName)) then
     sNewName = trim(sNewSeriesName)
   else
     sNewName = trim(sSeriesname)//"_TB"
   endif

    ! get pointers to the two series involved
    pTS => this%getSeries(sSeriesname)
    pTimeBaseTS => this%getSeries(sTimeBaseName)

    ! perform sanity checks on date boundaries
    if(pTS%tStartDate > pTimeBaseTS%tStartDate .or. &
      pTS%tEndDate < pTimeBaseTS%tStartDate) &
       call assert(lFALSE,"Series "//quote(sSeriesname) &
       //" "//pTS%printDateRange()//"~does not completely cover the timespan " &
       //" covered by series ~used as a timebase: "//quote(sTimebasename) &
       //" "//pTimeBaseTS%printDateRange(), trim(__FILE__), __LINE__)

!    allocate(pTempSeries, stat=iStat)
!    call Assert(iStat == 0, "Problem allocating memory for NEW_TIME_BASE calculation", &
!      trim(__FILE__), __LINE__)
!
!    allocate(pTempSeries%tData(size(pTimeBaseTS%tData)), stat=iStat)
!    call Assert(iStat == 0, "Problem allocating memory for NEW_TIME_BASE calculation", &
!      trim(__FILE__), __LINE__)

    allocate(pNewSeries, stat=iStat)
    call Assert(iStat == 0, "Problem allocating memory for NEW_TIME_BASE calculation", &
      trim(__FILE__), __LINE__)

    allocate(pNewSeries%tData(size(pTimeBaseTS%tData)), stat=iStat)
    call Assert(iStat == 0, "Problem allocating memory for NEW_TIME_BASE calculation", &
      trim(__FILE__), __LINE__)

    pNewSeries%sSeriesname = sNewName
    pNewSeries%sDescription = "Data from series "//trim(pTS%sSeriesName)//", " &
      //"interpolated to the datetimes found in series "//trim(pTimeBaseTS%sSeriesName)

    allocate(rX1(size(pTS%tData)),stat=iStat)
    call Assert(iStat == 0, "Problem allocating memory for NEW_TIME_BASE calculation", &
      trim(__FILE__), __LINE__)

    allocate(rY1(size(pTS%tData)),stat=iStat)
    call Assert(iStat == 0, "Problem allocating memory for NEW_TIME_BASE calculation", &
      trim(__FILE__), __LINE__)

    allocate(rX2(size(pTimeBaseTS%tData)),stat=iStat)
    call Assert(iStat == 0, "Problem allocating memory for NEW_TIME_BASE calculation", &
      trim(__FILE__), __LINE__)

    allocate(rY2(size(pTimeBaseTS%tData)),stat=iStat)
    call Assert(iStat == 0, "Problem allocating memory for NEW_TIME_BASE calculation", &
      trim(__FILE__), __LINE__)

    ! we're subtracting this (large) value so that we can get away with using
    ! single precision real values to represent the datetime values
    rOffset = real(MINVAL(pTS%tData%tDT%iJulianDay), kind=T_DBL)

    rX1 = real( pTS%tData%tDT%iJulianDay, kind=T_DBL ) - rOffset &
           + real(pTS%tData%tDT%rFractionOfDay, kind=T_DBL)

    rY1 = pTS%tData%rValue

    rX2 = real( pTimeBaseTS%tData%tDT%iJulianDay, kind=T_DBL ) - rOffset &
           + real(pTimeBaseTS%tData%tDT%rFractionOfDay, kind=T_DBL)


    call interp_1d( rX1, rY1, rX2, rY2)

    ! copy datetime values from the timebase series to the new series
    pNewSeries%tData%tDT = pTimeBaseTS%tData%tDT
    ! copy the interpolated values to the new series
    pNewSeries%tData%rValue = rY2
    call pNewSeries%findDateMinAndMax()

    ! add new series to collection of series

    call this%insert(pNewSeries=pNewSeries)
    deallocate(rX1, rY1, rX2, rY2)
    nullify(pTS)
    nullify(pTimeBaseTS)
    nullify(pNewSeries)

  end subroutine conform_ts_sub

!------------------------------------------------------------------------------

  function are_datetime_stamps_identical_names_fn(this, sSeriesname1, sSeriesname2)   result(lBool)

    class(TIME_SERIES_COLLECTION) :: this
    character(len=*), intent(in) :: sSeriesName1
    character(len=*), intent(in) :: sSeriesName2
    logical (kind=T_LOGICAL) :: lBool

    ! [ LOCALS ]
    type (T_TIME_SERIES), pointer :: pTS1, pTS2

    ! get pointers to the two series involved
    pTS1 => this%getSeries(sSeriesname1)
    pTS2 => this%getSeries(sSeriesName2)

    lBool = this%datesEqual(pTS1, pTS2)

    nullify(pTS1, pTS2)

  end function are_datetime_stamps_identical_names_fn

!------------------------------------------------------------------------------

  function are_datetime_stamps_identical_fn(this, pTS1, pTS2)   result(lBool)

    class(TIME_SERIES_COLLECTION) :: this
    type (T_TIME_SERIES), pointer :: pTS1
    type (T_TIME_SERIES), pointer :: pTS2
    logical (kind=T_LOGICAL) :: lBool

    ! [ LOCALS ]
    integer (kind=T_INT) :: iCount, iCount1, iCount2, i

    lBool = lTRUE

    iCount1 = size(pTS1%tData)
    iCount2 = size(pTS2%tData)

    if( iCount1 /= iCount2 ) then
       call warn(lFALSE, &
         "Series "//quote(pTS1%sSeriesname)//" does not contain the same number ~of data elements " &
         //"as "//quote(pTS2%sSeriesname))
       lBool = lFALSE
    endif

    if(pTS1%iDataType /= pTS2%iDataType) then
      call warn(lFALSE, &
        "Series "//quote(pTS1%sSeriesname)//" is of type: "//lowercase(sSERIES_TYPE(pTS1%iDataType))// &
        "~Series "//quote(pTS2%sSeriesname)//" is of type: "//lowercase(sSERIES_TYPE(pTS2%iDataType))// &
        "~A series of one type cannot be interpolated to a timebase with a differing type")
      lBool = lFALSE
    endif

    select case(pTS1%iDataType)

      case(iDAILY_SERIES)

        do i=1,iCount
          if(.not. pTS1%tData(i)%tDT == pTS2%tData(i)%tDT) then
            call warn(lFALSE, &
              "Series "//quote(pTS1%sSeriesname)//" is not equal to " &
              //quote(pTS1%sSeriesname)//"; ~datetime1: "//trim(pTS1%tData(i)%tDT%listdatetime() ) &
              //"  datetime2: "//trim(pTS2%tData(i)%tDT%listdatetime() ) )
            lBool = lFALSE
            exit
          endif
        enddo

      case(iMONTHLY_SERIES)

        do i=1,iCount
          if( pTS1%tData(i)%tDT%iMonth /= pTS2%tData(i)%tDT%iMonth &
             .or. pTS1%tData(i)%tDT%iYear /= pTS2%tData(i)%tDT%iYear) then
            call warn(lFALSE, &
              "Series "//quote(pTS1%sSeriesname)//" does not have month and year " &
              //"~values in alignment with those of "&
              //quote(pTS1%sSeriesname)//"; ~datetime1: "//trim(pTS1%tData(i)%tDT%listdatetime() ) &
              //"  datetime2: "//trim(pTS2%tData(i)%tDT%listdatetime() ) )
            lBool = lFALSE
            exit
          endif
        enddo

      case(iMONTHLY_SUMMARY)

        do i=1,iCount
          if( pTS1%tData(i)%tDT%iMonth /= pTS2%tData(i)%tDT%iMonth) then
            call warn(lFALSE, &
              "Series "//quote(pTS1%sSeriesname)//" does not have month " &
              //"~values in alignment with those of "&
              //quote(pTS1%sSeriesname)//"; ~datetime1: "//trim(pTS1%tData(i)%tDT%listdatetime() ) &
              //"  datetime2: "//trim(pTS2%tData(i)%tDT%listdatetime() ) )
            lBool = lFALSE
            exit
          endif
        enddo

      case(iANNUAL_SUMMARY)

        do i=1,iCount
          if( pTS1%tData(i)%tDT%iYear /= pTS2%tData(i)%tDT%iYear) then
            call warn(lFALSE, &
              "Series "//quote(pTS1%sSeriesname)//" does not have year " &
              //"~values in alignment with those of "&
              //quote(pTS1%sSeriesname)//"; ~datetime1: "//trim(pTS1%tData(i)%tDT%listdatetime() ) &
              //"  datetime2: "//trim(pTS2%tData(i)%tDT%listdatetime() ) )
            lBool = lFALSE
            exit
          endif
        enddo

      case default

        call assert(lFALSE, "Logic error in case select structure", &
          trim(__FILE__), __LINE__)

    end select

    if(lBool) then
      call echolog("  => Series "//quote(pTS1%sSeriesname) &
         //" has datestamps identical to those in "//quote(pTS2%sSeriesname)//".")
    endif

    nullify(pTS1, pTS2)

  end function are_datetime_stamps_identical_fn

!------------------------------------------------------------------------------

  subroutine add_ts_comparison_sub(this, sObservedSeries, sModeledSeries, sEquationText)

    class(TIME_SERIES_COLLECTION) :: this
    character(len=*), intent(in) :: sObservedSeries
    character(len=*), intent(in) :: sModeledSeries
    character(len=*), intent(in) :: sEquationText

    ! [ LOCALS ]
    type(T_TS_COMPARISON), dimension(:), allocatable :: tTemtTSComparison
    type (T_TIME_SERIES), pointer :: pObservedSeries, pModeledSeries
    integer (kind=T_INT) :: iCount
    integer (kind=T_INT) :: iCountObserved, iCountModeled
    integer (kind=T_INT) :: iStat
    integer (kind=T_INT) :: i
    integer (kind=T_INT) :: iNumDigits
    integer (kind=T_INT) :: iRecordToReplace
    logical (kind=T_LOGICAL) :: lIsNewComparison

    iCount = 0
    lIsNewComparison = lTRUE

    do
      ! get pointers to the two series involved
      pObservedSeries => this%getSeries(sObservedSeries)
      pModeledSeries => this%getSeries(sModeledSeries)

      if(.not. this%datesEqual(pObservedSeries, pModeledSeries) ) &
        call assert(lFALSE, "Cannot create comparison between " &
          //quote(sObservedSeries)//" and "//quote(sModeledSeries) &
          //"~because the date and timestamps are unequal",trim(__FILE__), __LINE__)

      iCountObserved = size(pObservedSeries%tData)
      iCountModeled = size(pModeledSeries%tData)

      iNumDigits = len_trim(asChar(iCountObserved) )

      if(len_trim(sObservedSeries) + 1 + iNumDigits > MAXNAMELENGTH) then
        call warn(lFALSE, "Cannot create comparison object: the series name " &
          //quote(sObservedSeries)//" is too long to allow for unique observation names to be created.")
        exit
      endif

      if(.not. allocated(this%tTSComparison)) then

        allocate(this%tTSComparison(1),stat=iStat)
        call Assert(iStat==0, "Unable to allocate memory for time series", &
          TRIM(__FILE__), __LINE__)

        this%tTSComparison(iCount + 1)%sObservedSeries = sObservedSeries
        this%tTSComparison(iCount + 1)%sModeledSeries = sModeledSeries
!        this%tTSComparison(1)%pObservedSeries => pObservedSeries
!        this%tTSComparison(1)%pModeledSeries => pModeledSeries
        this%tTSComparison(1)%sWeightsEquation = sEquationText
        allocate(this%tTSComparison(1)%rWeightValue(iCountObserved) , stat=iStat)
        call Assert(iStat==0, "Unable to allocate memory for time series", &
            TRIM(__FILE__), __LINE__)
        this%tTSComparison(1)%rWeightValue = this%calculate(sEquationText, iCountObserved)

      else   ! there are already TS comparison objects; add or replace

        ! get the number of time series currently in time series container
        iCount = size(this%tTSComparison)

        ! test to see if this comparison object already exists
        do i=1,iCount
          if( str_compare(this%tTSComparison(i)%sObservedSeries, sObservedSeries) &
            .and. str_compare(this%tTSComparison(i)%sModeledSeries, sModeledSeries) ) then
              lIsNewComparison = lFALSE
              iRecordToReplace = i
              exit
          endif
        enddo

        if( lIsNewComparison ) then
          ! allocate memory for size of current TSComparison object
          allocate(tTemtTSComparison(iCount),stat=iStat)
          call Assert(iStat==0, "Unable to allocate temporary memory for time " &
            //"series comparison object", &
            TRIM(__FILE__), __LINE__)

          ! make a copy of all previous TS objects
          tTemtTSComparison = this%tTSComparison

          ! deallocate TS comparison objects collection
          deallocate(this%tTSComparison, stat=iStat)
          call Assert(iStat==0, "Unable to deallocate memory for time " &
          //"series comparison object", &
          TRIM(__FILE__), __LINE__)

          ! allocate TS comparison objects collection to include room for new object
          allocate(this%tTSComparison(iCount + 1), stat=iStat)
          call Assert(iStat==0, "Unable to allocate memory for time series comparison object", &
            TRIM(__FILE__), __LINE__)

          this%tTSComparison(1:iCount) = tTemtTSComparison

!          this%tTSComparison(iCount + 1)%pObservedSeries => pObservedSeries
!          this%tTSComparison(iCount + 1)%pModeledSeries => pModeledSeries
          this%tTSComparison(iCount + 1)%sObservedSeries = sObservedSeries
          this%tTSComparison(iCount + 1)%sModeledSeries = sModeledSeries
          this%tTSComparison(iCount + 1)%sWeightsEquation = sEquationText
          allocate(this%tTSComparison(iCount + 1)%rWeightValue(iCountObserved) , stat=iStat)
          call Assert(iStat==0, "Unable to allocate memory for time series", &
            TRIM(__FILE__), __LINE__)

          this%tTSComparison(iCount + 1)%rWeightValue = this%calculate(sEquationText, iCountObserved)

        else

          ! if object already exists, just replace the equation text
          this%tTSComparison(iRecordToReplace)%sWeightsEquation = sEquationText
          this%tTSComparison(iRecordToReplace)%rWeightValue = this%calculate(sEquationText, iCountObserved)

        endif  ! lIsNewComparison

      endif  ! tTScomparison .not. allocated

      exit

    enddo


  end subroutine add_ts_comparison_sub

!------------------------------------------------------------------------------

  subroutine add_table_comparison_sub(this, sObservedTable, sModeledTable, sEquationText)

    class(TIME_SERIES_COLLECTION) :: this
    character(len=*), intent(in) :: sObservedTable
    character(len=*), intent(in) :: sModeledTable
    character(len=*), intent(in) :: sEquationText

    ! [ LOCALS ]
    type(T_TABLE_COMPARISON), dimension(:), allocatable :: tTempTableComparison
    type (T_TABLE), pointer :: pObservedTable, pModeledTable
    integer (kind=T_INT) :: iCount
    integer (kind=T_INT) :: iCountObserved, iCountModeled
    integer (kind=T_INT) :: iStat
    integer (kind=T_INT) :: i
    integer (kind=T_INT) :: iNumDigits
    integer (kind=T_INT) :: iRecordToReplace
    logical (kind=T_LOGICAL) :: lIsNewComparison

    ! get pointers to the two series involved
    pObservedTable => this%getTable(sObservedTable)
    pModeledTable => this%getTable(sModeledTable)

    iCountObserved = size(pObservedTable%tTableData)
    iCountModeled = size(pModeledTable%tTableData)
    iCount = 0

    lIsNewComparison = lTRUE

    ! ensure that we're about to compare tables of the same type
    call Assert(pObservedTable%iTableType == pModeledTable%iTableType, &
      "Cannot compare modeled table ("//quote(TABLE_TYPE(pModeledTable%iTableType)) &
      //") to observed table ("//quote(TABLE_TYPE(pObservedTable%iTableType)), &
      trim(__FILE__), __LINE__)

    do

      if( iCountObserved /= iCountModeled) then

        call warn(lFALSE, "Cannot create comparison object: "//trim(sObservedTable) &
          //" and "//trim(sModeledTable)//" do not have the same number of entries.")
        exit

      else  ! the two tables at least have the same number of entries; proceed

        iNumDigits = len_trim(asChar(iCountObserved) )

        if(len_trim(sObservedTable) + 1 + iNumDigits > MAXNAMELENGTH) then
          call warn(lFALSE, "Cannot create comparison object: the table name " &
            //quote(sObservedTable)//" is too long to allow for unique observation names to be created.")
          exit
        endif

        if(.not. allocated(this%tTableComparison)) then

          allocate(this%tTableComparison(1),stat=iStat)
          call Assert(iStat==0, "Unable to allocate memory for table comparison", &
            TRIM(__FILE__), __LINE__)

          this%tTableComparison(iCount + 1)%sObservedTable = sObservedTable
          this%tTableComparison(iCount + 1)%sModeledTable = sModeledTable
          this%tTableComparison(1)%sWeightsEquation = sEquationText
          allocate(this%tTableComparison(1)%rWeightValue(iCountObserved) , stat=iStat)
          call Assert(iStat==0, "Unable to allocate memory for table comparison", &
              TRIM(__FILE__), __LINE__)
          this%tTableComparison(1)%rWeightValue = this%calculate(sEquationText, iCountObserved)

        else   ! there are already table comparison objects; add or replace

          ! get the number of table comparisons currently in table comparisons container
          iCount = size(this%tTableComparison)

          ! test to see if this comparison object already exists
          do i=1,iCount
            if( str_compare(this%tTableComparison(i)%sObservedTable, sObservedTable) &
              .and. str_compare(this%tTableComparison(i)%sModeledTable, sModeledTable) ) then
                lIsNewComparison = lFALSE
                iRecordToReplace = i
                exit
            endif
          enddo

          if( lIsNewComparison ) then
            ! allocate memory for size of current TableComparison object
            allocate(tTempTableComparison(iCount),stat=iStat)
            call Assert(iStat==0, "Unable to allocate temporary memory for table " &
              //"comparison object", &
              TRIM(__FILE__), __LINE__)

            ! make a copy of all previous table objects
            tTempTableComparison = this%tTableComparison

            ! deallocate table comparison objects collection
            deallocate(this%tTableComparison, stat=iStat)
            call Assert(iStat==0, "Unable to deallocate memory for table " &
            //"comparison object", &
            TRIM(__FILE__), __LINE__)

            ! allocate table comparison objects collection to include room for new object
            allocate(this%tTableComparison(iCount + 1), stat=iStat)
            call Assert(iStat==0, "Unable to allocate memory for table comparison object", &
              TRIM(__FILE__), __LINE__)

            this%tTableComparison(1:iCount) = tTempTableComparison

            this%tTableComparison(iCount + 1)%sObservedTable = sObservedTable
            this%tTableComparison(iCount + 1)%sModeledTable = sModeledTable
            this%tTableComparison(iCount + 1)%sWeightsEquation = sEquationText
            allocate(this%tTableComparison(iCount + 1)%rWeightValue(iCountObserved) , stat=iStat)
            call Assert(iStat==0, "Unable to allocate memory for table comparison", &
              TRIM(__FILE__), __LINE__)

            this%tTableComparison(iCount + 1)%rWeightValue = this%calculate(sEquationText, iCountObserved)

          else

            ! if object already exists, just replace the equation text
            this%tTableComparison(iRecordToReplace)%sWeightsEquation = sEquationText
            this%tTableComparison(iRecordToReplace)%rWeightValue = this%calculate(sEquationText, iCountObserved)

          endif  ! lIsNewComparison

        endif  ! tTablecomparison .not. allocated

        exit

      endif  ! .not. dates are equal

    enddo

  end subroutine add_table_comparison_sub

!------------------------------------------------------------------------------

function calc_values_from_equation_fn(this,sFunctionText, iNumRecords) result(rOut)

  implicit none

  class(TIME_SERIES_COLLECTION) :: this
  character (len=*), intent(in) :: sFunctionText
  integer (kind=T_INT), intent(in) :: iNumRecords
  real (kind=T_SGL), dimension(:), allocatable :: rOut

  ! [ LOCALS ]
  character (len=MAXEQUATIONLENGTH) :: sFuncTxt
  character (len=4096) :: sBuf
  character (len=256), dimension(:), allocatable :: sVarTxt
  character (len=256), dimension(:), allocatable :: sSeriesTxt
  logical (kind=T_LOGICAL), dimension(:), allocatable :: lInclude
  logical (kind=T_LOGICAL) :: lConsistentTimebase
  integer (kind=T_INT) :: iNumFields, i, j
  character (len=5) :: sStatusFlag

  type (TIME_SERIES_COLLECTION), target :: TSCOL
  type (T_TIME_SERIES), pointer :: pTS
  real (kind=T_SGL), dimension(:), allocatable :: rTempValue
  integer (kind=T_INT) :: iNumSeries
  character (len=256) :: sPreviousSeriesName

  iNumFields = countFields(trim(sFuncTxt),OPERATORS//" ")

  allocate(sVarTxt(iNumFields), lInclude(iNumFields) )

  sFuncTxt = trim(sFunctionText)
  sBuf = sFuncTxt
  lInclude = lFALSE
  lConsistentTimebase = lTRUE

  ! scan equation for time series names; if found, add the time series name to
  ! a collections object and check for timebase consistency
  iNumSeries = 0
  do i=1,iNumFields
    call Chomp(sBuf, sVarTxt(i) , OPERATORS//" ")
!    print *, "|"//sVarTxt(i)//"|"
    if(isElement(sVarTxt(i), this%pTS%sSeriesName)) then
      iNumSeries = iNumSeries + 1
      pTS => this%getSeries( sVarTxt(i) )
      call TSCOL%insert( pNewSeries=pTS )
      lInclude(i) = lTRUE
      if(iNumSeries>1) then
        lConsistentTimebase = this%datesEqualByName(sPreviousSeriesName, sVarTxt(i) )
        if(.not. lConsistentTimebase) exit
      endif
      sPreviousSeriesName = sVarTxt(i)
    endif
  enddo

  if(lConsistentTimebase) then

    if(iNumSeries > 0) then

      allocate(sSeriesTxt(iNumSeries))
      sSeriesTxt = pack(sVarTxt, lInclude)

!      print *, iNumSeries
!      print *, sSeriesTxt

      call init_equation (sFuncTxt, sSeriesTxt, sStatusflag)

      call warn(str_compare(sStatusflag, "ok"), "Equation was not properly initialized", &
        trim(__FILE__),__LINE__)

      allocate(rOut(iNumRecords) )

      allocate(rTempValue( count(lInclude) ) )

      do i=1,iNumRecords
        do j=1,size(TSCOL%pTS)
          rTempValue(j) = TSCOL%pTS(j)%tData(i)%rValue
        enddo
        rOut(i) = evaluate_expression (rTempValue , TSCOL%pTS(1)%tData(i)%tDT )
      enddo

    else  ! no time series are referenced in equation text; modify calls accordingly

      deallocate(sVarTxt); allocate(sVarTxt(1) )
      sVarTxt = "none"
      call init_equation (sFuncTxt, sVarTxt, sStatusflag)

      call warn(str_compare(sStatusflag, "ok"), "Equation was not properly initialized", &
        trim(__FILE__),__LINE__)

      allocate(rOut(iNumRecords) )

      allocate(rTempValue(1) )
      rTempValue = 0.

      do i=1,iNumRecords
        rOut(i) = evaluate_expression (rTempValue)
      enddo

    endif

    call destroyfunc()

  else

    allocate(rOut(1))
    rOut = -huge(rOut)

    call warn(lFALSE,"Time series referenced in equation have differing time bases" &
      //" - cannot evaluate equation", trim(__FILE__),__LINE__)

  endif

  deallocate(sVarTxt)

  do j=1,size(TSCOL%pTS)
    deallocate( TSCOL%pTS(j)%tData )
  enddo

end function calc_values_from_equation_fn

end module tsp_collections
