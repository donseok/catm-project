      *================================================================*
      * COPYBOOK: CPYINVMS                                            *
      * 설명    : 재고 마스터 레코드                                   *
      *================================================================*
       05  IM-ITEM-CD              PIC X(15).
       05  IM-ITEM-NAME            PIC X(50).
       05  IM-CATEGORY             PIC X(10).
       05  IM-UNIT-CD              PIC X(05).
       05  IM-CURR-QTY             PIC S9(9) COMP-3.
       05  IM-MIN-QTY              PIC S9(9) COMP-3.
       05  IM-MAX-QTY              PIC S9(9) COMP-3.
       05  IM-UNIT-COST            PIC S9(9)V99 COMP-3.
       05  IM-LAST-DATE            PIC 9(08).
       05  IM-STATUS               PIC X(01).
           88  IM-ACTIVE           VALUE 'A'.
           88  IM-INACTIVE         VALUE 'I'.
           88  IM-DISCONTINUED     VALUE 'D'.
       05  FILLER                  PIC X(20).
