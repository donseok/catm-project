      *================================================================*
      * COPYBOOK: CPYLEDGR                                            *
      * 설명    : 수불대장 레코드                                      *
      *================================================================*
       05  LG-ITEM-CD              PIC X(15).
       05  LG-TRANS-DATE           PIC 9(08).
       05  LG-TRANS-TYPE           PIC X(01).
       05  LG-QTY                  PIC S9(9) COMP-3.
       05  LG-PREV-QTY             PIC S9(9) COMP-3.
       05  LG-CURR-QTY             PIC S9(9) COMP-3.
       05  LG-UNIT-PRICE           PIC S9(9)V99 COMP-3.
       05  LG-AMOUNT               PIC S9(11)V99 COMP-3.
       05  FILLER                  PIC X(20).
