      *================================================================*
      * COPYBOOK: CPYINVTR                                            *
      * 설명    : 재고 입출고 트랜잭션 레코드                          *
      *================================================================*
       05  IT-KEY.
           10  IT-PLANT-CD         PIC X(04).
           10  IT-ITEM-CD          PIC X(15).
           10  IT-TRANS-DATE       PIC 9(08).
           10  IT-TRANS-SEQ        PIC 9(05).
       05  IT-DATA.
           10  IT-TRANS-TYPE       PIC X(01).
               88  IT-IN           VALUE 'I'.
               88  IT-OUT          VALUE 'O'.
               88  IT-ADJ          VALUE 'A'.
           10  IT-QTY              PIC S9(9) COMP-3.
           10  IT-UNIT-PRICE       PIC S9(9)V99 COMP-3.
           10  IT-WAREHOUSE-CD     PIC X(05).
           10  IT-LOCATION-CD      PIC X(10).
           10  IT-VENDOR-CD        PIC X(10).
           10  IT-PO-NO            PIC X(15).
           10  IT-USER-ID          PIC X(10).
           10  IT-REG-TIME         PIC 9(06).
           10  FILLER              PIC X(10).
