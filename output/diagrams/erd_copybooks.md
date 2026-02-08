# 데이터 ERD (Entity Relationship Diagram)

```mermaid
erDiagram

    CPYINVMS {
        string IM_ITEM_CD
        string IM_ITEM_NAME
        string IM_CATEGORY
        string IM_UNIT_CD
        number IM_CURR_QTY
        number IM_MIN_QTY
        number IM_MAX_QTY
        number IM_UNIT_COST
        number IM_LAST_DATE
        string IM_STATUS
        string FILLER
    }

    CPYINVTR {
        string IT_PLANT_CD
        string IT_ITEM_CD
        number IT_TRANS_DATE
        number IT_TRANS_SEQ
        string IT_TRANS_TYPE
        number IT_QTY
        number IT_UNIT_PRICE
        string IT_WAREHOUSE_CD
        string IT_LOCATION_CD
        string IT_VENDOR_CD
        string IT_PO_NO
        string IT_USER_ID
        number IT_REG_TIME
        string FILLER
    }

    CPYLEDGR {
        string LG_ITEM_CD
        number LG_TRANS_DATE
        string LG_TRANS_TYPE
        number LG_QTY
        number LG_PREV_QTY
        number LG_CURR_QTY
        number LG_UNIT_PRICE
        number LG_AMOUNT
        string FILLER
    }

    CPYTRANS {
        string PT_PLANT_CD
        string PT_LINE_CD
        number PT_PROD_DATE
        number PT_SEQ_NO
        string PT_PRODUCT_CD
        number PT_QTY
        string PT_UNIT_CD
        string PT_WORKER_ID
        string PT_SHIFT_CD
        string PT_STATUS_CD
        number PT_REG_TIME
        number PT_UPD_TIME
        string FILLER
    }

```
