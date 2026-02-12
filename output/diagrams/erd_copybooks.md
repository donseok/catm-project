# 데이터 ERD (Entity Relationship Diagram)

```mermaid
erDiagram

    CPYBFOP {
        string BO_BF_NO
        number BO_OPER_DATE
        number BO_OPER_SEQ
        string BO_OPER_TYPE
        string BO_SHIFT_CD
        number BO_PRESSURE
        number BO_WIND_VOL
        number BO_COKE_RATE
        number BO_TAP_QTY
        number BO_SLAG_QTY
        string BO_REMARK_CD
        number BO_REG_TIME
        string BO_WORKER_ID
        string FILLER
    }

    CPYBFTM {
        string BT_BF_NO
        number BT_MEASURE_DT
        number BT_MEASURE_SEQ
        number BT_TEMP_VAL
        string BT_SENSOR_ID
        string BT_LOCATION_CD
        string BT_MEASURE_TYPE
        string BT_STATUS_CD
        number BT_REG_TIME
        string FILLER
    }

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

    CPYPACT {
        string PA_PRODUCT_CD
        number PA_YYYYMM
        number PA_ACTUAL_QTY
        number PA_WORK_DAYS
        number PA_LINE_CNT
        number PA_DEFECT_QTY
        number PA_YIELD_RATE
        number PA_LAST_UPD
        string FILLER
    }

    CPYPPLAN {
        string PP_PLANT_CD
        string PP_PRODUCT_CD
        number PP_YYYYMM
        number PP_PLAN_QTY
        string PP_UNIT_CD
        string PP_LINE_CD
        number PP_PRIORITY
        string PP_PLAN_TYPE
        number PP_REG_DATE
        string PP_REG_USER
        string FILLER
    }

    CPYQCHS {
        number QH_INSPECT_ID
        string QH_LINE_CD
        string QH_PRODUCT_CD
        string QH_JUDGE_CD
        number QH_INSPECT_DT
        string QH_FINAL_RESULT
        string FILLER
    }

    CPYQCRS {
        string QR_PLANT_CD
        string QR_LINE_CD
        number QR_INSPECT_DT
        number QR_INSPECT_ID
        string QR_PRODUCT_CD
        string QR_LOT_NO
        string QR_JUDGE_CD
        string QR_DEFECT_TYPE
        string QR_DEFECT_CD
        number QR_MEASURE_VAL
        number QR_UPPER_LIMIT
        number QR_LOWER_LIMIT
        number QR_REWORK_CNT
        string QR_INSPECTOR_ID
        number QR_REG_TIME
        string FILLER
    }

    CPYRMMS {
        string RM_MATL_CD
        string RM_MATL_NM
        string RM_CATEGORY
        string RM_UNIT_CD
        number RM_UNIT_PRICE
        number RM_STOCK_QTY
        number RM_SAFETY_QTY
        number RM_LEAD_DAYS
        string RM_MAIN_VENDOR
        number RM_LAST_IN_DT
        number RM_LAST_OUT_DT
        string FILLER
    }

    CPYRMRC {
        number RR_RECEIPT_NO
        number RR_RECEIPT_DT
        string RR_MATL_CD
        string RR_MATL_NM
        string RR_VENDOR_CD
        number RR_RECEIPT_QTY
        string RR_UNIT_CD
        string RR_INSPECT_CD
        number RR_ACCEPT_RATE
        string RR_REASON_CD
        string RR_WAREHOUSE_CD
        string RR_PO_NO
        string RR_REG_USER
        string FILLER
    }

    CPYSMRY {
        string DS_PLANT_CD
        number DS_PROD_DATE
        number DS_TOTAL_QTY
        number DS_TOTAL_COUNT
        number DS_ERROR_COUNT
        number DS_PROCESS_TIME
        string DS_STATUS_CD
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
