import sys
import os

# Add project root to path
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))

from catm.utils.cobol_parser import analyze_program, extract_db2_tables, extract_cics_maps, extract_calls

def test_baseline():
    print("\n--- Testing Baseline (PGM001) ---")
    try:
        pgm = analyze_program('src/cobol/PGM001.cbl')
        print(f"Name: {pgm.name}")
        print(f"Calls: {pgm.calls}")
        print(f"Tables: {pgm.db2_tables}")
        # Expected: ['TB_DAILY_PROD']
    except Exception as e:
        print(f"Error: {e}")

def test_complex_sql():
    print("\n--- Testing Complex SQL (JOIN) ---")
    source = """
    EXEC SQL
        SELECT A.COL1, B.COL2
        FROM TB_MASTER A
        INNER JOIN TB_DETAIL B
        ON A.KEY = B.KEY
        WHERE A.STATUS = 'Y'
    END-EXEC.
    """
    tables = extract_db2_tables(source)
    print(f"Extracted Tables: {tables}")
    if 'TB_DETAIL' not in tables:
        print("FAIL: Missed TB_DETAIL (JOIN)")
    else:
        print("PASS: Found TB_DETAIL")

def test_cics_commands():
    print("\n--- Testing CICS Commands (LINK) ---")
    source = """
    EXEC CICS LINK PROGRAM('PGM_SUB')
              COMMAREA(WS-COMMAREA)
              LENGTH(WS-LEN)
    END-EXEC.
    """
    # extract_cics_maps only looks for SEND/RECEIVE MAP
    # We want to see if we can/should extract LINKed programs as calls
    # Currently analyze_program uses extract_calls for CALL '...'
    # It does NOT check EXEC CICS LINK
    
    # Let's check if it's in calls or maps
    calls = extract_calls(source)
    maps = extract_cics_maps(source)
    
    print(f"Calls: {calls}")
    print(f"Maps: {maps}")
    
    if 'PGM_SUB' not in calls:
        print("FAIL: Missed CICS LINK PROGRAM as a call dependency")

def test_dynamic_call():
    print("\n--- Testing Dynamic Call without USING ---")
    source = """
    MOVE 'SUBPGM' TO WS-PGM-NAME.
    CALL WS-PGM-NAME.
    """
    calls = extract_calls(source)
    print(f"Calls: {calls}")
    # Current regex: r"CALL\s+([A-Z0-9-]+)\s+USING" -> requires USING
    # Matches literals: r"CALL\s+'([A-Z0-9_-]+)'"
    
    if 'WS-PGM-NAME' not in calls:
        print("FAIL: Missed Dynamic CALL without USING or it is expected to be missed if we only want literals/USING variables")

if __name__ == "__main__":
    test_baseline()
    test_complex_sql()
    test_cics_commands()
    test_dynamic_call()
