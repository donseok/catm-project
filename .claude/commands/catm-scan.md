---
description: COBOL 소스코드 인벤토리 스캔 (Phase 1)
allowed-tools: Bash(python3 catm/scripts/01*), Read
---

# CATM 인벤토리 스캔

```bash
python3 catm/scripts/01_scan_inventory.py
```

실행 후:
1. `output/reports/inventory.json` 결과를 읽어서 요약 보고
2. COBOL 프로그램 수, 총 라인 수, COPYBOOK 수 등 핵심 지표 표시
