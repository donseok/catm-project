---
description: COBOL 의존성 추출 + 다이어그램 생성 (Phase 2+4)
allowed-tools: Bash(python3 catm/scripts/02*), Bash(python3 catm/scripts/04*), Read
---

# CATM 의존성 추출 및 다이어그램 생성

순차 실행:

```bash
python3 catm/scripts/02_extract_dependencies.py
```

```bash
python3 catm/scripts/04_generate_diagrams.py
```

실행 후:
1. `output/reports/dependency-scan.json`을 읽어서 의존성 요약 보고
2. CALL 관계, COPYBOOK 참조, DB2 테이블, 복잡도 핵심 지표 표시
3. 생성된 Mermaid 다이어그램 파일 목록 표시
