---
description: 단일 COBOL 프로그램 또는 COPYBOOK을 Claude로 분석
argument-hint: [파일경로] (예: src/cobol/PGM001.cbl)
allowed-tools: Bash(python3 catm/scripts/05*), Read
---

# CATM Claude 분석 실행

대상 파일: `$ARGUMENTS`

## 실행 규칙
- `.cbl`, `.cob` 파일이면 logic 모드로 분석:
  ```bash
  python3 catm/scripts/05_analyze_with_claude.py --single $ARGUMENTS
  ```
- `.cpy`, `.dclgen` 또는 copybook/dclgen 경로면 data 모드로 분석:
  ```bash
  python3 catm/scripts/05_analyze_with_claude.py --mode data --single $ARGUMENTS
  ```
- 실행 완료 후 `output/docs/` 또는 `output/data-dict/`에 생성된 결과 파일을 읽어서 요약 보고
