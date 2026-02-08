---
description: CATM 프로젝트 테스트 실행
argument-hint: [테스트 파일 또는 패턴 (생략 시 전체)]
allowed-tools: Bash(python3 -m pytest*)
---

# CATM 테스트 실행

인자: `$ARGUMENTS`

## 실행 규칙
- 인자가 없으면 전체 테스트 실행:
  ```bash
  python3 -m pytest tests/ -v
  ```
- 인자가 있으면 해당 테스트만 실행:
  ```bash
  python3 -m pytest $ARGUMENTS -v
  ```
- 실패한 테스트가 있으면 원인을 분석하고 수정 방안을 제시
