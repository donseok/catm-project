---
description: CATM 전체 파이프라인 실행 (Phase 1-6) 또는 특정 Phase만 실행
argument-hint: [phase-number (생략 시 전체)]
allowed-tools: Bash(./catm.sh*), Bash(python3 catm/scripts/*), Read
---

# CATM 파이프라인 실행

인자: `$ARGUMENTS`

## 실행 규칙
- 인자가 없으면 `./catm.sh` 로 전체 파이프라인(Phase 1-6) 실행
- 인자가 숫자(1-6)이면 `./catm.sh --phase $ARGUMENTS` 로 해당 Phase만 실행
- 실행 후 결과를 요약해서 보고

```bash
# 인자 없으면 전체, 숫자면 해당 Phase
./catm.sh $ARGUMENTS
```

## Phase 참고
| Phase | 기능 |
|-------|------|
| 1 | 인벤토리 스캔 |
| 2 | 의존성 추출 |
| 3 | COPYBOOK 파싱 |
| 4 | 다이어그램 생성 |
| 5 | Claude 비즈니스 로직 분석 |
| 6 | 우선순위 산정 |
