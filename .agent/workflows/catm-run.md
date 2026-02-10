---
description: CATM COBOL 분석 실행
---

# CATM 분석 실행 워크플로우

COBOL 소스코드를 분석하고 결과를 대시보드에서 확인합니다.

## 사전 조건
- `src/cobol/` 폴더에 COBOL 소스 파일이 있어야 함
- `src/copybook/` 폴더에 COPYBOOK 파일이 있어야 함

## 실행 단계

// turbo-all

1. 가상환경 활성화 및 CATM 분석 실행:
```bash
cd /Users/jerry/catm-project && source venv/bin/activate && ./catm.sh
```

2. 분석 결과 확인:
```bash
ls -la /Users/jerry/catm-project/output/docs/
```

3. 대시보드 서버 실행 (이미 실행 중이면 생략):
```bash
cd /Users/jerry/catm-project && python3 -m http.server 8080
```

4. 브라우저에서 http://localhost:8080 접속하여 결과 확인

## 분석 결과 위치

| 결과물 | 경로 |
|--------|------|
| 비즈니스 로직 문서 | `output/docs/*.md` |
| 호출관계 다이어그램 | `output/diagrams/call_graph.md` |
| 데이터 사전 | `output/data-dict/*.md` |
| 우선순위 매트릭스 | `output/reports/priority_matrix.md` |
