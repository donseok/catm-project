# CLAUDE.md - CATM 프로젝트 컨텍스트

## 프로젝트 정보
이 프로젝트는 IBM 메인프레임 COBOL 레거시 시스템을 자동 분석하는
"COBOL Analysis Task Manager (CATM)"입니다.

## Claude Code 사용 방식
- **구독**: Claude Max 20x 구독 사용 (API 아님)
- **실행**: `claude` CLI로 직접 호출
- **용도**: COBOL 소스코드 → 한글 비즈니스 로직 해석, 데이터 구조 분석, 우선순위 산정

## 디렉토리 규칙
- `src/` : 메인프레임에서 추출한 원본 COBOL 소스 (읽기 전용으로 취급)
- `catm/` : 분석 봇 코어 모듈
- `catm/prompts/` : Claude에게 보낼 프롬프트 템플릿
- `output/` : 모든 분석 결과물은 여기에 생성
- `tests/sample-cobol/` : 테스트용 COBOL 샘플

## 코딩 규칙
- Python 3.11+
- 타입 힌트 필수
- 한글 주석 사용
- 파일 인코딩: UTF-8
- COBOL 소스 파싱 시 컬럼 규칙 준수 (1-6: 시퀀스, 7: 표시자, 8-72: 코드)

## 주요 명령어
```bash
# 전체 분석 실행
./catm.sh

# 단일 프로그램 Claude 분석
python catm/scripts/05_analyze_with_claude.py src/cobol/PGM001.cbl

# 테스트
python -m pytest tests/

# 인벤토리 스캔
python catm/scripts/01_scan_inventory.py
```

## Claude Code 호출 패턴
이 프로젝트에서 Claude Code를 호출할 때는 반드시 다음 패턴을 따릅니다:
1. 프롬프트 템플릿 로드 (catm/prompts/)
2. COBOL 소스 + COPYBOOK을 컨텍스트로 주입
3. `claude -p` 또는 `claude --print` 명령으로 실행
4. 결과를 output/ 디렉토리에 마크다운으로 저장
