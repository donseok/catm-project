# CATM - COBOL Analysis Task Manager

> IBM 메인프레임 COBOL 레거시 시스템을 Claude Code(Max 20x 구독)로 자동 분석하는 봇

## 🎯 프로젝트 목적

COBOL 분석 인력 없이도 레거시 시스템을 완전히 분석하여
MES 모더나이제이션을 위한 기반 문서를 자동 생성합니다.

## 📋 핵심 기능 4가지

| # | 기능 | 설명 |
|---|------|------|
| F1 | 비즈니스 로직 해석 | COBOL → 한글 비즈니스 로직 마크다운 문서 |
| F2 | 호출관계 매핑 | CALL/COPY/JCL 의존성 → Mermaid 다이어그램 |
| F3 | 데이터 구조 분석 | COPYBOOK/DB2 → 데이터 사전 + ERD |
| F4 | 모더나이제이션 우선순위 | 복잡도/의존성/중요도 → 전환 우선순위 매트릭스 |

## ⚡ Claude Code Max 20x 구독 사용

이 프로젝트는 **Anthropic API가 아닌 Claude Code Max 구독(20배)**을 사용합니다.

### API vs Max 구독 차이

| 항목 | API 방식 | Max 20x 구독 (이 프로젝트) |
|------|---------|---------------------------|
| 비용 | 토큰당 과금 (비쌈) | 월정액 구독 (예측 가능) |
| 호출 방식 | `anthropic.Anthropic()` | `claude` CLI 직접 실행 |
| 사용량 | 토큰 제한 | 20배 확장 사용량 |
| 모델 | API 모델 지정 | Max 구독 모델 자동 |
| 적합 용도 | 프로덕션 서비스 | 대량 분석/개발 작업 |

### Claude Code 설치 및 설정

```bash
# 1. Claude Code 설치 (Node.js 18+ 필요)
npm install -g @anthropic-ai/claude-code

# 2. 로그인 (Max 구독 계정으로)
claude login

# 3. Max 20x 구독 확인
claude config list
# → plan: max-20x 확인

# 4. 사용량 확인
claude usage
```

## 🚀 빠른 시작

### 1단계: 환경 설정
```bash
# 프로젝트 클론 또는 IDE에서 열기
cd catm-project

# Python 의존성 설치
pip install -r requirements.txt

# 실행 권한 부여
chmod +x catm.sh
chmod +x catm/scripts/*.py
```

### 2단계: COBOL 소스코드 배치
```bash
# 메인프레임에서 추출한 소스를 각 디렉토리에 배치
src/
  cobol/     ← *.cbl 파일
  copybook/  ← *.cpy 파일
  jcl/       ← *.jcl 파일
  dclgen/    ← DCLGEN 파일
  ddl/       ← DB2 DDL 파일
```

### 3단계: 분석 실행
```bash
# 전체 분석 (Phase 1-6 순차 실행)
./catm.sh

# 또는 단계별 실행
./catm.sh --phase 1    # 인벤토리 스캔
./catm.sh --phase 2    # 의존성 추출 (정적 분석)
./catm.sh --phase 3    # COPYBOOK 파싱 → 데이터 사전
./catm.sh --phase 4    # Mermaid 다이어그램 생성
./catm.sh --phase 5    # Claude Code 비즈니스 로직 분석
./catm.sh --phase 6    # 모더나이제이션 우선순위 산정

# 단일 프로그램 분석
./catm.sh --single src/cobol/PGM001.cbl
```

### 4단계: 결과 확인
```bash
output/
  docs/       ← 프로그램별 비즈니스 로직 문서 (*.md)
  diagrams/   ← Mermaid 다이어그램 (*.mermaid)
  data-dict/  ← 데이터 사전 (*.md)
  reports/    ← 우선순위 매트릭스, 종합 보고서 (*.md)
```

## 📁 프로젝트 구조

```
catm-project/
├── README.md                  ← 이 파일
├── requirements.txt           ← Python 의존성
├── catm.sh                    ← 메인 실행 스크립트
├── CLAUDE.md                  ← Claude Code 프로젝트 설정
│
├── catm/                      ← CATM 코어 모듈
│   ├── scripts/
│   │   ├── 01_scan_inventory.py       ← 전체 소스 인벤토리 스캔
│   │   ├── 02_extract_dependencies.py ← CALL/COPY/DB2 의존성 추출
│   │   ├── 03_parse_copybook.py       ← COPYBOOK 파서 + 데이터사전
│   │   ├── 04_generate_diagrams.py    ← Mermaid 다이어그램 생성
│   │   ├── 05_analyze_with_claude.py  ← Claude Code 호출 (비즈니스 로직)
│   │   └── 06_prioritize.py           ← 모더나이제이션 우선순위 산정
│   ├── prompts/
│   │   ├── analyze-logic.md           ← 비즈니스 로직 분석 프롬프트
│   │   ├── analyze-data.md            ← 데이터 구조 분석 프롬프트
│   │   └── prioritize.md             ← 우선순위 평가 프롬프트
│   ├── utils/
│   │   ├── cobol_parser.py            ← COBOL 공통 파서 유틸
│   │   ├── file_utils.py              ← 파일 I/O 유틸
│   │   └── mermaid_builder.py         ← Mermaid 다이어그램 빌더
│   └── config/
│       └── catm_config.yaml           ← CATM 설정 파일
│
├── src/                       ← COBOL 소스코드 (메인프레임에서 추출)
│   ├── cobol/
│   ├── copybook/
│   ├── jcl/
│   ├── proc/
│   ├── dclgen/
│   ├── ddl/
│   └── map/
│
├── output/                    ← 분석 결과물
│   ├── docs/
│   ├── diagrams/
│   ├── data-dict/
│   └── reports/
│
└── tests/                     ← 테스트 + 샘플
    └── sample-cobol/
```

## 📊 메인프레임 소스 추출 가이드

[EXTRACTION_GUIDE.md](./EXTRACTION_GUIDE.md) 참고

## ⚠️ 주의사항

1. **EBCDIC → UTF-8**: 메인프레임에서 FTP로 추출 시 `ascii` 모드 사용
2. **보안**: 소스코드는 로컬에서만 처리 (Claude Code는 로컬 실행)
3. **현업 검증**: AI 분석 결과는 반드시 현업 담당자가 검증
4. **점진적 진행**: MES 관련 핵심 모듈부터 단계적으로 분석
