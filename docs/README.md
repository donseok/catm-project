# CATM 프로젝트 문서 허브

> **CATM** (COBOL Analysis Task Manager) - IBM 메인프레임 COBOL 레거시 시스템 자동 분석 도구

---

## 문서 목록

| 문서 | 대상 독자 | 내용 |
|------|----------|------|
| [01. 프로젝트 개요](./01_PROJECT_OVERVIEW.md) | **전체** | CATM이 무엇인지, 왜 필요한지, 전체 구조 |
| [02. 분석 파이프라인](./02_ANALYSIS_PIPELINE.md) | **분석가, 기획자** | 6-Phase 분석 프로세스 상세, 각 Phase별 입출력 |
| [03. 시스템 아키텍처](./03_SYSTEM_ARCHITECTURE.md) | **설계자, 개발자** | 모듈 구조, 데이터 흐름, 클래스 설계 |
| [04. 설치 및 실행 가이드](./04_GETTING_STARTED.md) | **전체** | 환경 설정, 설치, 실행 방법 |
| [05. 산출물 해석 가이드](./05_OUTPUT_GUIDE.md) | **분석가, 기획자** | 분석 결과물 종류, 해석 방법, 활용 방안 |
| [06. 설정 참조](./06_CONFIG_REFERENCE.md) | **설계자, 개발자** | catm_config.yaml 전체 설정 항목 설명 |
| [고객 체크리스트](./CUSTOMER_CHECKLIST.md) | **고객** | 소스코드 추출 전 확인사항 |

---

## 역할별 필독 가이드

### 분석가 (Analyst)
1. [01. 프로젝트 개요](./01_PROJECT_OVERVIEW.md) - CATM 전체 이해
2. [02. 분석 파이프라인](./02_ANALYSIS_PIPELINE.md) - 6-Phase 흐름 숙지
3. [05. 산출물 해석 가이드](./05_OUTPUT_GUIDE.md) - 결과물 해석 방법

### 기획자 (Planner)
1. [01. 프로젝트 개요](./01_PROJECT_OVERVIEW.md) - 전체 범위 파악
2. [02. 분석 파이프라인](./02_ANALYSIS_PIPELINE.md) - 프로세스 이해
3. [05. 산출물 해석 가이드](./05_OUTPUT_GUIDE.md) - 우선순위 매트릭스 활용

### 설계자 (Architect)
1. [03. 시스템 아키텍처](./03_SYSTEM_ARCHITECTURE.md) - 모듈 구조 이해
2. [06. 설정 참조](./06_CONFIG_REFERENCE.md) - 커스터마이징 포인트
3. [02. 분석 파이프라인](./02_ANALYSIS_PIPELINE.md) - 데이터 흐름 파악

### 개발자 (Developer)
1. [04. 설치 및 실행 가이드](./04_GETTING_STARTED.md) - 환경 세팅
2. [03. 시스템 아키텍처](./03_SYSTEM_ARCHITECTURE.md) - 코드 구조 이해
3. [06. 설정 참조](./06_CONFIG_REFERENCE.md) - 설정 변경 방법
