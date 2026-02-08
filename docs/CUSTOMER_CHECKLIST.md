# COBOL 분석 의뢰 체크리스트

> CATM (COBOL Analysis Task Manager) 분석을 위해 고객에게 요청할 정보

---

## 1. 소스코드 전달 (필수)

### 필수 파일
- [ ] **COBOL 프로그램** (.cbl, .cob) - 분석 대상 메인 프로그램
- [ ] **COPYBOOK** (.cpy) - 공통 데이터 구조 정의

### 선택 파일 (해당 시)
- [ ] **JCL** (.jcl) - 배치 작업 정의
- [ ] **PROC** (.proc) - JCL 프로시저
- [ ] **DCLGEN** - DB2 테이블 COBOL 선언
- [ ] **DB2 DDL** (.ddl) - 데이터베이스 스키마
- [ ] **CICS MAP/BMS** (.bms) - 온라인 화면 정의

---

## 2. 추출 시 주의사항

> ⚠️ **반드시 ASCII 모드로 추출하세요!**

```bash
# FTP 예시
ftp mainframe.yourcompany.com
ascii                          # ← 필수! (EBCDIC → UTF-8 자동 변환)
cd 'YOUR.COBOL.SOURCE'
mget *
```

### 확인 사항
- [ ] ASCII 모드로 추출 (binary 모드 X)
- [ ] 파일 인코딩 UTF-8 확인
- [ ] 파일이 정상적으로 열리는지 확인 (한글 깨짐 없음)

---

## 3. 프로젝트 정보

| 항목 | 내용 |
|------|------|
| 시스템명 | |
| 업무 영역 | (예: 생산관리, 재고관리, 품질관리) |
| 우선 분석 프로그램 | (예: PGM001, BATCH100) |
| 모더나이제이션 목표 | (예: Java/Spring, Cloud 전환) |
| 현업 검증 담당자 | (이름/연락처) |

---

## 4. 기술 환경 정보 (선택)

| 항목 | 내용 |
|------|------|
| 메인프레임 OS | (예: z/OS) |
| COBOL 컴파일러 버전 | (예: IBM Enterprise COBOL 6.x) |
| DB2 버전 | (예: DB2 for z/OS 12) |
| CICS 버전 | (예: CICS TS 5.x) |
| 형상관리 도구 | (예: Endevor, ChangeMan) |

---

## 5. 파일 전달 방법

### 권장 디렉토리 구조
```
전달파일/
├── cobol/      ← COBOL 프로그램 (.cbl)
├── copybook/   ← COPYBOOK (.cpy)
├── jcl/        ← JCL (.jcl)
├── proc/       ← PROC (.proc)
├── dclgen/     ← DCLGEN
├── ddl/        ← DB2 DDL
└── map/        ← CICS MAP/BMS (.bms)
```

### 전달 방법
- [ ] 압축파일 (ZIP) 전달
- [ ] 클라우드 공유 (Google Drive, OneDrive 등)
- [ ] FTP/SFTP 업로드

---

## 6. 분석 결과물 안내

CATM 분석 완료 시 다음 결과물이 생성됩니다:

| 결과물 | 설명 |
|--------|------|
| **비즈니스 로직 문서** | 프로그램별 한글 비즈니스 로직 해석 |
| **호출관계 다이어그램** | CALL/COPY 의존성 Mermaid 다이어그램 |
| **데이터 사전** | COPYBOOK 기반 필드 정의서 |
| **우선순위 매트릭스** | 모더나이제이션 전환 우선순위 |

---

> 📧 문의: [담당자 이메일]  
> 📞 연락처: [담당자 전화번호]
