# IBM 메인프레임 소스코드 추출 가이드

> CATM 분석을 위해 메인프레임에서 소스코드를 추출하는 방법

---

## 1. 추출 대상 확인

메인프레임 관리자에게 아래 PDS(Partitioned Data Set) 목록을 확인하세요.

| 구분 | 일반적인 PDS 명명 패턴 | 설명 |
|------|----------------------|------|
| COBOL 소스 | `*.COBOL.SOURCE`, `*.CBL.SOURCE` | 메인 프로그램 |
| COPYBOOK | `*.COPYBOOK.SOURCE`, `*.CPY.SOURCE` | 공통 데이터 구조 |
| JCL | `*.JCL.SOURCE`, `*.JCL.CNTL` | 배치 작업 제어 |
| PROC | `*.PROC.SOURCE`, `*.PROCLIB` | JCL 프로시저 |
| DCLGEN | `*.DCLGEN.SOURCE` | DB2 테이블 COBOL 선언 |
| DB2 DDL | `*.DDL.SOURCE` | 데이터베이스 스키마 |
| CICS MAP | `*.MAP.SOURCE`, `*.BMS.SOURCE` | 화면 정의 |

---

## 2. 추출 방법

### 방법 A: FTP (가장 간단)

```bash
# 1. FTP 접속
ftp mainframe.yourcompany.com

# 2. ASCII 모드 설정 (EBCDIC → ASCII 자동 변환)
ascii

# 3. COBOL 소스 다운로드
cd 'YOUR.COBOL.SOURCE'
lcd src/cobol
mget *

# 4. COPYBOOK 다운로드
cd 'YOUR.COPYBOOK.SOURCE'
lcd ../copybook
mget *

# 5. JCL 다운로드
cd 'YOUR.JCL.SOURCE'
lcd ../jcl
mget *

# 6. DCLGEN 다운로드
cd 'YOUR.DCLGEN.SOURCE'
lcd ../dclgen
mget *
```

> ⚠️ 반드시 `ascii` 모드를 사용하세요. `binary`로 받으면 EBCDIC 인코딩 문제가 발생합니다.

### 방법 B: z/OSMF REST API (자동화)

```bash
#!/bin/bash
# z/OSMF REST API로 PDS 멤버 일괄 다운로드

HOST="https://mainframe.yourcompany.com:443"
AUTH=$(echo -n 'userid:password' | base64)

# PDS 멤버 목록 조회
MEMBERS=$(curl -sk -X GET "$HOST/zosmf/restfiles/ds/YOUR.COBOL.SOURCE/member" \
  -H "Authorization: Basic $AUTH" \
  -H "Accept: application/json" | python3 -c "
import sys, json
data = json.load(sys.stdin)
for m in data.get('items', []):
    print(m['member'])
")

# 각 멤버 다운로드
mkdir -p src/cobol
for MEMBER in $MEMBERS; do
    echo "다운로드: $MEMBER"
    curl -sk -X GET "$HOST/zosmf/restfiles/ds/YOUR.COBOL.SOURCE($MEMBER)" \
      -H "Authorization: Basic $AUTH" \
      -H "Accept: text/plain" \
      -o "src/cobol/${MEMBER}.cbl"
done
```

### 방법 C: Endevor (형상관리 도구)

```
// Endevor BATCH 추출 JCL
//EXTRACT  JOB ...
//STEP1    EXEC PROC=NDVRC1
//SYSIN    DD *
  RETRIEVE ELEMENT *
    FROM ENVIRONMENT 'PROD'
         SYSTEM 'MES'
         SUBSYSTEM '*'
         TYPE 'COBOL'
         STAGE 1
    TO DSNAME 'YOUR.EXTRACT.COBOL'
    OPTIONS CCID 'CATM-EXTRACT'
             COMMENT 'CATM 분석용 추출'
/*
```

### 방법 D: ISPF 수동 다운로드

1. TSO 로그인 → ISPF
2. Option 3.4 (DSLIST)
3. PDS 선택 → 멤버 목록 조회
4. TSO 명령: `TSO XMIT` 또는 `ISPF Edit → CREATE` 로 순차파일 생성
5. FTP로 순차파일 다운로드

---

## 3. 추출 후 확인

```bash
# 파일 수 확인
echo "COBOL: $(ls src/cobol/ | wc -l) 파일"
echo "COPYBOOK: $(ls src/copybook/ | wc -l) 파일"
echo "JCL: $(ls src/jcl/ | wc -l) 파일"

# 인코딩 확인 (UTF-8이어야 함)
file src/cobol/* | head -5

# 내용 확인 (COBOL 형식이 맞는지)
head -20 src/cobol/$(ls src/cobol/ | head -1)
```

### 인코딩 문제 해결

```bash
# EBCDIC → UTF-8 변환 (iconv)
for f in src/cobol/*; do
    iconv -f EBCDIC-US -t UTF-8 "$f" -o "${f}.tmp" && mv "${f}.tmp" "$f"
done
```

---

## 4. 확장자 정리

메인프레임에서 추출하면 확장자가 없을 수 있습니다.
CATM은 확장자 없는 파일도 처리하지만, 정리하면 관리가 편합니다.

```bash
# COBOL 소스에 .cbl 확장자 추가
cd src/cobol
for f in *; do
    [ -f "$f" ] && [ "${f##*.}" = "$f" ] && mv "$f" "$f.cbl"
done

# COPYBOOK에 .cpy 확장자 추가
cd ../copybook
for f in *; do
    [ -f "$f" ] && [ "${f##*.}" = "$f" ] && mv "$f" "$f.cpy"
done

# JCL에 .jcl 확장자 추가
cd ../jcl
for f in *; do
    [ -f "$f" ] && [ "${f##*.}" = "$f" ] && mv "$f" "$f.jcl"
done
```
