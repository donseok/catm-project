"""
cobol_parser.py - COBOL 소스코드 공통 파서 유틸리티

IBM 메인프레임 COBOL 고정 형식(Fixed Format) 기준
- 컬럼 1-6: 시퀀스 번호
- 컬럼 7: 표시자 (* = 주석, - = 연속, / = 페이지넘김)
- 컬럼 8-72: 코드 영역 (A영역: 8-11, B영역: 12-72)
- 컬럼 73-80: 식별 영역 (무시)
"""

import re
from dataclasses import dataclass, field
from typing import Optional
from pathlib import Path


@dataclass
class CobolField:
    """COBOL 데이터 필드 (DATA DIVISION)"""
    level: int
    name: str
    picture: str = ""
    usage: str = ""
    occurs: int = 0
    redefines: str = ""
    value: str = ""
    line_number: int = 0

    @property
    def data_type(self) -> str:
        """PIC 절 → 한글 데이터 타입 변환"""
        if not self.picture:
            return "GROUP"
        pic = self.picture.upper().replace(" ", "")
        if "X" in pic:
            return f"문자열({self._calc_length(pic)})"
        if "S" in pic and "V" in pic:
            return f"부호숫자({self._int_len(pic)}.{self._dec_len(pic)})"
        if "V" in pic:
            return f"숫자({self._int_len(pic)}.{self._dec_len(pic)})"
        if "9" in pic:
            return f"숫자({self._calc_length(pic)})"
        return pic

    def _calc_length(self, pic: str) -> int:
        match = re.search(r"[X9]\((\d+)\)", pic)
        if match:
            return int(match.group(1))
        return len(re.findall(r"[X9]", pic))

    def _int_len(self, pic: str) -> int:
        before_v = pic.split("V")[0].replace("S", "")
        return self._calc_length(before_v)

    def _dec_len(self, pic: str) -> int:
        if "V" not in pic:
            return 0
        after_v = pic.split("V")[1]
        return self._calc_length(after_v)


@dataclass
class CobolProgram:
    """분석된 COBOL 프로그램 정보"""
    name: str
    file_path: str
    line_count: int = 0
    
    # IDENTIFICATION DIVISION
    program_id: str = ""
    author: str = ""
    
    # 의존성
    calls: list = field(default_factory=list)         # CALL 'PGM'
    copies: list = field(default_factory=list)        # COPY CPYBOOK
    db2_tables: list = field(default_factory=list)    # SQL 테이블
    vsam_files: list = field(default_factory=list)    # SELECT ASSIGN
    cics_maps: list = field(default_factory=list)     # SEND MAP
    
    # 메트릭
    complexity: int = 0          # McCabe 근사치
    paragraph_count: int = 0     # PARAGRAPH 수
    has_cics: bool = False       # CICS 사용 여부
    has_db2: bool = False        # DB2 사용 여부
    has_vsam: bool = False       # VSAM 사용 여부
    
    # 데이터 구조
    data_fields: list = field(default_factory=list)   # CobolField 리스트


def clean_cobol_line(line: str) -> str:
    """COBOL 고정 형식 라인에서 코드 영역만 추출"""
    if len(line) < 7:
        return ""
    
    # 컬럼 7이 * 또는 / 이면 주석
    indicator = line[6] if len(line) > 6 else " "
    if indicator in ("*", "/"):
        return ""
    
    # 컬럼 8-72 추출 (0-based: index 7~71)
    code = line[7:72] if len(line) > 7 else ""
    return code.rstrip()


def extract_code_lines(source: str) -> list[str]:
    """소스 파일 → 코드 라인만 추출 (주석/시퀀스 제거)"""
    lines = []
    for line in source.split("\n"):
        cleaned = clean_cobol_line(line)
        if cleaned:
            lines.append(cleaned)
    return lines


def extract_program_id(source: str) -> str:
    """PROGRAM-ID 추출"""
    match = re.search(
        r"PROGRAM-ID\.\s*([A-Z0-9_-]+)",
        source, re.IGNORECASE
    )
    return match.group(1).upper() if match else ""


def extract_calls(source: str) -> list[str]:
    """CALL 문 추출 (정적 + 동적)"""
    patterns = [
        r"CALL\s+'([A-Z0-9_-]+)'",          # CALL 'PGMNAME'
        r'CALL\s+"([A-Z0-9_-]+)"',          # CALL "PGMNAME"
        r"CALL\s+([A-Z0-9-]+)\s+USING",     # CALL WS-VAR USING (동적)
    ]
    calls = []
    for pattern in patterns:
        calls.extend(re.findall(pattern, source, re.IGNORECASE))
    return sorted(set(c.upper() for c in calls))


def extract_copies(source: str) -> list[str]:
    """COPY 문 추출"""
    pattern = r"COPY\s+([A-Z0-9_-]+)"
    results = re.findall(pattern, source, re.IGNORECASE)
    return sorted(set(r.upper() for r in results))


def extract_db2_tables(source: str) -> list[str]:
    """EXEC SQL 블록 내 DB2 테이블명 추출"""
    sql_blocks = re.findall(
        r"EXEC\s+SQL(.*?)END-EXEC",
        source, re.IGNORECASE | re.DOTALL
    )
    
    table_patterns = [
        r"\bFROM\s+([A-Z][A-Z0-9_.]+)",
        r"\bINTO\s+([A-Z][A-Z0-9_.]+)",
        r"\bUPDATE\s+([A-Z][A-Z0-9_.]+)",
        r"\bINSERT\s+INTO\s+([A-Z][A-Z0-9_.]+)",
        r"\bDELETE\s+FROM\s+([A-Z][A-Z0-9_.]+)",
    ]
    
    tables = []
    for block in sql_blocks:
        for pattern in table_patterns:
            matches = re.findall(pattern, block, re.IGNORECASE)
            # COBOL 호스트 변수(:VAR) 제외
            tables.extend(
                m.upper() for m in matches
                if not m.startswith(":") and not m.startswith("WS-")
            )
    
    return sorted(set(tables))


def extract_file_assigns(source: str) -> list[str]:
    """SELECT ... ASSIGN TO 파일명 추출"""
    pattern = r"ASSIGN\s+TO\s+([A-Z0-9_-]+)"
    results = re.findall(pattern, source, re.IGNORECASE)
    return sorted(set(r.upper() for r in results))


def extract_cics_maps(source: str) -> list[str]:
    """CICS SEND MAP / RECEIVE MAP 추출"""
    pattern = r"(?:SEND|RECEIVE)\s+MAP\s*\(\s*'([A-Z0-9_-]+)'\s*\)"
    results = re.findall(pattern, source, re.IGNORECASE)
    return sorted(set(r.upper() for r in results))


def calculate_complexity(source: str) -> int:
    """McCabe 순환복잡도 근사 계산"""
    decision_keywords = [
        r"\bIF\b", r"\bELSE\b", r"\bEVALUATE\b", r"\bWHEN\b",
        r"\bPERFORM\s+UNTIL\b", r"\bPERFORM\s+VARYING\b",
        r"\bAT\s+END\b", r"\bINVALID\s+KEY\b",
    ]
    complexity = 1  # 기본 경로
    for keyword in decision_keywords:
        complexity += len(re.findall(keyword, source, re.IGNORECASE))
    return complexity


def count_paragraphs(source: str) -> int:
    """PROCEDURE DIVISION 내 PARAGRAPH 수 카운트"""
    code_lines = extract_code_lines(source)

    # PROCEDURE DIVISION 이후만 대상
    in_procedure = False
    count = 0
    # A영역(컬럼 8-11)에서 시작하고 .으로 끝나는 라인 = PARAGRAPH 또는 SECTION
    pattern = re.compile(r"^[A-Z0-9_-]+\s*(?:SECTION)?\s*\.", re.IGNORECASE)
    for line in code_lines:
        if not in_procedure:
            if re.match(r"^PROCEDURE\s+DIVISION", line, re.IGNORECASE):
                in_procedure = True
            continue
        if pattern.match(line):
            count += 1
    return count


def parse_copybook_fields(source: str) -> list[CobolField]:
    """COPYBOOK 소스 → CobolField 리스트"""
    fields = []

    # 일반 필드 (레벨 01-49, 66, 77)
    pattern = (
        r"^\s*(\d{2})\s+([\w-]+)"
        r"(?:\s+PIC(?:TURE)?\s+(?:IS\s+)?([\w(),.VSP+-]+))?"
        r"(?:\s+(?:USAGE\s+(?:IS\s+)?)?(COMP(?:-[0-9])?|BINARY|PACKED-DECIMAL|DISPLAY|INDEX))?"
        r"(?:\s+OCCURS\s+(\d+)\s+TIMES?)?"
        r"(?:\s+REDEFINES\s+([\w-]+))?"
        r"(?:\s+VALUE\s+(?:IS\s+)?(.+?))?"
        r"\s*\."
    )

    # 88 레벨 조건명
    pattern_88 = r"^\s*(88)\s+([\w-]+)\s+VALUE(?:S)?\s+(?:IS\s+|ARE\s+)?(.+?)\s*\."

    for i, line in enumerate(source.split("\n"), 1):
        cleaned = clean_cobol_line(line)
        if not cleaned:
            continue

        # 88 레벨 조건명 매칭 (별도 처리)
        match_88 = re.match(pattern_88, cleaned, re.IGNORECASE)
        if match_88:
            g = match_88.groups()
            fields.append(CobolField(
                level=88,
                name=g[1].upper(),
                value=g[2].strip().strip("'\""),
                line_number=i,
            ))
            continue

        match = re.match(pattern, cleaned, re.IGNORECASE)
        if match:
            g = match.groups()
            fields.append(CobolField(
                level=int(g[0]),
                name=g[1].upper(),
                picture=(g[2] or "").strip().upper(),
                usage=(g[3] or "").strip().upper(),
                occurs=int(g[4]) if g[4] else 0,
                redefines=(g[5] or "").strip().upper(),
                value=(g[6] or "").strip(),
                line_number=i,
            ))

    return fields


def analyze_program(file_path: str) -> CobolProgram:
    """단일 COBOL 프로그램 종합 분석"""
    path = Path(file_path)
    
    with open(file_path, "r", encoding="utf-8", errors="ignore") as f:
        source = f.read()
    
    lines = source.split("\n")
    
    program = CobolProgram(
        name=path.stem.upper(),
        file_path=str(path),
        line_count=len(lines),
        program_id=extract_program_id(source),
        calls=extract_calls(source),
        copies=extract_copies(source),
        db2_tables=extract_db2_tables(source),
        vsam_files=extract_file_assigns(source),
        cics_maps=extract_cics_maps(source),
        complexity=calculate_complexity(source),
        paragraph_count=count_paragraphs(source),
    )
    
    # 플래그 설정
    source_upper = source.upper()
    program.has_cics = "EXEC CICS" in source_upper
    program.has_db2 = "EXEC SQL" in source_upper
    program.has_vsam = len(program.vsam_files) > 0
    
    return program


def read_source_file(file_path: str) -> str:
    """COBOL 소스 파일 읽기 (인코딩 자동 감지)"""
    try:
        with open(file_path, "r", encoding="utf-8") as f:
            return f.read()
    except UnicodeDecodeError:
        # EBCDIC이나 다른 인코딩일 수 있음
        try:
            import chardet
            with open(file_path, "rb") as f:
                raw = f.read()
            detected = chardet.detect(raw)
            encoding = detected.get("encoding", "latin-1")
            return raw.decode(encoding, errors="replace")
        except ImportError:
            with open(file_path, "r", encoding="latin-1") as f:
                return f.read()
