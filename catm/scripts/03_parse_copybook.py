#!/usr/bin/env python3
"""
03_parse_copybook.py - COPYBOOK 파싱 → 데이터 사전 생성

COPYBOOK(.cpy) 파일을 파싱하여:
- 필드별 데이터 사전 (마크다운)
- ERD 다이어그램 (Mermaid)
을 자동 생성합니다.

사용법:
    python catm/scripts/03_parse_copybook.py
"""

import sys
import os

# pip install -e . 로 설치되지 않은 환경을 위한 폴백
if __name__ == "__main__":
    sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.dirname(__file__))))

from catm.utils.cobol_parser import parse_copybook_fields, read_source_file
from catm.utils.file_utils import (
    load_config, find_files, save_markdown
)
from catm.utils.mermaid_builder import build_erd
from catm.utils.logger import get_logger

logger = get_logger("scripts.03_parse_copybook")


def fields_to_markdown(copybook_name: str, fields: list) -> str:
    """CobolField 리스트 → 마크다운 데이터 사전"""
    md = f"""# 데이터 사전: {copybook_name}

> 자동 생성 by CATM (COBOL Analysis Task Manager)

## 필드 정의

| Lv | 필드명 | PIC | 데이터 타입 | OCCURS | REDEFINES | 기본값 |
|----|--------|-----|------------|--------|-----------|--------|
"""
    
    for f in fields:
        # 레벨에 따른 시각적 들여쓰기
        indent = ""
        if f.level >= 5:
            indent = "　" * ((f.level - 1) // 5)
        
        md += (
            f"| {f.level:02d} | {indent}{f.name} | "
            f"`{f.picture}` | {f.data_type} | "
            f"{f.occurs or ''} | {f.redefines} | "
            f"{f.value} |\n"
        )
    
    # 요약 통계
    group_count = sum(1 for f in fields if not f.picture)
    field_count = sum(1 for f in fields if f.picture)
    
    md += f"""
## 요약

| 항목 | 수 |
|------|-----|
| 그룹 항목 (GROUP) | {group_count} |
| 기본 항목 (ELEMENTARY) | {field_count} |
| 배열 (OCCURS) | {sum(1 for f in fields if f.occurs > 0)} |
| REDEFINES | {sum(1 for f in fields if f.redefines)} |
| 총 필드 수 | {len(fields)} |
"""
    
    return md


def main():
    logger.info("=" * 50)
    logger.info("CATM Step 3: COPYBOOK 파싱 → 데이터 사전")
    logger.info("=" * 50)

    config = load_config()
    source_root = config["paths"]["source_root"]
    output_root = config["paths"]["output_root"]

    cpy_dir = os.path.join(source_root, config["source_dirs"]["copybook"])
    cpy_files = find_files(cpy_dir, config["file_extensions"]["copybook"])

    logger.info("COPYBOOK 파싱: %d개", len(cpy_files))
    
    all_copybook_fields = {}  # ERD 생성용
    
    for f in cpy_files:
        name = f.stem.upper()
        try:
            source = read_source_file(str(f))
            fields = parse_copybook_fields(source)

            if fields:
                md = fields_to_markdown(name, fields)
                md_path = os.path.join(
                    output_root, "data-dict", f"{name}.md"
                )
                save_markdown(md, md_path)

                all_copybook_fields[name] = [
                    {"name": fld.name, "picture": fld.picture, "data_type": fld.data_type}
                    for fld in fields if fld.picture
                ]

                logger.info("  %s - %d 필드", name, len(fields))
            else:
                logger.warning("  %s - 필드 없음", name)

        except Exception as e:
            logger.error("  %s - 에러: %s", name, e)
    
    # ERD 다이어그램 생성
    if all_copybook_fields:
        erd = build_erd(all_copybook_fields)
        erd_path = os.path.join(output_root, "diagrams", "erd_copybooks.md")
        save_markdown(erd, erd_path)
    
    logger.info("=" * 50)
    logger.info("COPYBOOK 파싱 완료! 데이터 사전: %d개 생성", len(all_copybook_fields))
    logger.info("결과: %s/data-dict/", output_root)
    logger.info("=" * 50)


if __name__ == "__main__":
    main()
