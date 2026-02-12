#!/usr/bin/env python3
"""
01_scan_inventory.py - COBOL 소스코드 인벤토리 스캔

메인프레임에서 추출한 소스코드의 전체 규모를 파악합니다.
- 프로그램 수, 총 라인 수
- COPYBOOK, JCL, DCLGEN 수
- 기본 통계 (최대/최소/평균 라인 수)

사용법:
    python catm/scripts/01_scan_inventory.py
"""

import sys
import os

# pip install -e . 로 설치되지 않은 환경을 위한 폴백
if __name__ == "__main__":
    sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.dirname(__file__))))

from pathlib import Path
from datetime import datetime
from catm.utils.file_utils import load_config, save_json, save_markdown, find_files
from catm.utils.logger import get_logger

logger = get_logger("scripts.01_scan_inventory")


def scan_directory(dir_path: str, extensions: list[str]) -> list[dict]:
    """디렉토리 내 파일 목록 + 라인 수 스캔"""
    files = find_files(dir_path, extensions)
    results = []
    
    for f in files:
        try:
            with open(f, "r", encoding="utf-8", errors="ignore") as fp:
                lines = fp.readlines()
            results.append({
                "name": f.stem.upper(),
                "file": str(f),
                "lines": len(lines),
                "size_kb": round(f.stat().st_size / 1024, 1),
            })
        except Exception as e:
            results.append({
                "name": f.stem.upper(),
                "file": str(f),
                "lines": 0,
                "size_kb": 0,
                "error": str(e),
            })
    
    return results


def generate_inventory_report(inventory: dict) -> str:
    """인벤토리 마크다운 보고서 생성"""
    now = datetime.now().strftime("%Y-%m-%d %H:%M")
    
    md = f"""# COBOL 소스코드 인벤토리 보고서

> 생성일: {now}
> 생성 도구: CATM (COBOL Analysis Task Manager)

---

## 1. 전체 규모 요약

| 구분 | 파일 수 | 총 라인 수 | 총 크기 |
|------|---------|-----------|---------|
"""
    
    for category, data in inventory["categories"].items():
        files = data["files"]
        total_lines = sum(f["lines"] for f in files)
        total_size = sum(f["size_kb"] for f in files)
        md += f"| {category} | {len(files)} | {total_lines:,} | {total_size:,.1f} KB |\n"
    
    # 전체 합계
    all_files = []
    for data in inventory["categories"].values():
        all_files.extend(data["files"])
    
    grand_lines = sum(f["lines"] for f in all_files)
    grand_size = sum(f["size_kb"] for f in all_files)
    md += f"| **합계** | **{len(all_files)}** | **{grand_lines:,}** | **{grand_size:,.1f} KB** |\n"
    
    # COBOL 프로그램 상세
    cobol_files = inventory["categories"].get("COBOL 프로그램", {}).get("files", [])
    if cobol_files:
        lines_list = [f["lines"] for f in cobol_files]
        if not lines_list:
            lines_list = [0]
        md += f"""
## 2. COBOL 프로그램 통계

| 지표 | 값 |
|------|-----|
| 프로그램 수 | {len(cobol_files)} |
| 총 라인 수 | {sum(lines_list):,} |
| 평균 라인 수 | {sum(lines_list) // len(lines_list):,} |
| 최대 라인 수 | {max(lines_list):,} |
| 최소 라인 수 | {min(lines_list):,} |

## 3. COBOL 프로그램 목록 (라인 수 내림차순)

| # | 프로그램명 | 라인 수 | 크기 |
|---|-----------|---------|------|
"""
        for i, f in enumerate(sorted(cobol_files, key=lambda x: x["lines"], reverse=True), 1):
            md += f"| {i} | {f['name']} | {f['lines']:,} | {f['size_kb']:.1f} KB |\n"
    
    # COPYBOOK 목록
    cpy_files = inventory["categories"].get("COPYBOOK", {}).get("files", [])
    if cpy_files:
        md += f"""
## 4. COPYBOOK 목록

| # | COPYBOOK명 | 라인 수 |
|---|-----------|---------|
"""
        for i, f in enumerate(sorted(cpy_files, key=lambda x: x["name"]), 1):
            md += f"| {i} | {f['name']} | {f['lines']:,} |\n"
    
    md += """
---

## 다음 단계

이 인벤토리를 기반으로 다음 분석을 진행합니다:
1. `02_extract_dependencies.py` → 의존성 추출
2. `05_analyze_with_claude.py` → Claude Code 비즈니스 로직 분석
"""
    
    return md


def main():
    logger.info("=" * 50)
    logger.info("CATM Step 1: 소스코드 인벤토리 스캔")
    logger.info("=" * 50)
    
    config = load_config()
    source_root = config["paths"]["source_root"]
    output_root = config["paths"]["output_root"]
    
    # 카테고리별 스캔
    categories = {
        "COBOL 프로그램": {
            "dir": os.path.join(source_root, config["source_dirs"]["cobol"]),
            "ext": config["file_extensions"]["cobol"],
        },
        "COPYBOOK": {
            "dir": os.path.join(source_root, config["source_dirs"]["copybook"]),
            "ext": config["file_extensions"]["copybook"],
        },
        "JCL": {
            "dir": os.path.join(source_root, config["source_dirs"]["jcl"]),
            "ext": config["file_extensions"]["jcl"],
        },
        "PROC": {
            "dir": os.path.join(source_root, config["source_dirs"]["proc"]),
            "ext": config["file_extensions"]["proc"],
        },
        "DCLGEN": {
            "dir": os.path.join(source_root, "dclgen"),
            "ext": ["", ".dclgen"],
        },
        "DB2 DDL": {
            "dir": os.path.join(source_root, "ddl"),
            "ext": ["", ".ddl", ".sql"],
        },
    }
    
    inventory = {"categories": {}, "scan_date": datetime.now().isoformat()}
    
    for cat_name, cat_info in categories.items():
        logger.info("스캔 중: %s (%s)", cat_name, cat_info['dir'])
        files = scan_directory(cat_info["dir"], cat_info["ext"])
        inventory["categories"][cat_name] = {"files": files}
        logger.info("  → %d개 파일, %s 라인", len(files), f"{sum(f['lines'] for f in files):,}")
    
    # JSON 저장
    json_path = os.path.join(output_root, "reports", "inventory.json")
    save_json(inventory, json_path)
    
    # 마크다운 보고서 저장
    report = generate_inventory_report(inventory)
    md_path = os.path.join(output_root, "reports", "01_inventory_report.md")
    save_markdown(report, md_path)
    
    # 요약 출력
    all_files = []
    for data in inventory["categories"].values():
        all_files.extend(data["files"])
    
    logger.info("=" * 50)
    logger.info("스캔 완료! 총 %d개 파일, %s 라인", len(all_files), f"{sum(f['lines'] for f in all_files):,}")
    logger.info("결과: %s", md_path)
    logger.info("=" * 50)


if __name__ == "__main__":
    main()
