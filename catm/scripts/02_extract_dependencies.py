#!/usr/bin/env python3
"""
02_extract_dependencies.py - COBOL 의존성 추출

정적 분석으로 다음을 추출합니다:
- CALL 관계 (프로그램 → 프로그램)
- COPY 관계 (프로그램 → COPYBOOK)
- DB2 테이블 참조 (프로그램 → DB2)
- VSAM 파일 참조 (프로그램 → VSAM)
- JCL 실행 순서 (JOB → STEP → PGM)
- McCabe 복잡도 (근사치)

사용법:
    python catm/scripts/02_extract_dependencies.py
"""

import sys
import os
import re

# pip install -e . 로 설치되지 않은 환경을 위한 폴백
if __name__ == "__main__":
    sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.dirname(__file__))))

from pathlib import Path
from datetime import datetime
from catm.utils.cobol_parser import analyze_program
from catm.utils.file_utils import load_config, save_json, find_files
from catm.utils.cross_reference import (
    build_cross_reference,
    build_impact_scores,
    detect_dead_code,
    serialize_cross_reference,
)
from catm.utils.logger import get_logger

logger = get_logger("scripts.02_extract_dependencies")


def analyze_jcl(file_path: str) -> dict:
    """JCL 파일 분석 - 배치 실행 순서 추출"""
    with open(file_path, "r", encoding="utf-8", errors="ignore") as f:
        source = f.read()
    
    job_name = Path(file_path).stem.upper()
    
    # EXEC PGM= 추출
    steps = []
    step_pattern = r"//(\w+)\s+EXEC\s+(?:PGM=)?(\w+)"
    for match in re.finditer(step_pattern, source, re.IGNORECASE):
        step_name, pgm_name = match.groups()
        steps.append({
            "step": step_name.upper(),
            "program": pgm_name.upper(),
        })
    
    # DD DSN= 추출
    datasets = []
    dd_pattern = r"//(\w+)\s+DD\s+(?:DSN|DSNAME)=([^,\s]+)"
    for match in re.finditer(dd_pattern, source, re.IGNORECASE):
        dd_name, dsn = match.groups()
        datasets.append({
            "dd": dd_name.upper(),
            "dsn": dsn,
        })
    
    return {
        "job_name": job_name,
        "file_path": file_path,
        "steps": steps,
        "datasets": datasets,
        "step_count": len(steps),
    }


def build_category_map(config: dict) -> dict[str, str]:
    """프로그램명 → 카테고리명 매핑 딕셔너리 생성"""
    cat_map: dict[str, str] = {}
    for cat in config.get("program_categories", []):
        for pgm in cat.get("programs", []):
            cat_map[pgm.upper()] = cat["name"]
    return cat_map


def main():
    logger.info("=" * 50)
    logger.info("CATM Step 2: 의존성 추출 (정적 분석)")
    logger.info("=" * 50)

    config = load_config()
    source_root = config["paths"]["source_root"]
    output_root = config["paths"]["output_root"]
    category_map = build_category_map(config)

    result = {
        "programs": [],
        "jcl_jobs": [],
        "summary": {},
        "categories_summary": [],
        "scan_date": datetime.now().isoformat(),
    }
    
    # --- COBOL 프로그램 분석 ---
    cobol_dir = os.path.join(source_root, config["source_dirs"]["cobol"])
    cobol_files = find_files(cobol_dir, config["file_extensions"]["cobol"])
    
    logger.info("COBOL 프로그램 분석: %d개", len(cobol_files))
    for f in cobol_files:
        try:
            pgm = analyze_program(str(f))
            result["programs"].append({
                "name": pgm.name,
                "file_path": pgm.file_path,
                "line_count": pgm.line_count,
                "program_id": pgm.program_id,
                "calls": pgm.calls,
                "copies": pgm.copies,
                "db2_tables": pgm.db2_tables,
                "vsam_files": pgm.vsam_files,
                "cics_maps": pgm.cics_maps,
                "complexity": pgm.complexity,
                "paragraph_count": pgm.paragraph_count,
                "has_cics": pgm.has_cics,
                "has_db2": pgm.has_db2,
                "has_vsam": pgm.has_vsam,
                "category": category_map.get(pgm.name, "미분류"),
            })
            logger.info("  %s - CALL:%d, COPY:%d, DB2:%d, 복잡도:%d",
                        pgm.name, len(pgm.calls), len(pgm.copies),
                        len(pgm.db2_tables), pgm.complexity)
        except Exception as e:
            logger.error("  %s - 에러: %s", f.stem.upper(), e)
    
    # --- JCL 분석 ---
    jcl_dir = os.path.join(source_root, config["source_dirs"]["jcl"])
    jcl_files = find_files(jcl_dir, config["file_extensions"]["jcl"])
    
    logger.info("JCL 분석: %d개", len(jcl_files))
    for f in jcl_files:
        try:
            jcl = analyze_jcl(str(f))
            result["jcl_jobs"].append(jcl)
            logger.info("  %s - %d steps", jcl['job_name'], jcl['step_count'])
        except Exception as e:
            logger.error("  %s - 에러: %s", f.stem.upper(), e)
    
    # --- 요약 통계 ---
    programs = result["programs"]
    result["summary"] = {
        "total_programs": len(programs),
        "total_lines": sum(p["line_count"] for p in programs),
        "total_jcl_jobs": len(result["jcl_jobs"]),
        "avg_complexity": (
            round(sum(p["complexity"] for p in programs) / len(programs), 1)
            if programs else 0
        ),
        "max_complexity": max((p["complexity"] for p in programs), default=0),
        "unique_copybooks": len(set(c for p in programs for c in p["copies"])),
        "unique_db2_tables": len(set(t for p in programs for t in p["db2_tables"])),
        "unique_vsam_files": len(set(v for p in programs for v in p["vsam_files"])),
        "cics_programs": sum(1 for p in programs if p["has_cics"]),
        "db2_programs": sum(1 for p in programs if p["has_db2"]),
        "vsam_programs": sum(1 for p in programs if p["has_vsam"]),
    }
    
    # --- 카테고리별 요약 ---
    cat_groups: dict[str, list] = {}
    for p in programs:
        cat = p.get("category", "미분류")
        cat_groups.setdefault(cat, []).append(p)

    result["categories_summary"] = [
        {
            "name": cat_name,
            "program_count": len(progs),
            "total_lines": sum(p["line_count"] for p in progs),
            "avg_complexity": (
                round(sum(p["complexity"] for p in progs) / len(progs), 1)
                if progs else 0
            ),
            "programs": [p["name"] for p in progs],
        }
        for cat_name, progs in cat_groups.items()
    ]

    # --- 교차 참조 / 영향도 / 데드 코드 ---
    logger.info("교차 참조 및 영향도 분석 중...")
    cross_ref = build_cross_reference(programs, result["jcl_jobs"])
    result["cross_reference"] = serialize_cross_reference(cross_ref)
    result["impact_scores"] = build_impact_scores(cross_ref, programs)

    dead = detect_dead_code(programs, cross_ref)
    result["dead_code"] = {
        "orphan_programs": dead.orphan_programs,
        "unused_copybooks": dead.unused_copybooks,
    }

    # 저장
    json_path = os.path.join(output_root, "reports", "dependency-scan.json")
    save_json(result, json_path)
    
    # 요약 출력
    s = result["summary"]
    logger.info("=" * 50)
    logger.info("의존성 추출 완료!")
    logger.info("프로그램: %d개 (%s 라인)", s['total_programs'], f"{s['total_lines']:,}")
    logger.info("JCL: %d개", s['total_jcl_jobs'])
    logger.info("COPYBOOK: %d개 (고유)", s['unique_copybooks'])
    logger.info("DB2 테이블: %d개 (고유)", s['unique_db2_tables'])
    logger.info("평균 복잡도: %s", s['avg_complexity'])
    logger.info("CICS 사용: %d개 / DB2 사용: %d개", s['cics_programs'], s['db2_programs'])
    logger.info("결과: %s", json_path)
    logger.info("=" * 50)


if __name__ == "__main__":
    main()
