"""
cross_reference.py - 교차 참조 및 영향 분석 모듈

dependency-scan.json의 순방향 의존성 데이터를 역방향으로 매핑하고
영향도(impact score)를 계산합니다.
"""

from dataclasses import dataclass, field
from typing import Any

from catm.utils.logger import get_logger

logger = get_logger("cross_reference")


@dataclass
class CrossReferenceIndex:
    """역방향 의존성 맵"""
    # COPYBOOK → 사용 프로그램 목록
    copybook_usage: dict[str, list[str]] = field(default_factory=dict)
    # DB2 테이블 → 사용 프로그램 목록
    db2_table_usage: dict[str, list[str]] = field(default_factory=dict)
    # 서브프로그램 → 호출하는 프로그램 목록
    called_by: dict[str, list[str]] = field(default_factory=dict)
    # VSAM 파일 → 사용 프로그램 목록
    vsam_usage: dict[str, list[str]] = field(default_factory=dict)
    # 프로그램 → 실행하는 JCL 목록
    program_executed_by_jcl: dict[str, list[str]] = field(default_factory=dict)


@dataclass
class DeadCodeReport:
    """데드 코드 탐지 결과"""
    orphan_programs: list[str] = field(default_factory=list)
    unused_copybooks: list[str] = field(default_factory=list)


def build_cross_reference(
    programs: list[dict],
    jcl_jobs: list[dict] | None = None,
) -> CrossReferenceIndex:
    """순방향 의존성 → 역방향 교차 참조 인덱스 생성

    Args:
        programs: dependency-scan.json의 programs 배열
        jcl_jobs: dependency-scan.json의 jcl_jobs 배열

    Returns:
        역방향 매핑이 완성된 CrossReferenceIndex
    """
    idx = CrossReferenceIndex()

    for pgm in programs:
        name = pgm["name"]

        for cpy in pgm.get("copies", []):
            idx.copybook_usage.setdefault(cpy, []).append(name)

        for table in pgm.get("db2_tables", []):
            idx.db2_table_usage.setdefault(table, []).append(name)

        for call in pgm.get("calls", []):
            idx.called_by.setdefault(call, []).append(name)

        for vsam in pgm.get("vsam_files", []):
            idx.vsam_usage.setdefault(vsam, []).append(name)

    # JCL → 프로그램 실행 매핑
    for job in (jcl_jobs or []):
        job_name = job.get("job_name", "")
        for step in job.get("steps", []):
            pgm_name = step.get("program", "")
            if pgm_name:
                idx.program_executed_by_jcl.setdefault(pgm_name, []).append(job_name)

    logger.info(
        "교차 참조 생성: COPYBOOK %d, DB2 %d, CALL %d, VSAM %d, JCL %d",
        len(idx.copybook_usage),
        len(idx.db2_table_usage),
        len(idx.called_by),
        len(idx.vsam_usage),
        len(idx.program_executed_by_jcl),
    )

    return idx


def get_impact_analysis(
    target: str,
    cross_ref: CrossReferenceIndex,
) -> dict[str, list[str]]:
    """특정 리소스 변경 시 영향받는 프로그램 분석

    Args:
        target: 조회 대상 (COPYBOOK명, 테이블명, 프로그램명 등)
        cross_ref: 교차 참조 인덱스

    Returns:
        {"copybook_users": [...], "call_callers": [...], ...}
    """
    result: dict[str, list[str]] = {}
    target_upper = target.upper()

    if target_upper in cross_ref.copybook_usage:
        result["copybook_users"] = cross_ref.copybook_usage[target_upper]
    if target_upper in cross_ref.db2_table_usage:
        result["db2_users"] = cross_ref.db2_table_usage[target_upper]
    if target_upper in cross_ref.called_by:
        result["callers"] = cross_ref.called_by[target_upper]
    if target_upper in cross_ref.vsam_usage:
        result["vsam_users"] = cross_ref.vsam_usage[target_upper]
    if target_upper in cross_ref.program_executed_by_jcl:
        result["jcl_jobs"] = cross_ref.program_executed_by_jcl[target_upper]

    return result


def build_impact_scores(
    cross_ref: CrossReferenceIndex,
    programs: list[dict],
) -> dict[str, float]:
    """리소스별 영향도 점수 산출 (0.0 ~ 1.0)

    총 프로그램 수 대비 해당 리소스를 사용하는 프로그램 비율.

    Args:
        cross_ref: 교차 참조 인덱스
        programs: 프로그램 목록

    Returns:
        {"CPYTRANS": 0.67, "TB_DAILY_PROD": 0.33, ...}
    """
    total = len(programs)
    if total == 0:
        return {}

    scores: dict[str, float] = {}

    # 모든 역방향 매핑에서 영향도 산출
    all_resources: dict[str, list[str]] = {}
    for resource_map in [
        cross_ref.copybook_usage,
        cross_ref.db2_table_usage,
        cross_ref.called_by,
        cross_ref.vsam_usage,
    ]:
        for name, users in resource_map.items():
            all_resources.setdefault(name, []).extend(users)

    for name, users in all_resources.items():
        unique_users = len(set(users))
        scores[name] = round(unique_users / total, 2)

    return scores


def detect_dead_code(
    programs: list[dict],
    cross_ref: CrossReferenceIndex,
) -> DeadCodeReport:
    """JCL/CALL에서 참조되지 않는 고아 프로그램/미사용 COPYBOOK 식별

    Args:
        programs: 프로그램 목록
        cross_ref: 교차 참조 인덱스

    Returns:
        DeadCodeReport (orphan_programs, unused_copybooks)
    """
    report = DeadCodeReport()

    # 고아 프로그램: 다른 프로그램에서 CALL되지 않고 JCL에서도 실행되지 않는 프로그램
    all_program_names = {p["name"] for p in programs}
    called_programs = set(cross_ref.called_by.keys())
    jcl_executed = set(cross_ref.program_executed_by_jcl.keys())
    referenced_programs = called_programs | jcl_executed

    for name in sorted(all_program_names):
        if name not in referenced_programs:
            report.orphan_programs.append(name)

    # 미사용 COPYBOOK: 인벤토리에는 있지만 어떤 프로그램도 COPY하지 않는 COPYBOOK
    # (프로그램들이 참조하는 COPYBOOK 목록과 대조)
    all_used_copybooks = set(cross_ref.copybook_usage.keys())
    all_defined_copybooks = set()
    for p in programs:
        for cpy in p.get("copies", []):
            all_defined_copybooks.add(cpy)
    # 사용되는 copybook인데 소스가 없는 것 = 외부 COPYBOOK (이건 데드코드 아님)
    # 여기서는 "정의되어 있지만 아무도 사용하지 않는" 경우를 탐지할 인벤토리 정보 필요
    # 인벤토리가 없으면 빈 리스트 반환 (Phase 3 범위)

    logger.info(
        "데드 코드 탐지: 고아 프로그램 %d개, 미사용 COPYBOOK %d개",
        len(report.orphan_programs),
        len(report.unused_copybooks),
    )

    return report


def detect_dead_code_with_inventory(
    programs: list[dict],
    cross_ref: CrossReferenceIndex,
    copybook_names: list[str],
) -> DeadCodeReport:
    """인벤토리 정보를 활용한 전체 데드 코드 탐지

    Args:
        programs: 프로그램 목록
        cross_ref: 교차 참조 인덱스
        copybook_names: 인벤토리의 전체 COPYBOOK 이름 목록

    Returns:
        DeadCodeReport
    """
    report = detect_dead_code(programs, cross_ref)

    # 미사용 COPYBOOK: 인벤토리에 있지만 어떤 프로그램도 참조하지 않는 것
    used_copybooks = set(cross_ref.copybook_usage.keys())
    for cpy_name in sorted(copybook_names):
        cpy_upper = cpy_name.upper()
        if cpy_upper not in used_copybooks:
            report.unused_copybooks.append(cpy_upper)

    logger.info(
        "데드 코드 탐지 (인벤토리 포함): 고아 프로그램 %d개, 미사용 COPYBOOK %d개",
        len(report.orphan_programs),
        len(report.unused_copybooks),
    )

    return report


def serialize_cross_reference(cross_ref: CrossReferenceIndex) -> dict:
    """CrossReferenceIndex → JSON 직렬화 가능한 딕셔너리"""
    return {
        "copybook_usage": cross_ref.copybook_usage,
        "db2_table_usage": cross_ref.db2_table_usage,
        "called_by": cross_ref.called_by,
        "vsam_usage": cross_ref.vsam_usage,
        "program_executed_by_jcl": cross_ref.program_executed_by_jcl,
    }
