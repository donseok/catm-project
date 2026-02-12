#!/usr/bin/env python3
"""
06_prioritize.py - 모더나이제이션 우선순위 자동 산정

정적 분석 결과 + Claude Code 비즈니스 로직 해석을 종합하여
프로그램별 전환 우선순위를 산정합니다.

⚡ Claude Code Max 20x 구독 사용

사용법:
    python catm/scripts/06_prioritize.py
"""

import sys
import os
import json
import re
import time

# pip install -e . 로 설치되지 않은 환경을 위한 폴백
if __name__ == "__main__":
    sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.dirname(__file__))))

from datetime import datetime
from catm.utils.file_utils import (
    load_config, load_json, save_markdown, save_json, ensure_dir
)
from catm.utils.claude_client import call_claude
from catm.utils.logger import get_logger

logger = get_logger("scripts.06_prioritize")


def calculate_static_scores(programs: list[dict], config: dict) -> list[dict]:
    """정적 분석 데이터 기반 자동 점수 산정"""
    
    weights = config["priority_weights"]
    thresholds = config["static_analysis"]["complexity_threshold"]
    
    # 전체 통계 (상대 점수 계산용)
    all_complexity = [p["complexity"] for p in programs]
    max_complexity = max(all_complexity) if all_complexity else 1
    
    all_deps = [len(p.get("calls", [])) + len(p.get("copies", [])) for p in programs]
    max_deps = max(all_deps) if all_deps else 1
    
    scored = []
    
    for pgm in programs:
        name = pgm["name"]
        
        # --- 기술 복잡도 (1-10) ---
        complexity_raw = pgm.get("complexity", 1)
        tech_score = min(10, round(complexity_raw / max_complexity * 10, 1))
        
        # --- 의존성 영향도 (1-10) ---
        dep_count = len(pgm.get("calls", [])) + len(pgm.get("copies", []))
        dep_score = min(10, round(dep_count / max_deps * 10, 1))
        
        # --- 전환 용이도 (1-10, 높을수록 쉬움) ---
        ease = 8  # 기본값
        if pgm.get("has_cics"):
            ease -= 2  # CICS는 전환 어려움
        if pgm.get("has_db2"):
            ease -= 1  # DB2는 약간 어려움
        if complexity_raw > thresholds["high"]:
            ease -= 2
        elif complexity_raw > thresholds["medium"]:
            ease -= 1
        ease = max(1, min(10, ease))
        
        # --- 비즈니스 중요도 (Claude에게 위임, 여기서는 기본값) ---
        # 이 값은 Claude Code가 분석 결과를 보고 조정함
        biz_score = 5  # 기본값 (Claude가 나중에 수정)
        
        # --- 최종 점수 ---
        final = round(
            biz_score * weights["business_importance"]
            + tech_score * weights["technical_complexity"]
            + dep_score * weights["dependency_impact"]
            + ease * weights["conversion_ease"],
            2
        )
        
        # 전환 방식 자동 판별
        if ease >= 7 and complexity_raw < thresholds["medium"]:
            method = "자동 변환"
        elif pgm.get("has_cics") or complexity_raw > thresholds["high"]:
            method = "리라이트"
        else:
            method = "하이브리드"
        
        scored.append({
            "name": name,
            "scores": {
                "business_importance": biz_score,
                "technical_complexity": tech_score,
                "dependency_impact": dep_score,
                "conversion_ease": ease,
                "final": final,
            },
            "metrics": {
                "line_count": pgm.get("line_count", 0),
                "complexity": complexity_raw,
                "calls": len(pgm.get("calls", [])),
                "copies": len(pgm.get("copies", [])),
                "db2_tables": len(pgm.get("db2_tables", [])),
                "has_cics": pgm.get("has_cics", False),
                "has_db2": pgm.get("has_db2", False),
            },
            "recommendation": {
                "method": method,
                "phase": "",  # Claude가 결정
            },
        })
    
    # 최종 점수로 정렬 (내림차순)
    scored.sort(key=lambda x: x["scores"]["final"], reverse=True)
    
    return scored


def call_claude_for_priority(scored_data: list[dict], config: dict) -> str:
    """Claude Code에게 비즈니스 중요도 판단 + Phase 배정 요청"""
    
    prompt_path = os.path.join(config["paths"]["prompts_dir"], "prioritize.md")
    with open(prompt_path, "r", encoding="utf-8") as f:
        template = f.read()
    
    # 정적 분석 요약을 JSON으로 주입
    summary_json = json.dumps(scored_data, indent=2, ensure_ascii=False)
    prompt = template.replace("{{STATIC_ANALYSIS_JSON}}", summary_json)
    
    # 비즈니스 로직 문서가 있으면 함께 제공
    docs_dir = os.path.join(config["paths"]["output_root"], "docs")
    logic_summaries = ""
    if os.path.exists(docs_dir):
        for md_file in sorted(os.listdir(docs_dir)):
            if md_file.endswith(".md"):
                fpath = os.path.join(docs_dir, md_file)
                with open(fpath, "r", encoding="utf-8") as f:
                    # 첫 80줄 (개요 + 비즈니스 규칙 + MES 관련도)
                    content = "\n".join(f.readlines()[:80])
                logic_summaries += f"\n### {md_file}\n{content}\n"
    
    prompt = prompt.replace("{{BUSINESS_LOGIC_SUMMARY}}", logic_summaries or "(아직 생성되지 않음)")
    prompt = prompt.replace("{{DEPENDENCY_GRAPH}}", "(dependency-scan.json 참조)")
    
    # Claude Code CLI 호출 (Max 20x)
    return call_claude(
        prompt,
        cli_command=config["claude"].get("cli_command", "claude"),
        timeout=config["claude"].get("timeout_per_program", 300) * 2,
        max_retries=config["claude"].get("max_retries", 3),
        retry_delay=config["claude"].get("retry_delay_seconds", 30),
    )


def parse_claude_scores(claude_response: str) -> dict:
    """Claude 응답에서 비즈니스 중요도 JSON을 파싱

    다중 JSON 블록이 있을 경우 순차적으로 시도합니다.

    Returns:
        {"PGM001": {"business_importance": 8, "phase": 1, "reason": "..."}, ...}
    """
    # ```json ... ``` 블록 모두 추출
    json_blocks = re.findall(r"```json\s*\n(.*?)```", claude_response, re.DOTALL)
    if not json_blocks:
        logger.warning("Claude 응답에서 JSON 블록을 찾을 수 없음")
        return {}

    for i, block in enumerate(json_blocks):
        try:
            data = json.loads(block.strip())
            scores = data.get("scores", {})
            if scores:
                logger.info("JSON 블록 %d/%d에서 scores 파싱 성공", i + 1, len(json_blocks))
                return scores
        except json.JSONDecodeError as e:
            logger.warning("JSON 블록 %d/%d 파싱 실패: %s", i + 1, len(json_blocks), e)
            continue

    logger.warning("모든 JSON 블록에서 scores 키를 찾지 못함")
    return {}


def apply_claude_scores(scored: list[dict], claude_scores: dict, config: dict) -> list[dict]:
    """Claude가 평가한 비즈니스 중요도를 반영하여 최종 점수 재계산"""
    weights = config["priority_weights"]

    for item in scored:
        name = item["name"]
        if name in claude_scores:
            cs = claude_scores[name]
            biz = cs.get("business_importance", item["scores"]["business_importance"])
            # 1-10 범위 보정
            biz = max(1, min(10, biz))
            item["scores"]["business_importance"] = biz

            # 최종 점수 재계산
            sc = item["scores"]
            sc["final"] = round(
                biz * weights["business_importance"]
                + sc["technical_complexity"] * weights["technical_complexity"]
                + sc["dependency_impact"] * weights["dependency_impact"]
                + sc["conversion_ease"] * weights["conversion_ease"],
                2
            )

            # Phase 반영
            phase = cs.get("phase")
            if phase:
                item["recommendation"]["phase"] = f"Phase {phase}"

    # 재정렬 (최종 점수 내림차순)
    scored.sort(key=lambda x: x["scores"]["final"], reverse=True)
    return scored


def generate_priority_report(scored: list[dict]) -> str:
    """우선순위 매트릭스 마크다운 보고서"""
    
    now = datetime.now().strftime("%Y-%m-%d %H:%M")
    
    # Phase 분류
    phase1 = [s for s in scored if s["scores"]["final"] >= 7]
    phase2 = [s for s in scored if 4 <= s["scores"]["final"] < 7]
    phase3 = [s for s in scored if s["scores"]["final"] < 4]
    
    md = f"""# 모더나이제이션 우선순위 매트릭스

> 생성일: {now}
> 생성 도구: CATM (Claude Code Max 20x)

---

## 1. 종합 대시보드

| Phase | 프로그램 수 | 총 라인 수 | 전환 방식 |
|-------|-----------|-----------|----------|
| Phase 1 (즉시) | {len(phase1)} | {sum(s['metrics']['line_count'] for s in phase1):,} | 리라이트 우선 |
| Phase 2 (3개월) | {len(phase2)} | {sum(s['metrics']['line_count'] for s in phase2):,} | 하이브리드 |
| Phase 3 (6개월) | {len(phase3)} | {sum(s['metrics']['line_count'] for s in phase3):,} | 자동 변환 |
| **합계** | **{len(scored)}** | **{sum(s['metrics']['line_count'] for s in scored):,}** | |

---

## 2. 전체 우선순위 (점수 내림차순)

| 순위 | 프로그램 | 최종점수 | 비즈니스 | 복잡도 | 영향도 | 용이도 | 전환방식 | CICS | DB2 |
|------|---------|---------|---------|--------|--------|--------|---------|------|-----|
"""
    
    for i, s in enumerate(scored, 1):
        sc = s["scores"]
        m = s["metrics"]
        md += (
            f"| {i} | {s['name']} | **{sc['final']:.1f}** | "
            f"{sc['business_importance']} | {sc['technical_complexity']:.1f} | "
            f"{sc['dependency_impact']:.1f} | {sc['conversion_ease']} | "
            f"{s['recommendation']['method']} | "
            f"{'✅' if m['has_cics'] else ''} | {'✅' if m['has_db2'] else ''} |\n"
        )
    
    md += """
---

## 3. 가중치 기준

| 지표 | 가중치 | 설명 |
|------|--------|------|
| 비즈니스 중요도 | 35% | MES 직접 관련도, 핵심 업무 여부 |
| 기술 복잡도 | 25% | McCabe 복잡도, 라인 수 |
| 의존성 영향도 | 20% | CALL/COPY 연결 수, 파급 범위 |
| 전환 용이도 | 20% | CICS/DB2 의존성, 표준 패턴 여부 |

## 4. 전환 방식 가이드

| 방식 | 조건 | 장점 | 단점 |
|------|------|------|------|
| 자동 변환 | 단순 로직, 높은 용이도 | 빠름, 비용 낮음 | 코드 품질 이슈 |
| 하이브리드 | 중간 복잡도 | 균형 잡힌 접근 | 관리 복잡 |
| 리라이트 | 높은 복잡도, CICS 사용 | 최고 품질 | 시간/비용 높음 |

---

> ⚡ 비즈니스 중요도는 Claude Code가 분석 문서를 기반으로 자동 평가합니다.
> JSON 파싱 실패 시 기본값(5)이 사용됩니다.
"""
    
    return md


def main():
    logger.info("=" * 60)
    logger.info("CATM Step 6: 모더나이제이션 우선순위 산정")
    logger.info("Claude Code Max 20x 구독 사용")
    logger.info("=" * 60)

    config = load_config()
    output_root = config["paths"]["output_root"]

    scan_path = os.path.join(output_root, "reports", "dependency-scan.json")
    if not os.path.exists(scan_path):
        logger.error("%s이 없습니다. 먼저 02를 실행하세요.", scan_path)
        return

    scan_data = load_json(scan_path)
    programs = scan_data.get("programs", [])

    if not programs:
        logger.error("분석된 프로그램이 없습니다.")
        return

    logger.info("정적 분석 기반 점수 산정: %d개 프로그램", len(programs))
    scored = calculate_static_scores(programs, config)
    
    # 2. Claude Code에게 비즈니스 중요도 판단 요청
    logger.info("Claude Code에게 비즈니스 중요도 평가 요청 중...")
    claude_response = call_claude_for_priority(scored, config)
    
    # Claude 응답 저장
    claude_md_path = os.path.join(output_root, "reports", "claude_priority_analysis.md")
    save_markdown(claude_response, claude_md_path)

    # 2-1. Claude 응답에서 비즈니스 중요도 파싱 및 반영
    claude_scores = parse_claude_scores(claude_response)
    if claude_scores:
        logger.info("Claude 비즈니스 중요도 파싱 성공: %d개 프로그램", len(claude_scores))
        scored = apply_claude_scores(scored, claude_scores, config)
    else:
        logger.warning("Claude 응답에서 JSON 점수를 파싱하지 못했습니다. 기본값(5) 사용")

    # 3. 보고서 생성
    logger.info("우선순위 매트릭스 보고서 생성 중...")
    report = generate_priority_report(scored)
    report_path = os.path.join(output_root, "reports", "priority_matrix.md")
    save_markdown(report, report_path)
    
    # JSON 데이터 저장
    json_path = os.path.join(output_root, "reports", "priority_data.json")
    save_json({"scored_programs": scored}, json_path)
    
    logger.info("=" * 60)
    logger.info("우선순위 산정 완료!")
    logger.info("매트릭스: %s", report_path)
    logger.info("Claude 분석: %s", claude_md_path)
    logger.info("데이터: %s", json_path)
    logger.info("=" * 60)


if __name__ == "__main__":
    main()
