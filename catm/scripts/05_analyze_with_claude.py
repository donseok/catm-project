#!/usr/bin/env python3
"""
05_analyze_with_claude.py - Claude Code (Max 20x 구독) 비즈니스 로직 분석

⚡ 이 스크립트는 Anthropic API가 아닌 Claude Code Max 구독을 사용합니다.
   → `claude` CLI를 subprocess로 호출하여 분석합니다.
   → API key 불필요, Max 20x 구독만 있으면 됩니다.

기능:
- COBOL 소스코드를 Claude Code에 전달
- 비즈니스 로직을 한국어로 해석/문서화
- COPYBOOK을 자동으로 인라인 확장하여 컨텍스트 제공
- 배치 처리로 여러 프로그램을 순차 분석
- rate limit 대응 (자동 재시도, 배치 간 대기)

사용법:
    # 전체 프로그램 분석
    python catm/scripts/05_analyze_with_claude.py

    # 단일 프로그램 분석
    python catm/scripts/05_analyze_with_claude.py --single src/cobol/PGM001.cbl

    # 특정 프로그램만 분석
    python catm/scripts/05_analyze_with_claude.py --names PGM001,PGM002,PGM003
"""

import sys
import os
import subprocess
import time
import argparse
import json

# pip install -e . 로 설치되지 않은 환경을 위한 폴백
if __name__ == "__main__":
    sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.dirname(__file__))))

from pathlib import Path
from datetime import datetime
from catm.utils.cobol_parser import extract_copies, read_source_file
from catm.utils.file_utils import (
    load_config, find_files, save_markdown, load_json, ensure_dir
)
from catm.utils.claude_client import call_claude, is_error_response


# ============================================================
#  Claude Code Max 20x 구독 호출 함수
# ============================================================

def call_claude_code(prompt: str, config: dict) -> str:
    """Claude Code CLI 호출 래퍼 (공통 모듈 위임)"""
    return call_claude(
        prompt,
        cli_command=config.get("cli_command", "claude"),
        timeout=config.get("timeout_per_program", 300),
        max_retries=config.get("max_retries", 3),
        retry_delay=config.get("retry_delay_seconds", 30),
    )


# ============================================================
#  프롬프트 빌더
# ============================================================

def build_analysis_prompt(
    program_name: str,
    cobol_source: str,
    copybook_contents: dict[str, str],
    prompt_template: str,
) -> str:
    """분석 프롬프트 조립"""
    
    # COPYBOOK 내용 조립
    copybook_section = ""
    if copybook_contents:
        copybook_section = "\n## 참조 COPYBOOK\n"
        for name, content in copybook_contents.items():
            copybook_section += f"\n### {name}\n```cobol\n{content}\n```\n"
    else:
        copybook_section = "\n## 참조 COPYBOOK\n(없음)\n"
    
    # 템플릿 변수 치환
    prompt = prompt_template.replace("{{PROGRAM_ID}}", program_name)
    prompt = prompt.replace("{{COBOL_SOURCE}}", cobol_source)
    prompt = prompt.replace("{{COPYBOOK_CONTENTS}}", copybook_section)
    
    return prompt


def load_copybooks(
    copies: list[str],
    copybook_dir: str,
    extensions: list[str],
) -> dict[str, str]:
    """프로그램이 참조하는 COPYBOOK 내용 로드"""
    contents = {}
    
    for cpy_name in copies:
        # 다양한 확장자로 검색
        for ext in extensions:
            candidates = [
                os.path.join(copybook_dir, f"{cpy_name}{ext}"),
                os.path.join(copybook_dir, f"{cpy_name.lower()}{ext}"),
            ]
            for path in candidates:
                if os.path.exists(path):
                    contents[cpy_name] = read_source_file(path)
                    break
            if cpy_name in contents:
                break
    
    return contents


# ============================================================
#  메인 분석 루프
# ============================================================

def analyze_single_program(
    cobol_file: str,
    config: dict,
    prompt_template: str,
    output_dir: str,
) -> dict:
    """단일 프로그램 Claude 분석"""
    
    pgm_name = Path(cobol_file).stem.upper()
    source = read_source_file(cobol_file)
    
    # COPYBOOK 참조 추출 및 로드
    copies = extract_copies(source)
    copybook_dir = os.path.join(
        config["paths"]["source_root"],
        config["source_dirs"]["copybook"]
    )
    copybook_contents = load_copybooks(
        copies, copybook_dir, config["file_extensions"]["copybook"]
    )
    
    # 프롬프트 조립
    prompt = build_analysis_prompt(
        pgm_name, source, copybook_contents, prompt_template
    )
    
    # Claude Code 호출 (Max 20x 구독)
    print(f"    🤖 Claude Code 분석 중: {pgm_name} ({len(source)} chars)", end="")
    
    start_time = time.time()
    response = call_claude_code(prompt, config["claude"])
    elapsed = time.time() - start_time
    
    # 결과 저장
    output_path = os.path.join(output_dir, f"{pgm_name}.md")
    save_markdown(response, output_path)
    
    print(f" ✅ ({elapsed:.1f}초)")
    
    return {
        "program": pgm_name,
        "status": "error" if is_error_response(response) else "success",
        "elapsed_seconds": round(elapsed, 1),
        "output_file": output_path,
        "copybooks_used": list(copybook_contents.keys()),
    }


def analyze_single_data(
    copybook_file: str,
    config: dict,
    prompt_template: str,
    output_dir: str,
) -> dict:
    """단일 COPYBOOK/DCLGEN Claude 데이터 구조 분석"""

    name = Path(copybook_file).stem.upper()
    source = read_source_file(copybook_file)

    # DDL 파일 검색 (있을 경우)
    ddl_dir = os.path.join(config["paths"]["source_root"], "ddl")
    ddl_source = ""
    for ext in [".ddl", ".sql", ""]:
        ddl_path = os.path.join(ddl_dir, f"{name}{ext}")
        if os.path.exists(ddl_path):
            ddl_source = read_source_file(ddl_path)
            break

    # 프롬프트 조립
    prompt = prompt_template.replace("{{COPYBOOK_NAME}}", name)
    prompt = prompt.replace("{{COPYBOOK_SOURCE}}", source)
    prompt = prompt.replace("{{DDL_SOURCE}}", ddl_source or "(없음)")

    print(f"    🤖 Claude Code 데이터 분석 중: {name} ({len(source)} chars)", end="")

    start_time = time.time()
    response = call_claude_code(prompt, config["claude"])
    elapsed = time.time() - start_time

    output_path = os.path.join(output_dir, f"{name}.md")
    save_markdown(response, output_path)

    print(f" ✅ ({elapsed:.1f}초)")

    return {
        "copybook": name,
        "status": "error" if is_error_response(response) else "success",
        "elapsed_seconds": round(elapsed, 1),
        "output_file": output_path,
    }


def main():
    parser = argparse.ArgumentParser(description="CATM: Claude Code 비즈니스 로직 분석")
    parser.add_argument("--single", type=str, help="단일 프로그램 파일 경로")
    parser.add_argument("--names", type=str, help="분석할 프로그램명 (콤마 구분)")
    parser.add_argument(
        "--mode", choices=["logic", "data"], default="logic",
        help="분석 모드: logic(기본값)=COBOL 프로그램, data=COPYBOOK/DCLGEN 데이터 구조"
    )
    parser.add_argument("--config", default="")
    args = parser.parse_args()

    is_data_mode = args.mode == "data"

    if is_data_mode:
        print("=" * 60)
        print("  CATM Step 5: Claude Code 데이터 구조 분석")
        print("  ⚡ Claude Max 20x 구독 사용 (API 아님)")
        print("=" * 60)
    else:
        print("=" * 60)
        print("  CATM Step 5: Claude Code 비즈니스 로직 분석")
        print("  ⚡ Claude Max 20x 구독 사용 (API 아님)")
        print("=" * 60)

    # 설정 로드
    config = load_config(args.config if args.config else "")
    prompt_file = "analyze-data.md" if is_data_mode else "analyze-logic.md"
    output_subdir = "data-dict" if is_data_mode else "docs"
    output_dir = os.path.join(config["paths"]["output_root"], output_subdir)
    ensure_dir(output_dir)

    # 프롬프트 템플릿 로드
    prompt_path = os.path.join(config["paths"]["prompts_dir"], prompt_file)
    if not os.path.exists(prompt_path):
        print(f"\n  ❌ 프롬프트 템플릿이 없습니다: {prompt_path}")
        return

    with open(prompt_path, "r", encoding="utf-8") as f:
        prompt_template = f.read()

    # Claude Code 사용 가능 여부 확인
    print("\n  🔍 Claude Code 확인 중...")
    try:
        check = subprocess.run(
            [config["claude"]["cli_command"], "--version"],
            capture_output=True, text=True, timeout=10
        )
        if check.returncode == 0:
            print(f"    ✅ Claude Code 버전: {check.stdout.strip()}")
        else:
            print(f"    ⚠️ Claude Code 응답: {check.stderr.strip()}")
    except FileNotFoundError:
        print("    ❌ claude 명령어를 찾을 수 없습니다!")
        print("    설치: npm install -g @anthropic-ai/claude-code")
        print("    로그인: claude login (Max 20x 계정으로)")
        return
    except Exception as e:
        print(f"    ⚠️ 확인 중 오류: {e} (계속 진행)")

    # 분석 대상 결정
    if is_data_mode:
        # data 모드: COPYBOOK + DCLGEN 파일 대상
        if args.single:
            target_files = [Path(args.single)]
        else:
            cpy_dir = os.path.join(
                config["paths"]["source_root"],
                config["source_dirs"]["copybook"]
            )
            dclgen_dir = os.path.join(
                config["paths"]["source_root"],
                config["source_dirs"].get("dclgen", "dclgen")
            )
            target_files = find_files(cpy_dir, config["file_extensions"]["copybook"])
            target_files += find_files(dclgen_dir, ["", ".dclgen"])

            if args.names:
                target_names = set(n.upper() for n in args.names.split(","))
                target_files = [f for f in target_files if f.stem.upper() in target_names]
    else:
        # logic 모드: COBOL 프로그램 대상
        if args.single:
            target_files = [Path(args.single)]
        else:
            cobol_dir = os.path.join(
                config["paths"]["source_root"],
                config["source_dirs"]["cobol"]
            )
            target_files = find_files(cobol_dir, config["file_extensions"]["cobol"])

            if args.names:
                target_names = set(n.upper() for n in args.names.split(","))
                target_files = [f for f in target_files if f.stem.upper() in target_names]

    if not target_files:
        print(f"\n  ❌ 분석할 {'COPYBOOK/DCLGEN' if is_data_mode else 'COBOL'} 파일이 없습니다!")
        return

    print(f"\n  📋 분석 대상: {len(target_files)}개 파일")

    # 배치 설정
    batch_size = config["claude"].get("batch_size", 5)
    batch_delay = config["claude"].get("batch_delay_seconds", 10)

    # 분석 실행
    results = []
    total = len(target_files)

    for i, target_file in enumerate(target_files, 1):
        print(f"\n  [{i}/{total}] {target_file.stem.upper()}")

        if is_data_mode:
            result = analyze_single_data(
                str(target_file), config, prompt_template, output_dir
            )
        else:
            result = analyze_single_program(
                str(target_file), config, prompt_template, output_dir
            )
        results.append(result)

        # 배치 간 대기 (rate limit 방지)
        if i % batch_size == 0 and i < total:
            print(f"\n  ⏳ 배치 완료. {batch_delay}초 대기 (rate limit 방지)...")
            time.sleep(batch_delay)

    # 결과 요약
    success = sum(1 for r in results if r["status"] == "success")
    errors = sum(1 for r in results if r["status"] == "error")
    total_time = sum(r["elapsed_seconds"] for r in results)

    # 분석 로그 저장
    log_filename = "data_analysis_log.json" if is_data_mode else "analysis_log.json"
    log = {
        "analysis_date": datetime.now().isoformat(),
        "mode": f"claude_code_max_20x_{args.mode}",
        "total_files": len(results),
        "success": success,
        "errors": errors,
        "total_elapsed_seconds": round(total_time, 1),
        "results": results,
    }

    log_path = os.path.join(config["paths"]["output_root"], "reports", log_filename)
    ensure_dir(os.path.dirname(log_path))
    with open(log_path, "w", encoding="utf-8") as f:
        json.dump(log, f, indent=2, ensure_ascii=False)

    print(f"\n{'=' * 60}")
    print(f"  Claude Code 분석 완료!")
    print(f"  성공: {success}개 / 에러: {errors}개 / 총 시간: {total_time:.0f}초")
    print(f"  결과: {output_dir}/")
    print(f"  로그: {log_path}")
    print(f"{'=' * 60}")


if __name__ == "__main__":
    main()
