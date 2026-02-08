#!/usr/bin/env python3
"""
04_generate_diagrams.py - Mermaid 다이어그램 자동 생성

dependency-scan.json을 기반으로:
- 호출관계 그래프 (Call Graph)
- JCL 배치 흐름도
- 데이터 흐름도
를 Mermaid 마크다운으로 생성합니다.

사용법:
    python catm/scripts/04_generate_diagrams.py
"""

import sys
import os

# pip install -e . 로 설치되지 않은 환경을 위한 폴백
if __name__ == "__main__":
    sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.dirname(__file__))))

from catm.utils.file_utils import load_config, load_json, save_markdown
from catm.utils.mermaid_builder import (
    build_call_graph, build_jcl_flow, build_data_flow
)


def main():
    print("=" * 50)
    print("  CATM Step 4: Mermaid 다이어그램 생성")
    print("=" * 50)
    
    config = load_config()
    output_root = config["paths"]["output_root"]
    
    # 의존성 데이터 로드
    scan_path = os.path.join(output_root, "reports", "dependency-scan.json")
    
    if not os.path.exists(scan_path):
        print(f"\n  ❌ {scan_path} 파일이 없습니다.")
        print(f"     먼저 02_extract_dependencies.py를 실행하세요.")
        return
    
    scan_data = load_json(scan_path)
    programs = scan_data.get("programs", [])
    jcl_jobs = scan_data.get("jcl_jobs", [])
    
    diagrams_dir = os.path.join(output_root, config["output"]["diagrams_dir"])
    generated = 0
    
    # 1. 호출관계 그래프
    if programs and config["output"]["generate_diagrams"]["call_graph"]:
        print("\n  📐 호출관계 그래프 생성 중...")
        md = build_call_graph(programs)
        save_markdown(md, os.path.join(diagrams_dir, "call_graph.md"))
        generated += 1
    
    # 2. JCL 배치 흐름도
    if jcl_jobs and config["output"]["generate_diagrams"]["jcl_flow"]:
        print("  📐 JCL 배치 흐름도 생성 중...")
        md = build_jcl_flow(jcl_jobs)
        save_markdown(md, os.path.join(diagrams_dir, "jcl_flow.md"))
        generated += 1
    
    # 3. 데이터 흐름도
    if programs and config["output"]["generate_diagrams"]["data_flow"]:
        print("  📐 데이터 흐름도 생성 중...")
        md = build_data_flow(programs)
        save_markdown(md, os.path.join(diagrams_dir, "data_flow.md"))
        generated += 1
    
    # 4. 종합 다이어그램 문서
    print("  📐 종합 다이어그램 문서 생성 중...")
    summary_md = f"""# COBOL 시스템 다이어그램 종합

> 자동 생성 by CATM

## 프로그램 수: {len(programs)}개 / JCL: {len(jcl_jobs)}개

---

개별 다이어그램은 아래 파일을 참조하세요:
- [호출관계 그래프](./call_graph.md)
- [JCL 배치 흐름도](./jcl_flow.md)
- [데이터 흐름도](./data_flow.md)
- [ERD](./erd_copybooks.md) (03_parse_copybook에서 생성)
"""
    save_markdown(summary_md, os.path.join(diagrams_dir, "README.md"))
    
    print(f"\n{'=' * 50}")
    print(f"  다이어그램 생성 완료! ({generated}개)")
    print(f"  결과: {diagrams_dir}/")
    print(f"{'=' * 50}")


if __name__ == "__main__":
    main()
