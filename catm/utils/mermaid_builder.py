"""
mermaid_builder.py - Mermaid 다이어그램 생성 유틸리티
"""


class MermaidBuilder:
    """Mermaid 다이어그램 마크다운 래퍼"""
    
    def __init__(self):
        self.lines: list[str] = []
    
    def add(self, line: str) -> "MermaidBuilder":
        self.lines.append(line)
        return self
    
    def blank(self) -> "MermaidBuilder":
        self.lines.append("")
        return self
    
    def build(self) -> str:
        return "\n".join(self.lines)
    
    def build_markdown(self, title: str = "") -> str:
        """마크다운 코드블록으로 감싸기"""
        md = ""
        if title:
            md += f"# {title}\n\n"
        md += "```mermaid\n"
        md += self.build()
        md += "\n```\n"
        return md


def build_call_graph(programs: list[dict]) -> str:
    """프로그램 간 호출 관계 다이어그램"""
    b = MermaidBuilder()
    b.add("graph TD")
    b.add('    classDef mainPgm fill:#2563EB,stroke:#1E40AF,color:#fff,stroke-width:2px')
    b.add('    classDef subPgm fill:#059669,stroke:#047857,color:#fff')
    b.add('    classDef copybook fill:#D97706,stroke:#B45309,color:#fff')
    b.add('    classDef db2 fill:#7C3AED,stroke:#6D28D9,color:#fff')
    b.blank()
    
    all_called = set()
    all_names = set()
    
    for pgm in programs:
        name = pgm["name"]
        all_names.add(name)
        
        for call in pgm.get("calls", []):
            all_called.add(call)
            b.add(f"    {name}[{name}] -->|CALL| {call}[{call}]")
        
        for copy in pgm.get("copies", []):
            b.add(f"    {name} -.->|COPY| CPY_{copy}({copy})")
        
        for table in pgm.get("db2_tables", [])[:5]:  # 너무 많으면 제한
            b.add(f"    {name} <-->|SQL| DB_{table}[({table})]")
    
    b.blank()
    
    # 스타일 적용
    main_pgms = all_names - all_called
    for name in main_pgms:
        b.add(f"    class {name} mainPgm")
    for name in all_called & all_names:
        b.add(f"    class {name} subPgm")
    
    return b.build_markdown("프로그램 호출 관계도 (Call Graph)")


def build_jcl_flow(jcl_jobs: list[dict]) -> str:
    """JCL 배치 실행 흐름 다이어그램"""
    b = MermaidBuilder()
    b.add("graph LR")
    b.add('    classDef job fill:#DC2626,stroke:#B91C1C,color:#fff,stroke-width:2px')
    b.add('    classDef step fill:#2563EB,stroke:#1E40AF,color:#fff')
    b.blank()
    
    for job in jcl_jobs:
        job_name = job["job_name"]
        prev_step = None
        
        for step in job.get("steps", []):
            step_id = f"{job_name}_{step['step']}"
            label = f"{step['step']}\\n({step['program']})"
            b.add(f'    {step_id}["{label}"]')
            
            if prev_step:
                b.add(f"    {prev_step} --> {step_id}")
            else:
                b.add(f"    {job_name}(({job_name})) --> {step_id}")
                b.add(f"    class {job_name} job")
            
            b.add(f"    class {step_id} step")
            prev_step = step_id
        
        b.blank()
    
    return b.build_markdown("JCL 배치 실행 흐름도")


def build_data_flow(programs: list[dict]) -> str:
    """프로그램 ↔ DB/파일 데이터 흐름도"""
    b = MermaidBuilder()
    b.add("graph LR")
    b.add('    classDef pgm fill:#2563EB,stroke:#1E40AF,color:#fff')
    b.add('    classDef db fill:#7C3AED,stroke:#6D28D9,color:#fff')
    b.add('    classDef file fill:#D97706,stroke:#B45309,color:#fff')
    b.blank()
    
    for pgm in programs:
        name = pgm["name"]
        
        for table in pgm.get("db2_tables", []):
            b.add(f"    {name}[{name}] <-->|SQL| DB_{table}[({table})]")
            b.add(f"    class DB_{table} db")
        
        for vsam in pgm.get("vsam_files", []):
            b.add(f"    {name}[{name}] <-->|I/O| F_{vsam}[/{vsam}/]")
            b.add(f"    class F_{vsam} file")
        
        if pgm.get("db2_tables") or pgm.get("vsam_files"):
            b.add(f"    class {name} pgm")
        
        b.blank()
    
    return b.build_markdown("데이터 흐름도 (Data Flow)")


def build_erd(copybook_fields: dict) -> str:
    """COPYBOOK/DB2 기반 ERD"""
    b = MermaidBuilder()
    b.add("erDiagram")
    b.blank()
    
    for table_name, fields in copybook_fields.items():
        b.add(f"    {table_name} {{")
        for field in fields:
            if field.get("picture"):
                dtype = "string" if "X" in field["picture"].upper() else "number"
                fname = field["name"].replace("-", "_")
                b.add(f"        {dtype} {fname}")
        b.add("    }")
        b.blank()
    
    return b.build_markdown("데이터 ERD (Entity Relationship Diagram)")
