/**
 * CATM Dashboard - Modern Application
 * COBOL Analysis Task Manager Dashboard
 */

// ===================================
// Configuration
// ===================================

const CONFIG = {
    dataPath: 'output/',
    files: {
        dependency: 'reports/dependency-scan.json',
        priority: 'reports/priority_data.json',
        inventory: 'reports/inventory.json',
        analysisLog: 'reports/analysis_log.json'
    },
    // Phase 분류 기준 (final score)
    phaseThresholds: {
        phase1: { max: 5 },        // 즉시 전환: final <= 5
        phase2: { min: 5, max: 7 }, // 3개월: 5 < final <= 7
        phase3: { min: 7 }          // 6개월: final > 7
    },
    counterDuration: 800 // ms
};

// ===================================
// State
// ===================================

let state = {
    programs: [],
    priorityData: [],
    summary: {},
    scanDate: null,
    inventory: null,
    analysisLog: null,
    activeTab: 'overview',
    mermaidRendered: false,
    dataFlowRendered: false,
    erdRendered: false,
    dataDictLoaded: false,
    modernizationRendered: false,
    graphView: 'call', // 'call' or 'dataflow'
    dataLoaded: false,
    expandedProgram: null, // 현재 펼쳐진 프로그램명
    programDocsCache: {}, // 프로그램 문서 캐시
    // Pagination state
    currentPage: 1,
    pageSize: 50,
    filteredPrograms: [],
    // Filter state
    filters: {
        complexityMin: 0,
        complexityMax: 100,
        linesMin: 0,
        linesMax: 10000,
        showDB2: true,
        showCICS: true,
        showVSAM: true,
        showPhase1: true,
        showPhase2: true,
        showPhase3: true
    }
};

// ===================================
// Data Loading
// ===================================

async function loadData() {
    try {
        const [depResponse, priorityResponse] = await Promise.all([
            fetch(CONFIG.dataPath + CONFIG.files.dependency),
            fetch(CONFIG.dataPath + CONFIG.files.priority)
        ]);

        const depData = await depResponse.json();
        state.programs = depData.programs || [];
        state.summary = depData.summary || {};
        state.scanDate = depData.scan_date;

        const priorityData = await priorityResponse.json();
        state.priorityData = priorityData.scored_programs || [];

        // Merge priority scores into programs
        state.programs = state.programs.map(prog => {
            const priority = state.priorityData.find(p => p.name === prog.name);
            return {
                ...prog,
                scores: priority?.scores || null,
                recommendation: priority?.recommendation || null
            };
        });

        state.dataLoaded = true;

        // Load optional files (non-blocking)
        loadOptionalData();

        return true;
    } catch (error) {
        console.error('데이터 로드 오류:', error);
        return false;
    }
}

async function loadOptionalData() {
    // Inventory
    try {
        const resp = await fetch(CONFIG.dataPath + CONFIG.files.inventory);
        if (resp.ok) state.inventory = await resp.json();
    } catch (e) { /* inventory 없으면 무시 */ }

    // Analysis log
    try {
        const resp = await fetch(CONFIG.dataPath + CONFIG.files.analysisLog);
        if (resp.ok) state.analysisLog = await resp.json();
    } catch (e) { /* log 없으면 무시 */ }

    // 개요 탭 추가 데이터 렌더링
    renderExtraSummaryCards();
    renderAnalysisStatus();
    renderRiskSummary();
}

// ===================================
// Counter Animation
// ===================================

function animateCounter(element, targetValue, duration = CONFIG.counterDuration, isFloat = false) {
    const startTime = performance.now();
    const startValue = 0;

    function update(currentTime) {
        const elapsed = currentTime - startTime;
        const progress = Math.min(elapsed / duration, 1);

        // easeOutCubic
        const eased = 1 - Math.pow(1 - progress, 3);
        const current = startValue + (targetValue - startValue) * eased;

        if (isFloat) {
            element.textContent = current.toFixed(1);
        } else {
            element.textContent = Math.round(current).toLocaleString();
        }

        if (progress < 1) {
            requestAnimationFrame(update);
        }
    }

    requestAnimationFrame(update);
}

// ===================================
// Skeleton Removal
// ===================================

function removeSkeleton(containerId) {
    const container = document.getElementById(containerId);
    if (!container) return;
    const skeletons = container.querySelectorAll('.skeleton');
    skeletons.forEach(s => s.remove());
}

function showChartCanvas(wrapperId, canvasId) {
    const wrapper = document.getElementById(wrapperId);
    if (!wrapper) return;
    const skeleton = wrapper.querySelector('.skeleton');
    if (skeleton) skeleton.remove();
    const canvas = document.getElementById(canvasId);
    if (canvas) canvas.style.display = 'block';
}

// ===================================
// Tab Navigation
// ===================================

function initTabs() {
    const tabBtns = document.querySelectorAll('.tab-btn');
    tabBtns.forEach(btn => {
        btn.addEventListener('click', () => {
            const tabId = btn.dataset.tab;
            switchTab(tabId);
        });
    });
}

function switchTab(tabId) {
    if (state.activeTab === tabId) return;

    // Update button states
    document.querySelectorAll('.tab-btn').forEach(btn => {
        btn.classList.toggle('active', btn.dataset.tab === tabId);
    });

    // Fade out current panel
    const currentPanel = document.querySelector('.tab-panel.active');
    if (currentPanel) {
        currentPanel.style.opacity = '0';
        currentPanel.style.transform = 'translateY(8px)';
        setTimeout(() => {
            currentPanel.classList.remove('active');
            currentPanel.style.display = 'none';

            // Fade in new panel
            const newPanel = document.getElementById('tab-' + tabId);
            if (newPanel) {
                newPanel.style.display = 'block';
                newPanel.style.opacity = '0';
                newPanel.style.transform = 'translateY(8px)';
                // Force reflow
                newPanel.offsetHeight;
                newPanel.classList.add('active');
                newPanel.style.opacity = '1';
                newPanel.style.transform = 'translateY(0)';
            }

            // Lazy-load tab content
            if (tabId === 'dependencies' && !state.mermaidRendered && state.dataLoaded) {
                renderDependencyGraph();
            }
            if (tabId === 'datadict' && !state.dataDictLoaded && state.dataLoaded) {
                renderDataDictTab();
            }
            if (tabId === 'modernization' && !state.modernizationRendered && state.dataLoaded) {
                renderModernizationTab();
            }
        }, 200);
    }

    state.activeTab = tabId;
}

// ===================================
// Summary Cards Rendering
// ===================================

function renderSummaryCards() {
    const totalProg = state.summary.total_programs || 0;
    const totalLines = state.summary.total_lines || 0;
    const avgComp = state.summary.avg_complexity || 0;
    const db2Prog = state.summary.db2_programs || 0;

    // Remove skeletons first
    ['totalPrograms', 'totalLines', 'avgComplexity', 'db2Programs', 'copybookCount', 'vsamCount'].forEach(id => {
        const el = document.getElementById(id);
        if (el) {
            const skel = el.querySelector('.skeleton');
            if (skel) skel.remove();
            el.textContent = '0';
        }
    });

    // Animate counters
    animateCounter(document.getElementById('totalPrograms'), totalProg);
    animateCounter(document.getElementById('totalLines'), totalLines);
    animateCounter(document.getElementById('avgComplexity'), avgComp, CONFIG.counterDuration, true);
    animateCounter(document.getElementById('db2Programs'), db2Prog);

    // COPYBOOK, VSAM 수 (summary에서 가져옴)
    const copybookCount = state.summary.unique_copybooks || 0;
    const vsamCount = state.summary.unique_vsam_files || 0;
    animateCounter(document.getElementById('copybookCount'), copybookCount);
    animateCounter(document.getElementById('vsamCount'), vsamCount);

    // Scan date
    if (state.scanDate) {
        const date = new Date(state.scanDate);
        document.getElementById('scanDate').textContent =
            `마지막 스캔: ${date.toLocaleString('ko-KR')}`;
    }
}

function renderExtraSummaryCards() {
    // inventory에서 COPYBOOK 수 업데이트 (summary에 없을 경우)
    if (state.inventory && !state.summary.unique_copybooks) {
        const copybookCat = state.inventory.categories?.find(
            c => c.category === 'COPYBOOK'
        );
        if (copybookCat) {
            const count = copybookCat.files?.length || 0;
            const el = document.getElementById('copybookCount');
            if (el) animateCounter(el, count);
        }
    }
}

// ===================================
// Risk Summary (개요 탭)
// ===================================

async function renderRiskSummary() {
    const riskSection = document.getElementById('riskSection');
    const riskCards = document.getElementById('riskCards');
    if (!riskSection || !riskCards) return;

    const risks = [];

    // 각 프로그램 문서에서 리스크 파싱
    const validPrograms = state.programs.filter(p => p.name !== '.GITKEEP');
    for (const prog of validPrograms) {
        try {
            const resp = await fetch(`${CONFIG.dataPath}docs/${prog.name}.md`);
            if (!resp.ok) continue;
            const md = await resp.text();

            // "리스크" 또는 "특이사항" 섹션 찾기
            const riskMatch = md.match(/#{1,3}\s*\d*\.?\s*(특이사항|리스크|위험|Risk)[^\n]*\n([\s\S]*?)(?=\n#{1,3}\s|\n---|\z)/i);
            if (riskMatch) {
                const riskText = riskMatch[2].trim();
                const lines = riskText.split('\n').filter(l => l.trim().startsWith('-') || l.trim().startsWith('*') || l.trim().match(/^\d+\./));
                lines.forEach(line => {
                    const text = line.replace(/^[\s\-\*\d\.]+/, '').trim();
                    if (text.length > 5) {
                        const level = text.match(/높음|심각|critical|high/i) ? 'high' :
                            text.match(/중간|주의|medium|warning/i) ? 'medium' : 'info';
                        risks.push({ program: prog.name, text, level });
                    }
                });
            }
        } catch (e) { /* 파일 없으면 무시 */ }
    }

    if (risks.length === 0) return;

    riskSection.style.display = 'block';
    const iconMap = { high: '!!', medium: '!', info: 'i' };

    riskCards.innerHTML = risks.map(r => `
        <div class="risk-card risk-${r.level}">
            <div class="risk-icon">${iconMap[r.level]}</div>
            <div class="risk-content">
                <div class="risk-title">${escapeHtml(r.text.slice(0, 60))}${r.text.length > 60 ? '...' : ''}</div>
                <div class="risk-desc">${escapeHtml(r.text)}</div>
                <div class="risk-program">${r.program}</div>
            </div>
        </div>
    `).join('');
}

// ===================================
// Analysis Status (개요 탭)
// ===================================

function renderAnalysisStatus() {
    const section = document.getElementById('analysisStatusSection');
    const container = document.getElementById('analysisStatus');
    if (!section || !container || !state.analysisLog) return;

    const log = state.analysisLog;
    const results = log.results || [];
    const totalElapsed = log.total_elapsed_seconds || 0;
    const successCount = results.filter(r => r.status === 'success').length;
    const failCount = results.filter(r => r.status !== 'success').length;
    const maxElapsed = Math.max(...results.map(r => r.elapsed_seconds || 0), 1);

    section.style.display = 'block';

    const dateStr = log.start_time ?
        new Date(log.start_time).toLocaleString('ko-KR') : '-';

    container.innerHTML = `
        <div class="analysis-status-header">
            <div class="status-meta">
                <div class="status-meta-item">
                    <span class="status-meta-label">분석일시</span>
                    <span class="status-meta-value">${dateStr}</span>
                </div>
                <div class="status-meta-item">
                    <span class="status-meta-label">모드</span>
                    <span class="status-meta-value">${log.mode || '-'}</span>
                </div>
                <div class="status-meta-item">
                    <span class="status-meta-label">총 소요시간</span>
                    <span class="status-meta-value">${totalElapsed.toFixed(1)}초</span>
                </div>
                <div class="status-meta-item">
                    <span class="status-meta-label">결과</span>
                    <span class="status-meta-value">
                        <span class="badge badge-success">성공 ${successCount}</span>
                        ${failCount > 0 ? `<span class="badge badge-fail">실패 ${failCount}</span>` : ''}
                    </span>
                </div>
            </div>
        </div>
        <div class="analysis-bars">
            ${results.map(r => {
        const pct = Math.max(5, (r.elapsed_seconds / maxElapsed) * 100);
        const statusBadge = r.status === 'success'
            ? '<span class="badge badge-success">OK</span>'
            : '<span class="badge badge-fail">FAIL</span>';
        return `
                <div class="analysis-bar-row">
                    <span class="analysis-bar-label">${r.program || r.copybook || '-'}</span>
                    <div class="analysis-bar-track">
                        <div class="analysis-bar-fill" style="width:${pct}%">
                            <span class="bar-time">${(r.elapsed_seconds || 0).toFixed(1)}s</span>
                        </div>
                    </div>
                    <div class="analysis-bar-status">${statusBadge}</div>
                </div>`;
    }).join('')}
        </div>
    `;
}

// ===================================
// Phase Classification
// ===================================

function classifyPhases() {
    const validPrograms = state.programs.filter(p => p.name !== '.GITKEEP');
    const t = CONFIG.phaseThresholds;

    const phases = { 1: [], 2: [], 3: [] };

    validPrograms.forEach(prog => {
        const score = prog.scores?.final || 0;
        if (score <= t.phase1.max) {
            phases[1].push(prog);
        } else if (score <= t.phase2.max) {
            phases[2].push(prog);
        } else {
            phases[3].push(prog);
        }
    });

    return phases;
}

function renderPhaseCards() {
    const phases = classifyPhases();

    [1, 2, 3].forEach(phaseNum => {
        const container = document.getElementById(`phase${phaseNum}Stats`);
        if (!container) return;

        const programs = phases[phaseNum];
        const totalLines = programs.reduce((sum, p) => sum + (p.line_count || 0), 0);
        const methods = {};
        programs.forEach(p => {
            const m = p.recommendation?.method || '미정';
            methods[m] = (methods[m] || 0) + 1;
        });

        container.innerHTML = `
            <div class="phase-stat">
                <span class="phase-stat-label">프로그램 수</span>
                <span class="phase-stat-value">${programs.length}</span>
            </div>
            <div class="phase-stat">
                <span class="phase-stat-label">총 라인 수</span>
                <span class="phase-stat-value">${totalLines.toLocaleString()}</span>
            </div>
            <div class="phase-methods">
                ${Object.entries(methods).map(([method, count]) =>
            `<span class="badge ${getMethodBadgeClass(method)}">${method} (${count})</span>`
        ).join('')}
            </div>
        `;
    });
}

function getMethodBadgeClass(method) {
    if (method.includes('자동')) return 'badge-auto';
    if (method.includes('하이브리드')) return 'badge-hybrid';
    if (method.includes('리라이트') || method.includes('재작성')) return 'badge-rewrite';
    return 'badge-primary';
}

// ===================================
// Program Table (프로그램 상세 탭)
// ===================================

function renderProgramTable() {
    const tbody = document.getElementById('programTableBody');
    const searchTerm = document.getElementById('searchInput').value.toLowerCase();
    const sortBy = document.getElementById('sortSelect').value;

    let filtered = state.programs.filter(prog =>
        prog.name.toLowerCase().includes(searchTerm) &&
        prog.name !== '.GITKEEP'
    );

    filtered.sort((a, b) => {
        switch (sortBy) {
            case 'complexity': return (b.complexity || 0) - (a.complexity || 0);
            case 'lines': return (b.line_count || 0) - (a.line_count || 0);
            case 'score': return (b.scores?.final || 0) - (a.scores?.final || 0);
            default: return a.name.localeCompare(b.name);
        }
    });

    if (filtered.length === 0) {
        tbody.innerHTML = `<tr class="no-data-row"><td colspan="9">
            데이터가 없습니다.
        </td></tr>`;
        return;
    }

    tbody.innerHTML = filtered.map(prog => {
        const method = prog.recommendation?.method || '';
        const badgeClass = getMethodBadgeClass(method);
        const isExpanded = state.expandedProgram === prog.name;

        return `
        <tr class="clickable-row ${isExpanded ? 'expanded' : ''}" data-program="${prog.name}">
            <td>${prog.name}</td>
            <td>${(prog.line_count || 0).toLocaleString()}</td>
            <td>${prog.complexity || 0}</td>
            <td>${prog.calls?.length || 0}</td>
            <td>${prog.copies?.length || 0}</td>
            <td>${prog.has_db2 ? '<span class="check-icon">&#10003;</span>' : '<span class="cross-icon">&mdash;</span>'}</td>
            <td>${prog.has_cics ? '<span class="check-icon">&#10003;</span>' : '<span class="cross-icon">&mdash;</span>'}</td>
            <td>${prog.scores?.final ? `<span class="badge badge-score">${prog.scores.final.toFixed(1)}</span>` : '<span class="cross-icon">&mdash;</span>'}</td>
            <td>${method ? `<span class="badge ${badgeClass}">${method}</span>` : '<span class="cross-icon">&mdash;</span>'}</td>
        </tr>
        ${isExpanded ? `<tr class="detail-panel"><td colspan="9"><div class="detail-panel-inner" id="detail-${prog.name}">
            <div style="color:var(--text-muted);font-size:0.85rem;">로딩 중...</div>
        </div></td></tr>` : ''}`;
    }).join('');

    // 클릭 이벤트 바인딩
    tbody.querySelectorAll('.clickable-row').forEach(row => {
        row.addEventListener('click', () => {
            const progName = row.dataset.program;
            toggleProgramDetail(progName);
        });
    });

    // 이미 펼쳐진 프로그램이 있으면 상세 로드
    if (state.expandedProgram) {
        loadProgramDetail(state.expandedProgram);
    }
}

// ===================================
// Program Detail Panel (아코디언)
// ===================================

function toggleProgramDetail(programName) {
    if (state.expandedProgram === programName) {
        state.expandedProgram = null;
    } else {
        state.expandedProgram = programName;
    }
    renderProgramTable();
}

async function loadProgramDetail(programName) {
    const container = document.getElementById(`detail-${programName}`);
    if (!container) return;

    // 캐시 확인
    if (state.programDocsCache[programName]) {
        renderProgramDetailContent(container, programName, state.programDocsCache[programName]);
        return;
    }

    try {
        const resp = await fetch(`${CONFIG.dataPath}docs/${programName}.md`);
        if (!resp.ok) {
            container.innerHTML = '<div style="color:var(--text-muted);font-size:0.85rem;">분석 문서가 없습니다. Phase 5 분석을 먼저 실행하세요.</div>';
            return;
        }
        const md = await resp.text();
        state.programDocsCache[programName] = md;
        renderProgramDetailContent(container, programName, md);
    } catch (e) {
        container.innerHTML = '<div style="color:var(--text-muted);font-size:0.85rem;">문서 로드 실패</div>';
    }
}

function renderProgramDetailContent(container, programName, markdownText) {
    // 마크다운 섹션 분리
    const sections = parseMarkdownSections(markdownText);

    const tabDefs = [
        { key: 'overview', label: '프로그램 개요' },
        { key: 'dataflow', label: '데이터 흐름' },
        { key: 'logic', label: '비즈니스 로직' },
        { key: 'rules', label: '비즈니스 규칙' },
        { key: 'mes', label: 'MES 관련도' },
        { key: 'risk', label: '리스크/특이사항' }
    ];

    const tabButtons = tabDefs.map((t, i) =>
        `<button class="detail-tab-btn ${i === 0 ? 'active' : ''}" data-detail-tab="${t.key}">${t.label}</button>`
    ).join('');

    const tabContents = tabDefs.map((t, i) => {
        const content = sections[t.key] || '<p style="color:var(--text-muted)">해당 섹션 데이터 없음</p>';
        return `<div class="detail-tab-content ${i === 0 ? 'active' : ''}" data-detail-content="${t.key}">
            <div class="markdown-body">${content}</div>
        </div>`;
    }).join('');

    container.innerHTML = `
        <div class="detail-tabs">${tabButtons}</div>
        ${tabContents}
    `;

    // 서브 탭 전환 이벤트
    container.querySelectorAll('.detail-tab-btn').forEach(btn => {
        btn.addEventListener('click', (e) => {
            e.stopPropagation();
            const tabKey = btn.dataset.detailTab;
            container.querySelectorAll('.detail-tab-btn').forEach(b => b.classList.remove('active'));
            container.querySelectorAll('.detail-tab-content').forEach(c => c.classList.remove('active'));
            btn.classList.add('active');
            container.querySelector(`[data-detail-content="${tabKey}"]`)?.classList.add('active');
        });
    });
}

function parseMarkdownSections(md) {
    const result = { overview: '', dataflow: '', logic: '', rules: '', mes: '', risk: '' };

    // marked.js로 HTML 변환
    const html = marked.parse(md);

    // 섹션 키워드 매핑
    const sectionMap = [
        { key: 'overview', patterns: ['개요', '프로그램 개요', 'overview', '프로그램 정보'] },
        { key: 'dataflow', patterns: ['데이터 흐름', 'data flow', '입출력', '입력/출력'] },
        { key: 'logic', patterns: ['비즈니스 로직', 'business logic', '로직 상세', '처리 로직'] },
        { key: 'rules', patterns: ['비즈니스 규칙', 'business rule', '규칙 요약'] },
        { key: 'mes', patterns: ['MES', '관련도', 'manufacturing'] },
        { key: 'risk', patterns: ['리스크', '특이사항', '위험', 'risk'] }
    ];

    // h2/h3 기준으로 섹션 분할
    const parts = html.split(/(?=<h[23])/i);

    parts.forEach(part => {
        const headerMatch = part.match(/<h[23][^>]*>(.*?)<\/h[23]>/i);
        if (!headerMatch) {
            // 첫 파트 (헤더 없는 부분) → overview
            if (part.trim()) result.overview += part;
            return;
        }
        const headerText = headerMatch[1].replace(/<[^>]+>/g, '').toLowerCase();

        for (const sec of sectionMap) {
            if (sec.patterns.some(p => headerText.includes(p.toLowerCase()))) {
                result[sec.key] += part;
                return;
            }
        }
        // 매칭 안 된 섹션은 logic에 추가 (비즈니스 로직이 가장 범용)
        result.logic += part;
    });

    // overview가 비어있고 전체 문서가 있으면 전체를 overview로
    if (!result.overview.trim() && html.trim()) {
        result.overview = html;
    }

    return result;
}

// ===================================
// Data Dictionary Tab (데이터 사전)
// ===================================

async function renderDataDictTab() {
    state.dataDictLoaded = true;

    renderCopybookCards();
    renderERDDiagram();
}

function renderCopybookCards() {
    const container = document.getElementById('copybookCards');
    if (!container) return;

    // COPYBOOK 목록 구성: inventory + dependency에서 매핑
    const copybooks = getCopybookList();

    if (copybooks.length === 0) {
        container.innerHTML = '<div style="color:var(--text-muted);font-size:0.85rem;padding:1rem;">COPYBOOK 데이터가 없습니다.</div>';
        return;
    }

    container.innerHTML = copybooks.map(cb => {
        const refsHtml = cb.referencedBy.length > 0
            ? cb.referencedBy.map(r => `<span class="badge badge-primary">${r}</span>`).join(' ')
            : '<span style="color:var(--text-muted)">참조 없음</span>';

        return `
        <div class="copybook-card" data-copybook="${cb.name}">
            <div class="copybook-card-name">${cb.name}</div>
            <div class="copybook-card-meta">
                <span>${cb.lines || '-'} lines</span>
                <span>${cb.sizeKb || '-'} KB</span>
            </div>
            <div class="copybook-card-refs">참조: ${refsHtml}</div>
        </div>`;
    }).join('');

    // 카드 클릭 이벤트
    container.querySelectorAll('.copybook-card').forEach(card => {
        card.addEventListener('click', () => {
            const name = card.dataset.copybook;
            selectCopybook(name);
            // active 상태 토글
            container.querySelectorAll('.copybook-card').forEach(c => c.classList.remove('active'));
            card.classList.add('active');
        });
    });
}

function getCopybookList() {
    const copybooks = [];
    const copybookNames = new Set();

    // inventory에서 COPYBOOK 파일 목록
    if (state.inventory?.categories) {
        const copyCat = state.inventory.categories.find(c => c.category === 'COPYBOOK');
        if (copyCat?.files) {
            copyCat.files.forEach(f => {
                const name = f.name.replace(/\.(cpy|CPY)$/, '');
                copybookNames.add(name);
                copybooks.push({
                    name,
                    lines: f.lines || 0,
                    sizeKb: f.size_kb || 0,
                    referencedBy: []
                });
            });
        }
    }

    // dependency-scan에서 참조 매핑
    state.programs.filter(p => p.name !== '.GITKEEP').forEach(prog => {
        (prog.copies || []).forEach(copyName => {
            let cb = copybooks.find(c => c.name === copyName);
            if (!cb) {
                cb = { name: copyName, lines: 0, sizeKb: 0, referencedBy: [] };
                copybooks.push(cb);
            }
            if (!cb.referencedBy.includes(prog.name)) {
                cb.referencedBy.push(prog.name);
            }
        });
    });

    return copybooks;
}

async function selectCopybook(name) {
    const detailDiv = document.getElementById('copybookDetail');
    const titleEl = document.getElementById('copybookDetailTitle');
    const bodyEl = document.getElementById('copybookDetailBody');
    if (!detailDiv || !titleEl || !bodyEl) return;

    detailDiv.style.display = 'block';
    titleEl.textContent = name;
    bodyEl.innerHTML = '<div style="color:var(--text-muted)">로딩 중...</div>';

    try {
        const resp = await fetch(`${CONFIG.dataPath}data-dict/${name}.md`);
        if (!resp.ok) {
            bodyEl.innerHTML = '<div style="color:var(--text-muted)">데이터 사전 파일이 없습니다.</div>';
            return;
        }
        const md = await resp.text();
        bodyEl.innerHTML = marked.parse(md);
    } catch (e) {
        bodyEl.innerHTML = '<div style="color:var(--text-muted)">로드 실패</div>';
    }
}

async function renderERDDiagram() {
    const erdSection = document.getElementById('erdSection');
    const erdGraph = document.getElementById('erdGraph');
    if (!erdSection || !erdGraph || state.erdRendered) return;

    try {
        const resp = await fetch(`${CONFIG.dataPath}diagrams/erd_copybooks.md`);
        if (!resp.ok) return;

        const md = await resp.text();
        const mermaidCode = extractMermaidCode(md);
        if (!mermaidCode) return;

        erdSection.style.display = 'block';
        erdGraph.textContent = mermaidCode;
        erdGraph.removeAttribute('data-processed');
        mermaid.init(undefined, erdGraph);
        state.erdRendered = true;
    } catch (e) { /* ERD 없으면 무시 */ }
}

// ===================================
// Modernization Tab (모더나이제이션)
// ===================================

function renderModernizationTab() {
    state.modernizationRendered = true;

    renderTimeline();
    renderStrategyTable();
    renderRiskMatrix();
    renderRadarChart();
}

function renderTimeline() {
    const container = document.getElementById('modernTimeline');
    if (!container) return;

    const phases = classifyPhases();
    const phaseConfig = [
        { num: 1, title: 'Phase 1 - 즉시 전환', period: '0 ~ 3개월', cssClass: 'phase-1' },
        { num: 2, title: 'Phase 2 - 중기 전환', period: '3 ~ 6개월', cssClass: 'phase-2' },
        { num: 3, title: 'Phase 3 - 안정화', period: '6 ~ 8개월', cssClass: 'phase-3' }
    ];

    container.innerHTML = phaseConfig.map(pc => {
        const progs = phases[pc.num] || [];
        const programBadges = progs.map(p => {
            const badgeClass = getMethodBadgeClass(p.recommendation?.method || '');
            return `<span class="badge ${badgeClass}">${p.name}</span>`;
        }).join('');

        return `
        <div class="timeline-phase ${pc.cssClass}">
            <div class="timeline-phase-dot"></div>
            <div class="timeline-phase-title">${pc.title}</div>
            <div class="timeline-phase-period">${pc.period}</div>
            <div class="timeline-phase-programs">
                ${programBadges || '<span style="color:var(--text-muted);font-size:0.75rem;">해당 없음</span>'}
            </div>
        </div>`;
    }).join('');
}

function renderStrategyTable() {
    const tbody = document.getElementById('strategyTableBody');
    if (!tbody) return;

    const phases = classifyPhases();
    const validPrograms = state.programs.filter(p => p.name !== '.GITKEEP');

    tbody.innerHTML = validPrograms.map(prog => {
        const method = prog.recommendation?.method || '-';
        const badgeClass = getMethodBadgeClass(method);
        const score = prog.scores?.final?.toFixed(1) || '-';

        // Phase 결정
        let phaseNum = 1;
        if (phases[2].includes(prog)) phaseNum = 2;
        if (phases[3].includes(prog)) phaseNum = 3;
        const phaseBadgeClass = `phase-${phaseNum}`;

        // 마일스톤 & 주의사항 (프로그램 특성 기반으로 생성)
        const milestones = [];
        const warnings = [];

        if (prog.has_db2) {
            milestones.push('DB2 쿼리 변환');
            warnings.push('SQL 호환성 검증');
        }
        if (prog.has_cics) {
            milestones.push('CICS 화면 전환');
            warnings.push('트랜잭션 처리 주의');
        }
        if ((prog.calls?.length || 0) > 2) {
            milestones.push('서브프로그램 통합 테스트');
        }
        if ((prog.copies?.length || 0) > 3) {
            warnings.push('COPYBOOK 매핑 복잡');
        }
        milestones.push('단위 테스트');

        return `
        <tr>
            <td>${prog.name}</td>
            <td><span class="badge ${badgeClass}">${method}</span></td>
            <td><span class="phase-badge ${phaseBadgeClass}">Phase ${phaseNum}</span></td>
            <td><span class="badge badge-score">${score}</span></td>
            <td>${milestones.join(', ')}</td>
            <td>${warnings.join(', ') || '-'}</td>
        </tr>`;
    }).join('');
}

function renderRiskMatrix() {
    const container = document.getElementById('riskMatrixCards');
    if (!container) return;

    const riskCategories = [
        {
            icon: '&#128451;',
            title: '데이터 마이그레이션',
            body: 'DB2 테이블 구조 변환, 데이터 타입 매핑, COMP-3/COMP 필드 변환 시 정밀도 손실 가능성'
        },
        {
            icon: '&#128296;',
            title: '통합 테스트',
            body: 'CALL 체인 간 파라미터 전달 검증, 온라인(CICS)-배치 간 데이터 정합성 확인 필요'
        },
        {
            icon: '&#9200;',
            title: '배치 스케줄링',
            body: 'JCL 기반 배치 흐름을 현대 스케줄러로 전환 시 실행 순서와 의존성 재검증 필요'
        },
        {
            icon: '&#128220;',
            title: 'COPYBOOK 전환',
            body: '공유 COPYBOOK 변경 시 참조하는 모든 프로그램에 영향. 88-레벨 조건명 매핑 주의'
        }
    ];

    container.innerHTML = riskCategories.map(r => `
        <div class="risk-matrix-card">
            <div class="risk-matrix-card-header">
                <span class="risk-matrix-card-icon">${r.icon}</span>
                <span class="risk-matrix-card-title">${r.title}</span>
            </div>
            <div class="risk-matrix-card-body">${r.body}</div>
        </div>
    `).join('');
}

let radarChartInstance = null;

function renderRadarChart() {
    const canvas = document.getElementById('radarChart');
    if (!canvas) return;

    const { textColor, gridColor } = getChartColors();

    if (radarChartInstance) radarChartInstance.destroy();

    // 가중치 기준 데이터
    const weights = {
        labels: ['비즈니스 중요도 (35%)', '기술 복잡도 (25%)', '의존성 영향도 (20%)', '전환 용이도 (20%)'],
        values: [35, 25, 20, 20]
    };

    // 프로그램별 점수도 함께 표시
    const validPrograms = state.priorityData.filter(p => p.name !== '.GITKEEP');
    const datasets = [
        {
            label: '가중치 기준',
            data: weights.values.map(v => v / 5), // 0-10 스케일로 변환
            backgroundColor: 'rgba(59, 130, 246, 0.15)',
            borderColor: 'rgba(59, 130, 246, 0.8)',
            borderWidth: 2,
            pointBackgroundColor: 'rgba(59, 130, 246, 1)',
            pointRadius: 4
        }
    ];

    // 각 프로그램의 실제 점수
    const colors = [
        { bg: 'rgba(139, 92, 246, 0.1)', border: 'rgba(139, 92, 246, 0.7)', point: 'rgba(139, 92, 246, 1)' },
        { bg: 'rgba(16, 185, 129, 0.1)', border: 'rgba(16, 185, 129, 0.7)', point: 'rgba(16, 185, 129, 1)' },
        { bg: 'rgba(245, 158, 11, 0.1)', border: 'rgba(245, 158, 11, 0.7)', point: 'rgba(245, 158, 11, 1)' }
    ];

    validPrograms.forEach((prog, i) => {
        const c = colors[i % colors.length];
        datasets.push({
            label: prog.name,
            data: [
                prog.scores?.business_importance || 0,
                prog.scores?.technical_complexity || 0,
                prog.scores?.dependency_impact || 0,
                prog.scores?.conversion_ease || 0
            ],
            backgroundColor: c.bg,
            borderColor: c.border,
            borderWidth: 2,
            pointBackgroundColor: c.point,
            pointRadius: 4
        });
    });

    radarChartInstance = new Chart(canvas, {
        type: 'radar',
        data: {
            labels: weights.labels,
            datasets
        },
        options: {
            responsive: true,
            maintainAspectRatio: false,
            plugins: {
                legend: {
                    position: 'bottom',
                    labels: {
                        color: textColor,
                        font: { family: "'Pretendard Variable', Pretendard, sans-serif", size: 12 },
                        padding: 16,
                        usePointStyle: true
                    }
                }
            },
            scales: {
                r: {
                    beginAtZero: true,
                    max: 10,
                    ticks: {
                        color: textColor,
                        backdropColor: 'transparent',
                        font: { size: 10 },
                        stepSize: 2
                    },
                    grid: { color: gridColor },
                    angleLines: { color: gridColor },
                    pointLabels: {
                        color: textColor,
                        font: { family: "'Pretendard Variable', Pretendard, sans-serif", size: 11 }
                    }
                }
            }
        }
    });
}

// ===================================
// Charts
// ===================================

let priorityChartInstance = null;
let techChartInstance = null;

function getChartColors() {
    const isDark = document.documentElement.getAttribute('data-theme') === 'dark';
    return {
        textColor: isDark ? '#e8edf4' : '#1a2332',
        gridColor: isDark ? 'rgba(148, 163, 184, 0.1)' : 'rgba(148, 163, 184, 0.15)',
        isDark
    };
}

function renderPriorityChart() {
    showChartCanvas('priorityChartWrap', 'priorityChart');

    const ctx = document.getElementById('priorityChart').getContext('2d');
    const validPrograms = state.priorityData.filter(p => p.name !== '.GITKEEP');
    const { textColor, gridColor } = getChartColors();

    if (priorityChartInstance) priorityChartInstance.destroy();

    // Gradient fills
    const blueGrad = ctx.createLinearGradient(0, 0, 0, 280);
    blueGrad.addColorStop(0, 'rgba(59, 130, 246, 0.8)');
    blueGrad.addColorStop(1, 'rgba(59, 130, 246, 0.3)');

    const purpleGrad = ctx.createLinearGradient(0, 0, 0, 280);
    purpleGrad.addColorStop(0, 'rgba(139, 92, 246, 0.8)');
    purpleGrad.addColorStop(1, 'rgba(139, 92, 246, 0.3)');

    const greenGrad = ctx.createLinearGradient(0, 0, 0, 280);
    greenGrad.addColorStop(0, 'rgba(16, 185, 129, 0.8)');
    greenGrad.addColorStop(1, 'rgba(16, 185, 129, 0.3)');

    priorityChartInstance = new Chart(ctx, {
        type: 'bar',
        data: {
            labels: validPrograms.map(p => p.name),
            datasets: [
                {
                    label: '비즈니스 중요도',
                    data: validPrograms.map(p => p.scores?.business_importance || 0),
                    backgroundColor: blueGrad,
                    borderRadius: 6,
                    borderSkipped: false
                },
                {
                    label: '기술 복잡도',
                    data: validPrograms.map(p => p.scores?.technical_complexity || 0),
                    backgroundColor: purpleGrad,
                    borderRadius: 6,
                    borderSkipped: false
                },
                {
                    label: '의존성 영향도',
                    data: validPrograms.map(p => p.scores?.dependency_impact || 0),
                    backgroundColor: greenGrad,
                    borderRadius: 6,
                    borderSkipped: false
                }
            ]
        },
        options: {
            responsive: true,
            maintainAspectRatio: false,
            plugins: {
                legend: {
                    position: 'bottom',
                    labels: {
                        color: textColor,
                        font: { family: "'Pretendard Variable', Pretendard, sans-serif", size: 12 },
                        padding: 16,
                        usePointStyle: true,
                        pointStyleWidth: 8
                    }
                }
            },
            scales: {
                x: {
                    ticks: {
                        color: textColor,
                        font: { family: "'Pretendard Variable', Pretendard, sans-serif", size: 11 }
                    },
                    grid: { display: false }
                },
                y: {
                    beginAtZero: true,
                    max: 10,
                    ticks: {
                        color: textColor,
                        font: { family: "'Pretendard Variable', Pretendard, sans-serif", size: 11 },
                        stepSize: 2
                    },
                    grid: { color: gridColor }
                }
            }
        }
    });
}

function renderTechChart() {
    showChartCanvas('techChartWrap', 'techChart');

    const ctx = document.getElementById('techChart').getContext('2d');
    const { textColor } = getChartColors();

    if (techChartInstance) techChartInstance.destroy();

    const total = state.summary.total_programs || 0;
    const db2 = state.summary.db2_programs || 0;
    const vsam = state.summary.vsam_programs || 0;
    const cics = state.summary.cics_programs || 0;
    const simple = Math.max(0, total - db2 - vsam - cics);

    // Show center text
    const centerEl = document.getElementById('doughnutCenter');
    if (centerEl) {
        centerEl.style.display = 'block';
        document.getElementById('doughnutTotal').textContent = total;
    }

    techChartInstance = new Chart(ctx, {
        type: 'doughnut',
        data: {
            labels: ['DB2', 'VSAM', 'CICS', '단순'],
            datasets: [{
                data: [db2, vsam, cics, simple],
                backgroundColor: [
                    'rgba(139, 92, 246, 0.85)',
                    'rgba(59, 130, 246, 0.85)',
                    'rgba(245, 158, 11, 0.85)',
                    'rgba(148, 163, 184, 0.4)'
                ],
                borderWidth: 0,
                hoverOffset: 6
            }]
        },
        options: {
            responsive: true,
            maintainAspectRatio: false,
            cutout: '65%',
            plugins: {
                legend: {
                    position: 'bottom',
                    labels: {
                        color: textColor,
                        font: { family: "'Pretendard Variable', Pretendard, sans-serif", size: 12 },
                        padding: 16,
                        usePointStyle: true,
                        pointStyleWidth: 8
                    }
                }
            }
        }
    });
}

// ===================================
// Dependency Graph
// ===================================

let graphScale = 1;

function renderDependencyGraph() {
    const validPrograms = state.programs.filter(p => p.name !== '.GITKEEP');

    let mermaidCode = 'graph TD\n';
    mermaidCode += '    classDef mainPgm fill:#3b82f6,stroke:#1e40af,color:#fff,rx:8,font-size:14px\n';
    mermaidCode += '    classDef subPgm fill:#10b981,stroke:#047857,color:#fff,rx:8,font-size:14px\n';
    mermaidCode += '    classDef copybook fill:#f59e0b,stroke:#b45309,color:#fff,rx:8,font-size:14px\n';
    mermaidCode += '    classDef db2 fill:#8b5cf6,stroke:#6d28d9,color:#fff,rx:8,font-size:14px\n\n';

    validPrograms.forEach(prog => {
        prog.calls?.forEach(call => {
            mermaidCode += `    ${prog.name}[${prog.name}] -->|CALL| ${call}[${call}]\n`;
        });
        prog.copies?.forEach(copy => {
            mermaidCode += `    ${prog.name} -.->|COPY| CPY_${copy}(${copy})\n`;
        });
        prog.db2_tables?.forEach(table => {
            mermaidCode += `    ${prog.name} <-->|SQL| DB_${table}[(${table})]\n`;
        });
    });

    // Apply styles
    validPrograms.forEach(prog => {
        mermaidCode += `    class ${prog.name} mainPgm\n`;
        prog.calls?.forEach(call => {
            mermaidCode += `    class ${call} subPgm\n`;
        });
        prog.copies?.forEach(copy => {
            mermaidCode += `    class CPY_${copy} copybook\n`;
        });
        prog.db2_tables?.forEach(table => {
            mermaidCode += `    class DB_${table} db2\n`;
        });
    });

    const graphContainer = document.getElementById('dependencyGraph');
    graphContainer.textContent = mermaidCode;
    graphContainer.removeAttribute('data-processed');

    mermaid.init(undefined, graphContainer);
    state.mermaidRendered = true;

    // Apply initial scale after rendering
    setTimeout(() => updateGraphScale(), 100);
}

async function renderDataFlowGraph() {
    if (state.dataFlowRendered) return;

    const graphContainer = document.getElementById('dependencyGraph');

    try {
        const resp = await fetch(`${CONFIG.dataPath}diagrams/data_flow.md`);
        if (!resp.ok) {
            graphContainer.innerHTML = '<div style="color:var(--text-muted);padding:2rem;text-align:center;">Data Flow 다이어그램이 없습니다.</div>';
            return;
        }

        const md = await resp.text();
        const mermaidCode = extractMermaidCode(md);
        if (!mermaidCode) {
            graphContainer.innerHTML = '<div style="color:var(--text-muted);padding:2rem;text-align:center;">Mermaid 코드를 찾을 수 없습니다.</div>';
            return;
        }

        graphContainer.textContent = mermaidCode;
        graphContainer.removeAttribute('data-processed');
        mermaid.init(undefined, graphContainer);
        state.dataFlowRendered = true;

        setTimeout(() => updateGraphScale(), 100);
    } catch (e) {
        graphContainer.innerHTML = '<div style="color:var(--text-muted);padding:2rem;text-align:center;">Data Flow 로드 실패</div>';
    }
}

function extractMermaidCode(md) {
    // ```mermaid ... ``` 블록 추출
    const match = md.match(/```mermaid\s*\n([\s\S]*?)```/);
    if (match) return match[1].trim();

    // mermaid 코드 블록이 없으면 graph/erDiagram으로 시작하는 부분 찾기
    const lines = md.split('\n');
    let inCode = false;
    let code = [];
    for (const line of lines) {
        if (line.trim().match(/^(graph|flowchart|erDiagram|sequenceDiagram|classDiagram)/)) {
            inCode = true;
        }
        if (inCode) {
            if (line.trim() === '```' || (line.startsWith('#') && code.length > 0)) break;
            code.push(line);
        }
    }
    return code.length > 0 ? code.join('\n').trim() : null;
}

function switchGraphView(view) {
    if (state.graphView === view) return;
    state.graphView = view;

    // 버튼 상태 업데이트
    document.getElementById('graphCallView')?.classList.toggle('active', view === 'call');
    document.getElementById('graphDataFlowView')?.classList.toggle('active', view === 'dataflow');

    // 범례 파일 항목 표시/숨김
    const legendFile = document.getElementById('legendFile');
    if (legendFile) legendFile.style.display = view === 'dataflow' ? 'flex' : 'none';

    if (view === 'call') {
        // Call graph 재렌더링 필요
        state.mermaidRendered = false;
        renderDependencyGraph();
    } else {
        renderDataFlowGraph();
    }
}

function updateGraphScale() {
    const mermaidEl = document.querySelector('#dependencyGraph svg');
    if (mermaidEl) {
        mermaidEl.style.transform = `scale(${graphScale})`;
        mermaidEl.style.transformOrigin = 'center top';
    }
}

function initGraphControls() {
    // Call Graph / Data Flow 토글
    document.getElementById('graphCallView')?.addEventListener('click', () => switchGraphView('call'));
    document.getElementById('graphDataFlowView')?.addEventListener('click', () => switchGraphView('dataflow'));

    // Zoom In
    document.getElementById('graphZoomIn')?.addEventListener('click', () => {
        graphScale = Math.min(graphScale + 0.2, 2.5);
        updateGraphScale();
    });

    // Zoom Out
    document.getElementById('graphZoomOut')?.addEventListener('click', () => {
        graphScale = Math.max(graphScale - 0.2, 0.5);
        updateGraphScale();
    });

    // Fullscreen
    document.getElementById('graphFullscreen')?.addEventListener('click', () => {
        const container = document.getElementById('mermaidContainer');
        container.classList.add('fullscreen');
        document.body.style.overflow = 'hidden';
    });

    // Close Fullscreen
    document.getElementById('fullscreenClose')?.addEventListener('click', () => {
        const container = document.getElementById('mermaidContainer');
        container.classList.remove('fullscreen');
        document.body.style.overflow = '';
    });

    // ESC key to close fullscreen
    document.addEventListener('keydown', (e) => {
        if (e.key === 'Escape') {
            const container = document.getElementById('mermaidContainer');
            if (container?.classList.contains('fullscreen')) {
                container.classList.remove('fullscreen');
                document.body.style.overflow = '';
            }
        }
    });
}

// ===================================
// Theme Toggle
// ===================================

function initTheme() {
    const savedTheme = localStorage.getItem('catm-theme') || 'light';
    document.documentElement.setAttribute('data-theme', savedTheme);
}

function toggleTheme() {
    const btn = document.getElementById('themeToggle');
    btn.classList.add('rotating');
    setTimeout(() => btn.classList.remove('rotating'), 400);

    const current = document.documentElement.getAttribute('data-theme');
    const next = current === 'dark' ? 'light' : 'dark';
    document.documentElement.setAttribute('data-theme', next);
    localStorage.setItem('catm-theme', next);

    // Re-render charts with new colors
    if (state.dataLoaded) {
        renderPriorityChart();
        renderTechChart();
        if (state.modernizationRendered) {
            renderRadarChart();
        }
    }
}

// ===================================
// Utility Functions
// ===================================

function escapeHtml(str) {
    const div = document.createElement('div');
    div.textContent = str;
    return div.innerHTML;
}

// ===================================
// Event Listeners
// ===================================

function initEventListeners() {
    document.getElementById('themeToggle').addEventListener('click', toggleTheme);
    document.getElementById('searchInput').addEventListener('input', renderProgramTable);
    document.getElementById('sortSelect').addEventListener('change', renderProgramTable);
    initGraphControls();
}

// ===================================
// Initialization
// ===================================

async function init() {
    initTheme();
    initTabs();
    initEventListeners();

    // Initialize Mermaid with larger font settings
    mermaid.initialize({
        startOnLoad: false,
        theme: 'default',
        securityLevel: 'loose',
        flowchart: {
            nodeSpacing: 60,
            rankSpacing: 80,
            curve: 'basis',
            htmlLabels: true,
            padding: 20
        },
        themeVariables: {
            fontSize: '16px',
            fontFamily: "'Pretendard Variable', Pretendard, sans-serif"
        }
    });

    // Load and render data
    const loaded = await loadData();
    if (loaded) {
        renderSummaryCards();
        renderPhaseCards();
        applyFiltersAndRender(); // Use new filter-based rendering
        renderPriorityChart();
        renderTechChart();
        initFilterControls();
        initPaginationControls();
        initDetailPanel();
        // Mermaid, DataDict, Modernization은 탭 전환 시 lazy-load
    } else {
        // Clear skeletons and show error
        ['totalPrograms', 'totalLines', 'avgComplexity', 'db2Programs', 'copybookCount', 'vsamCount'].forEach(id => {
            const el = document.getElementById(id);
            if (el) el.textContent = '-';
        });
        [1, 2, 3].forEach(n => {
            const el = document.getElementById(`phase${n}Stats`);
            if (el) el.innerHTML = '<span style="color:var(--text-muted);font-size:0.85rem;">데이터 없음</span>';
        });
        document.getElementById('programTableBody').innerHTML = `
            <tr class="no-data-row"><td colspan="10">
                데이터를 불러올 수 없습니다. CATM 분석을 먼저 실행하세요.
            </td></tr>
        `;
    }
}

// ===================================
// Advanced Filter Controls
// ===================================

function initFilterControls() {
    const filterToggleBtn = document.getElementById('filterToggleBtn');
    const filterPanel = document.getElementById('filterPanel');
    const applyFilterBtn = document.getElementById('applyFilterBtn');
    const resetFilterBtn = document.getElementById('resetFilterBtn');

    if (filterToggleBtn && filterPanel) {
        filterToggleBtn.addEventListener('click', () => {
            const isVisible = filterPanel.style.display !== 'none';
            filterPanel.style.display = isVisible ? 'none' : 'block';
            filterToggleBtn.classList.toggle('active', !isVisible);
        });
    }

    if (applyFilterBtn) {
        applyFilterBtn.addEventListener('click', () => {
            readFiltersFromUI();
            state.currentPage = 1;
            applyFiltersAndRender();
        });
    }

    if (resetFilterBtn) {
        resetFilterBtn.addEventListener('click', () => {
            resetFilters();
            state.currentPage = 1;
            applyFiltersAndRender();
        });
    }
}

function readFiltersFromUI() {
    state.filters.complexityMin = parseInt(document.getElementById('complexityMin')?.value || 0);
    state.filters.complexityMax = parseInt(document.getElementById('complexityMax')?.value || 100);
    state.filters.linesMin = parseInt(document.getElementById('linesMin')?.value || 0);
    state.filters.linesMax = parseInt(document.getElementById('linesMax')?.value || 10000);
    state.filters.showDB2 = document.getElementById('filterDB2')?.checked ?? true;
    state.filters.showCICS = document.getElementById('filterCICS')?.checked ?? true;
    state.filters.showVSAM = document.getElementById('filterVSAM')?.checked ?? true;
    state.filters.showPhase1 = document.getElementById('filterPhase1')?.checked ?? true;
    state.filters.showPhase2 = document.getElementById('filterPhase2')?.checked ?? true;
    state.filters.showPhase3 = document.getElementById('filterPhase3')?.checked ?? true;
}

function resetFilters() {
    document.getElementById('complexityMin').value = 0;
    document.getElementById('complexityMax').value = 100;
    document.getElementById('linesMin').value = 0;
    document.getElementById('linesMax').value = 10000;
    document.getElementById('filterDB2').checked = true;
    document.getElementById('filterCICS').checked = true;
    document.getElementById('filterVSAM').checked = true;
    document.getElementById('filterPhase1').checked = true;
    document.getElementById('filterPhase2').checked = true;
    document.getElementById('filterPhase3').checked = true;

    state.filters = {
        complexityMin: 0,
        complexityMax: 100,
        linesMin: 0,
        linesMax: 10000,
        showDB2: true,
        showCICS: true,
        showVSAM: true,
        showPhase1: true,
        showPhase2: true,
        showPhase3: true
    };
}

function getPhase(score) {
    const t = CONFIG.phaseThresholds;
    if (score <= t.phase1.max) return 1;
    if (score <= t.phase2.max) return 2;
    return 3;
}

function applyFiltersAndRender() {
    const searchTerm = document.getElementById('searchInput')?.value?.toLowerCase() || '';
    const sortBy = document.getElementById('sortSelect')?.value || 'name';
    const f = state.filters;

    // Apply filters
    let filtered = state.programs.filter(prog => {
        if (prog.name === '.GITKEEP') return false;
        if (!prog.name.toLowerCase().includes(searchTerm)) return false;

        const complexity = prog.complexity || 0;
        const lines = prog.line_count || 0;
        const score = prog.scores?.final || 0;
        const phase = getPhase(score);

        if (complexity < f.complexityMin || complexity > f.complexityMax) return false;
        if (lines < f.linesMin || lines > f.linesMax) return false;

        // Tech filter - if none checked, show all
        const anyTechChecked = f.showDB2 || f.showCICS || f.showVSAM;
        if (anyTechChecked) {
            const hasDB2 = prog.has_db2;
            const hasCICS = prog.has_cics;
            const hasVSAM = prog.has_vsam;
            const noTech = !hasDB2 && !hasCICS && !hasVSAM;

            if (!noTech) {
                if (hasDB2 && !f.showDB2) return false;
                if (hasCICS && !f.showCICS) return false;
                if (hasVSAM && !f.showVSAM) return false;
            }
        }

        // Phase filter
        if (phase === 1 && !f.showPhase1) return false;
        if (phase === 2 && !f.showPhase2) return false;
        if (phase === 3 && !f.showPhase3) return false;

        return true;
    });

    // Sort
    filtered.sort((a, b) => {
        switch (sortBy) {
            case 'complexity': return (b.complexity || 0) - (a.complexity || 0);
            case 'lines': return (b.line_count || 0) - (a.line_count || 0);
            case 'score': return (b.scores?.final || 0) - (a.scores?.final || 0);
            default: return a.name.localeCompare(b.name);
        }
    });

    state.filteredPrograms = filtered;
    renderPaginatedTable();
    updatePaginationUI();
}

// ===================================
// Pagination Controls
// ===================================

function initPaginationControls() {
    const pageSizeSelect = document.getElementById('pageSizeSelect');
    const prevBtn = document.getElementById('prevPageBtn');
    const nextBtn = document.getElementById('nextPageBtn');

    if (pageSizeSelect) {
        pageSizeSelect.addEventListener('change', (e) => {
            state.pageSize = parseInt(e.target.value);
            state.currentPage = 1;
            renderPaginatedTable();
            updatePaginationUI();
        });
    }

    if (prevBtn) {
        prevBtn.addEventListener('click', () => {
            if (state.currentPage > 1) {
                state.currentPage--;
                renderPaginatedTable();
                updatePaginationUI();
            }
        });
    }

    if (nextBtn) {
        nextBtn.addEventListener('click', () => {
            const totalPages = Math.ceil(state.filteredPrograms.length / state.pageSize);
            if (state.currentPage < totalPages) {
                state.currentPage++;
                renderPaginatedTable();
                updatePaginationUI();
            }
        });
    }

    // Search input with debounce
    const searchInput = document.getElementById('searchInput');
    if (searchInput) {
        let debounceTimer;
        searchInput.addEventListener('input', () => {
            clearTimeout(debounceTimer);
            debounceTimer = setTimeout(() => {
                state.currentPage = 1;
                applyFiltersAndRender();
            }, 300);
        });
    }

    // Sort select
    const sortSelect = document.getElementById('sortSelect');
    if (sortSelect) {
        sortSelect.addEventListener('change', () => {
            state.currentPage = 1;
            applyFiltersAndRender();
        });
    }
}

function renderPaginatedTable() {
    const tbody = document.getElementById('programTableBody');
    if (!tbody) return;

    const startIdx = (state.currentPage - 1) * state.pageSize;
    const endIdx = startIdx + state.pageSize;
    const pageData = state.filteredPrograms.slice(startIdx, endIdx);

    if (pageData.length === 0) {
        tbody.innerHTML = `<tr class="no-data-row"><td colspan="10">
            검색 결과가 없습니다.
        </td></tr>`;
        return;
    }

    tbody.innerHTML = pageData.map(prog => {
        const method = prog.recommendation?.method || '';
        const badgeClass = getMethodBadgeClass(method);

        return `
        <tr>
            <td><strong>${prog.name}</strong></td>
            <td>${(prog.line_count || 0).toLocaleString()}</td>
            <td>${prog.complexity || 0}</td>
            <td>${prog.calls?.length || 0}</td>
            <td>${prog.copies?.length || 0}</td>
            <td>${prog.has_db2 ? '<span class="check-icon">&#10003;</span>' : '<span class="cross-icon">&mdash;</span>'}</td>
            <td>${prog.has_cics ? '<span class="check-icon">&#10003;</span>' : '<span class="cross-icon">&mdash;</span>'}</td>
            <td>${prog.scores?.final ? `<span class="badge badge-score">${prog.scores.final.toFixed(1)}</span>` : '<span class="cross-icon">&mdash;</span>'}</td>
            <td>${method ? `<span class="badge ${badgeClass}">${method}</span>` : '<span class="cross-icon">&mdash;</span>'}</td>
            <td><button class="view-detail-btn" onclick="openDetailPanel('${prog.name}')">상세</button></td>
        </tr>`;
    }).join('');
}

function updatePaginationUI() {
    const total = state.filteredPrograms.length;
    const totalPages = Math.max(1, Math.ceil(total / state.pageSize));
    const startIdx = (state.currentPage - 1) * state.pageSize + 1;
    const endIdx = Math.min(state.currentPage * state.pageSize, total);

    const paginationInfo = document.getElementById('paginationInfo');
    const pageNumber = document.getElementById('pageNumber');
    const prevBtn = document.getElementById('prevPageBtn');
    const nextBtn = document.getElementById('nextPageBtn');

    if (paginationInfo) {
        paginationInfo.textContent = total > 0 ? `${total}개 중 ${startIdx}-${endIdx}` : '0개';
    }
    if (pageNumber) {
        pageNumber.textContent = `${state.currentPage} / ${totalPages}`;
    }
    if (prevBtn) {
        prevBtn.disabled = state.currentPage <= 1;
    }
    if (nextBtn) {
        nextBtn.disabled = state.currentPage >= totalPages;
    }
}

// ===================================
// Detail Panel
// ===================================

function initDetailPanel() {
    const closeBtn = document.getElementById('detailCloseBtn');
    const overlay = document.getElementById('detailOverlay');

    if (closeBtn) {
        closeBtn.addEventListener('click', closeDetailPanel);
    }
    if (overlay) {
        overlay.addEventListener('click', closeDetailPanel);
    }

    // ESC key to close
    document.addEventListener('keydown', (e) => {
        if (e.key === 'Escape') {
            closeDetailPanel();
        }
    });
}

function openDetailPanel(programName) {
    const panel = document.getElementById('detailPanel');
    const overlay = document.getElementById('detailOverlay');
    const titleEl = document.getElementById('detailProgramName');
    const contentEl = document.getElementById('detailPanelContent');

    if (!panel || !overlay) return;

    const program = state.programs.find(p => p.name === programName);
    if (!program) return;

    // Set title
    if (titleEl) {
        titleEl.textContent = `📄 ${programName}`;
    }

    // Render content
    if (contentEl) {
        contentEl.innerHTML = renderDetailContent(program);
    }

    // Open panel
    panel.classList.add('open');
    overlay.classList.add('open');
    document.body.style.overflow = 'hidden';
}

function closeDetailPanel() {
    const panel = document.getElementById('detailPanel');
    const overlay = document.getElementById('detailOverlay');

    if (panel) panel.classList.remove('open');
    if (overlay) overlay.classList.remove('open');
    document.body.style.overflow = '';
}

function renderDetailContent(program) {
    const score = program.scores?.final || 0;
    const phase = getPhase(score);
    const phaseLabel = phase === 1 ? 'Phase 1 (즉시 전환)' : phase === 2 ? 'Phase 2 (3개월)' : 'Phase 3 (6개월)';

    let html = `
        <div class="detail-section">
            <h3 class="detail-section-title">기본 정보</h3>
            <div class="detail-info-grid">
                <div class="detail-info-item">
                    <div class="detail-info-label">라인 수</div>
                    <div class="detail-info-value">${(program.line_count || 0).toLocaleString()}</div>
                </div>
                <div class="detail-info-item">
                    <div class="detail-info-label">복잡도</div>
                    <div class="detail-info-value">${program.complexity || 0}</div>
                </div>
                <div class="detail-info-item">
                    <div class="detail-info-label">우선순위 점수</div>
                    <div class="detail-info-value">${score ? score.toFixed(1) : '-'}</div>
                </div>
                <div class="detail-info-item">
                    <div class="detail-info-label">전환 단계</div>
                    <div class="detail-info-value" style="font-size:0.9rem;">${phaseLabel}</div>
                </div>
            </div>
        </div>

        <div class="detail-section">
            <h3 class="detail-section-title">기술 스택</h3>
            <div class="checkbox-group" style="gap:1rem;">
                <span class="badge ${program.has_db2 ? 'badge-auto' : ''}" style="opacity:${program.has_db2 ? 1 : 0.4}">DB2</span>
                <span class="badge ${program.has_cics ? 'badge-hybrid' : ''}" style="opacity:${program.has_cics ? 1 : 0.4}">CICS</span>
                <span class="badge ${program.has_vsam ? 'badge-rewrite' : ''}" style="opacity:${program.has_vsam ? 1 : 0.4}">VSAM</span>
            </div>
        </div>
    `;

    // CALL dependencies
    if (program.calls?.length > 0) {
        html += `
            <div class="detail-section">
                <h3 class="detail-section-title">호출 프로그램 (CALL)</h3>
                <ul class="detail-list">
                    ${program.calls.map(c => `<li>${c}</li>`).join('')}
                </ul>
            </div>
        `;
    }

    // COPY dependencies
    if (program.copies?.length > 0) {
        html += `
            <div class="detail-section">
                <h3 class="detail-section-title">COPYBOOK (COPY)</h3>
                <ul class="detail-list">
                    ${program.copies.map(c => `<li>${c}</li>`).join('')}
                </ul>
            </div>
        `;
    }

    // DB2 Tables
    if (program.db2_tables?.length > 0) {
        html += `
            <div class="detail-section">
                <h3 class="detail-section-title">DB2 테이블</h3>
                <ul class="detail-list">
                    ${program.db2_tables.map(t => `<li>${t}</li>`).join('')}
                </ul>
            </div>
        `;
    }

    // Recommendation
    if (program.recommendation) {
        html += `
            <div class="detail-section">
                <h3 class="detail-section-title">전환 권장 사항</h3>
                <div class="detail-info-item" style="margin-bottom:0.75rem;">
                    <div class="detail-info-label">전환 방식</div>
                    <div class="detail-info-value">${program.recommendation.method || '-'}</div>
                </div>
                <p style="font-size:0.85rem;color:var(--text-secondary);margin:0;">${program.recommendation.reason || ''}</p>
            </div>
        `;
    }

    // Link to docs
    html += `
        <div class="detail-section">
            <h3 class="detail-section-title">분석 문서</h3>
            <button class="detail-btn" onclick="window.open('output/docs/${program.name}.md', '_blank')">
                📄 ${program.name}.md 열기
            </button>
        </div>
    `;

    return html;
}

document.addEventListener('DOMContentLoaded', init);
