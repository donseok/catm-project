/**
 * CATM Dashboard - Main Application
 * COBOL Analysis Task Manager Dashboard
 */

// ===================================
// Configuration
// ===================================

const CONFIG = {
    dataPath: 'output/',
    files: {
        dependency: 'dependency-scan.json',
        priority: 'reports/priority_data.json',
        inventory: 'reports/inventory.json',
        analysisLog: 'reports/analysis_log.json'
    }
};

// ===================================
// State
// ===================================

let state = {
    programs: [],
    priorityData: [],
    summary: {},
    scanDate: null
};

// ===================================
// Data Loading
// ===================================

async function loadData() {
    try {
        // Load dependency scan data
        const depResponse = await fetch(CONFIG.dataPath + CONFIG.files.dependency);
        const depData = await depResponse.json();
        state.programs = depData.programs || [];
        state.summary = depData.summary || {};
        state.scanDate = depData.scan_date;

        // Load priority data
        const priorityResponse = await fetch(CONFIG.dataPath + CONFIG.files.priority);
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

        return true;
    } catch (error) {
        console.error('데이터 로드 오류:', error);
        return false;
    }
}

// ===================================
// Rendering Functions
// ===================================

function renderSummaryCards() {
    document.getElementById('totalPrograms').textContent = state.summary.total_programs || 0;
    document.getElementById('totalLines').textContent = (state.summary.total_lines || 0).toLocaleString();
    document.getElementById('avgComplexity').textContent = (state.summary.avg_complexity || 0).toFixed(1);
    document.getElementById('db2Programs').textContent = state.summary.db2_programs || 0;

    if (state.scanDate) {
        const date = new Date(state.scanDate);
        document.getElementById('scanDate').textContent = `마지막 스캔: ${date.toLocaleString('ko-KR')}`;
    }
}

function renderProgramTable() {
    const tbody = document.getElementById('programTableBody');
    const searchTerm = document.getElementById('searchInput').value.toLowerCase();
    const sortBy = document.getElementById('sortSelect').value;

    // Filter
    let filtered = state.programs.filter(prog =>
        prog.name.toLowerCase().includes(searchTerm) &&
        prog.name !== '.GITKEEP'
    );

    // Sort
    filtered.sort((a, b) => {
        switch (sortBy) {
            case 'complexity': return (b.complexity || 0) - (a.complexity || 0);
            case 'lines': return (b.line_count || 0) - (a.line_count || 0);
            case 'score': return (b.scores?.final || 0) - (a.scores?.final || 0);
            default: return a.name.localeCompare(b.name);
        }
    });

    // Render
    tbody.innerHTML = filtered.map(prog => `
        <tr>
            <td><strong>${prog.name}</strong></td>
            <td>${prog.line_count || 0}</td>
            <td>${prog.complexity || 0}</td>
            <td>${prog.calls?.length || 0}</td>
            <td>${prog.copies?.length || 0}</td>
            <td>${prog.has_db2 ? '<span class="check-icon">✅</span>' : '<span class="cross-icon">-</span>'}</td>
            <td>${prog.has_cics ? '<span class="check-icon">✅</span>' : '<span class="cross-icon">-</span>'}</td>
            <td>${prog.scores?.final ? `<span class="badge badge-primary">${prog.scores.final.toFixed(1)}</span>` : '-'}</td>
            <td>${prog.recommendation?.method ? `<span class="badge badge-success">${prog.recommendation.method}</span>` : '-'}</td>
        </tr>
    `).join('');
}

// ===================================
// Charts
// ===================================

let priorityChartInstance = null;
let techChartInstance = null;

function renderPriorityChart() {
    const ctx = document.getElementById('priorityChart').getContext('2d');
    const validPrograms = state.priorityData.filter(p => p.name !== '.GITKEEP');

    if (priorityChartInstance) {
        priorityChartInstance.destroy();
    }

    const isDark = document.documentElement.getAttribute('data-theme') === 'dark';
    const textColor = isDark ? '#f1f5f9' : '#1e293b';
    const gridColor = isDark ? 'rgba(148, 163, 184, 0.15)' : 'rgba(148, 163, 184, 0.2)';

    priorityChartInstance = new Chart(ctx, {
        type: 'bar',
        data: {
            labels: validPrograms.map(p => p.name),
            datasets: [
                {
                    label: '비즈니스 중요도',
                    data: validPrograms.map(p => p.scores?.business_importance || 0),
                    backgroundColor: 'rgba(59, 130, 246, 0.7)',
                    borderRadius: 4
                },
                {
                    label: '기술 복잡도',
                    data: validPrograms.map(p => p.scores?.technical_complexity || 0),
                    backgroundColor: 'rgba(139, 92, 246, 0.7)',
                    borderRadius: 4
                },
                {
                    label: '의존성 영향도',
                    data: validPrograms.map(p => p.scores?.dependency_impact || 0),
                    backgroundColor: 'rgba(16, 185, 129, 0.7)',
                    borderRadius: 4
                }
            ]
        },
        options: {
            responsive: true,
            maintainAspectRatio: false,
            plugins: {
                legend: {
                    position: 'bottom',
                    labels: { color: textColor }
                }
            },
            scales: {
                x: {
                    ticks: { color: textColor },
                    grid: { color: gridColor }
                },
                y: {
                    beginAtZero: true,
                    max: 10,
                    ticks: { color: textColor },
                    grid: { color: gridColor }
                }
            }
        }
    });
}

function renderTechChart() {
    const ctx = document.getElementById('techChart').getContext('2d');

    if (techChartInstance) {
        techChartInstance.destroy();
    }

    const isDark = document.documentElement.getAttribute('data-theme') === 'dark';
    const textColor = isDark ? '#f1f5f9' : '#1e293b';

    techChartInstance = new Chart(ctx, {
        type: 'doughnut',
        data: {
            labels: ['DB2 사용', 'VSAM 사용', 'CICS 사용', '단순 프로그램'],
            datasets: [{
                data: [
                    state.summary.db2_programs || 0,
                    state.summary.vsam_programs || 0,
                    state.summary.cics_programs || 0,
                    Math.max(0, (state.summary.total_programs || 0) -
                        (state.summary.db2_programs || 0) -
                        (state.summary.vsam_programs || 0) -
                        (state.summary.cics_programs || 0))
                ],
                backgroundColor: [
                    'rgba(139, 92, 246, 0.8)',
                    'rgba(59, 130, 246, 0.8)',
                    'rgba(245, 158, 11, 0.8)',
                    'rgba(148, 163, 184, 0.5)'
                ],
                borderWidth: 0
            }]
        },
        options: {
            responsive: true,
            maintainAspectRatio: false,
            plugins: {
                legend: {
                    position: 'bottom',
                    labels: { color: textColor }
                }
            }
        }
    });
}

// ===================================
// Dependency Graph
// ===================================

function renderDependencyGraph() {
    const validPrograms = state.programs.filter(p => p.name !== '.GITKEEP');

    let mermaidCode = 'graph TD\n';
    mermaidCode += '    classDef mainPgm fill:#3b82f6,stroke:#1e40af,color:#fff\n';
    mermaidCode += '    classDef subPgm fill:#10b981,stroke:#047857,color:#fff\n';
    mermaidCode += '    classDef copybook fill:#f59e0b,stroke:#b45309,color:#fff\n';
    mermaidCode += '    classDef db2 fill:#8b5cf6,stroke:#6d28d9,color:#fff\n\n';

    validPrograms.forEach(prog => {
        // CALL relationships
        prog.calls?.forEach(call => {
            mermaidCode += `    ${prog.name}[${prog.name}] -->|CALL| ${call}[${call}]\n`;
        });

        // COPY relationships
        prog.copies?.forEach(copy => {
            mermaidCode += `    ${prog.name} -.->|COPY| CPY_${copy}(${copy})\n`;
        });

        // DB2 relationships
        prog.db2_tables?.forEach(table => {
            mermaidCode += `    ${prog.name} <-->|SQL| DB_${table}[(${table})]\n`;
        });
    });

    // Apply classes
    validPrograms.forEach(prog => {
        mermaidCode += `    class ${prog.name} mainPgm\n`;
    });

    const graphContainer = document.getElementById('dependencyGraph');
    graphContainer.textContent = mermaidCode;

    // Re-render Mermaid
    mermaid.init(undefined, graphContainer);
}

// ===================================
// Theme Toggle
// ===================================

function initTheme() {
    const savedTheme = localStorage.getItem('catm-theme') || 'light';
    document.documentElement.setAttribute('data-theme', savedTheme);
}

function toggleTheme() {
    const current = document.documentElement.getAttribute('data-theme');
    const next = current === 'dark' ? 'light' : 'dark';
    document.documentElement.setAttribute('data-theme', next);
    localStorage.setItem('catm-theme', next);

    // Re-render charts with new theme colors
    renderPriorityChart();
    renderTechChart();
}

// ===================================
// Event Listeners
// ===================================

function initEventListeners() {
    document.getElementById('themeToggle').addEventListener('click', toggleTheme);
    document.getElementById('searchInput').addEventListener('input', renderProgramTable);
    document.getElementById('sortSelect').addEventListener('change', renderProgramTable);
}

// ===================================
// Initialization
// ===================================

async function init() {
    initTheme();
    initEventListeners();

    // Initialize Mermaid
    mermaid.initialize({
        startOnLoad: false,
        theme: 'default',
        securityLevel: 'loose'
    });

    // Load and render data
    const loaded = await loadData();
    if (loaded) {
        renderSummaryCards();
        renderProgramTable();
        renderPriorityChart();
        renderTechChart();
        renderDependencyGraph();
    } else {
        document.getElementById('programTableBody').innerHTML = `
            <tr><td colspan="9" style="text-align: center; color: var(--text-muted);">
                데이터를 불러올 수 없습니다. CATM 분석을 먼저 실행하세요.
            </td></tr>
        `;
    }
}

// Start application
document.addEventListener('DOMContentLoaded', init);
