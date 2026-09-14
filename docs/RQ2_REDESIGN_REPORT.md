# RQ2 后半部分：独立重构与实验报告

日期：2026-09-14。完整方法见 [RQ2_CONDITIONAL_RELIABILITY.md](RQ2_CONDITIONAL_RELIABILITY.md)，
实验及失败路线见 [RQ2_INFORMATION_EXPERIMENTS.md](RQ2_INFORMATION_EXPERIMENTS.md)。

## 推荐版本

主问题改为 **context-conditioned measurement reliability**：给定目标表征与容差，
可获取的 context 能否识别测量在哪些条件下更可靠？主要统计对象为
`Pr(|z| > epsilon | information)`，同时拟合 `E(|z| | information)` 定义风险组。
不再以逐日 signed-error correction 或 Shapley recovery 为主图中心。

主图保留三个层次：

1. 在新参与者上，由训练集定义的 context 风险组是否对应不同实际失真；
2. context 相对配置平均值的信息价值，与在已有测量之上的增量是否不同；
3. 同一容差在不同 context 风险组下对应怎样的实际超容差概率。

这条路线延续 Fig.3 的 context structure，并把它转成目标/容差相关的可靠性信息。
它尚未证明 context 能普遍替代更多测量，或显著降低 joint measurement burden。
这些边界直接写入主图说明、方法和敏感性结果。

## 最清楚的实证信号

采用 52 个日尺度指标、8 个对比，共 414 个可估计任务，2 个 LIGHT 不可用任务。
全部任务使用同一 16 个 signature、50 个 context 特征字典、同一模型规则与容差集合。
运行三次按地点分层的 participant-grouped five-fold OOS，并以 1,000 次参与者配对
bootstrap 计算主划分的区间。Bootstrap 条件于已拟合预测，并非重拟合区间。

| 对比 | Context 的 Brier skill，% | 95% 参与者区间 | 低/高风险组实际平均失真，相对未分组均值 |
|---|---:|---:|---:|
| Chest → eye | 1.86 | 1.48–2.25 | 0.83 / 1.14 |
| Wrist → eye | 1.50 | 1.05–1.92 | 0.82 / 1.19 |
| LIGHT → MEDI | 2.78 | 2.33–3.21 | 0.71 / 1.37 |
| 20 → 10 s | 1.72 | 0.99–2.32 | 0.74 / 1.25 |
| 30 → 10 s | 2.06 | 1.47–2.62 | 0.76 / 1.26 |
| 40 → 10 s | 1.86 | 1.39–2.33 | 0.81 / 1.23 |
| 60 → 10 s | 2.14 | 1.66–2.58 | 0.75 / 1.25 |
| 120 → 10 s | 2.01 | 1.59–2.42 | 0.80 / 1.23 |

Brier skill 的基线是训练配置均值；不是相对强测量模型的增益。三次划分中，八个
对比的 context skill 方向均一致。Context 同时优于训练地点均值基线，主划分改善
约 1.84%–2.86%，所以结果不只是一个地点标签的替代。

在 ε=0.2 时，风险差异具有直接的容差解释：

| 对比 | 低风险组超容差率 | 高风险组超容差率 | 高减低差异的 95% 区间，百分点 |
|---|---:|---:|---:|
| Chest → eye | 36.6% | 46.6% | 7.85–12.08 |
| Wrist → eye | 45.0% | 56.3% | 8.62–13.86 |
| LIGHT → MEDI | 16.4% | 28.9% | 10.78–14.15 |
| 20 → 10 s | 7.2% | 11.8% | 3.66–5.44 |
| 30 → 10 s | 10.5% | 16.2% | 4.61–6.84 |
| 40 → 10 s | 13.2% | 18.9% | 4.54–6.85 |
| 60 → 10 s | 15.8% | 23.5% | 6.41–8.96 |
| 120 → 10 s | 22.2% | 30.1% | 6.57–9.21 |

这张表只是六个已报告容差中的一个解释示例；模型和图保留全部容差。
每个数字先在单个 metric 内计算，再以 metric 等权汇总，不能直接用汇总比例给任意
单项 scientific target 制定测量规则。低/中/高组实际覆盖各约三分之一的 metric-days；
没有删除困难日期后把剩余样本包装为全队列充分。
风险组按 metric × contrast 分别拟合，但使用完全相同的规则；它们不是一套对所有
指标都相同的日期分组。同一天可以对某项表征属于高风险，对另一项属于低风险。

## “哪些 context”具有解释意义

完整 50 特征构成已导出，而不是只保留有利变量。按各任务可用样本 SD 标准化后，
placement 高风险组相对低风险组有更多太阳辐射和户外报告：chest 的 radiation、outdoor
差异约 +0.43、+0.40 SD；wrist 约 +0.24、+0.30 SD。Optical 高风险组的对应方向相反，
约 −0.17、−0.14 SD。Temporal radiation 差异为正，但 outdoor 差异较小。

这些是模型定义风险组的描述性构成，不是独立因果效应、变量重要性排序或每个指标的
统一物理机制。其意义在于：不存在一个可以对所有配置通用的“好天气/坏天气”充分性规则。

## 在已有测量之上的独立增量：需要保留的限制

保留测量模型，再增加正交化 context block 后，Brier 增益为 LIGHT 0.676%、chest
0.340%、wrist 0.300%；temporal 接近零。三次划分中 optical/placement 的方向一致。
但只有 LIGHT 和 chest 的主 bootstrap 区间高于零，wrist 区间跨零。

更严格的模型容量控制显示：二十自由度的 measurement-only 模型优于十自由度测量
加十自由度 context 的模型，尤其在 optical 和 temporal 中。Placement 的比较点估计
仍约 +0.4%，但区间跨零。因此不能把第一组小增量称为已经识别的信息论独立贡献。

这也解释了为何以前更强的 baseline 会压低 context gain：一部分 context 可用结构
已被个人测量表达。新设计把这件事显示出来，同时保留 context 在尚未以个人测量
条件化时的稳定风险信息。没有通过弱化 baseline 来制造大的 context 贡献。

## 尝试后未被采用的路线

- 旧 observability 预测的风险排序：有风险排序能力，但 context 增量普遍不明显。
- 固定总复杂度直接拼接 S/C：造成预测器竞争，Brier 表现下降；因此测试保留测量
  基线的正交 context block，而非继续调整 XGBoost。
- 基于 context 的 coarsest-passing cadence 策略：在完整 temporal 配对支持上没有
  显示足够强的误差—采样量优势，不进入主图，不宣称 adaptive burden savings。
- 首个配对日校准后续日期：所有对比的平均误差变差。例如 chest 从 0.356 增至
  0.512，LIGHT 从 0.159 增至 0.210。该结果反对本次测试的未收缩单日校准规则，
  不证明个体异质性不存在，也不排除更充分的个体校准。

没有采用逐 metric/transition 特征、outer-test 超参数选择、只展示有利容差、或以
零 clipping 掩盖负增益。整个过程是分析开发，重复划分是稳定性检验，不是独立外部验证。

## 与 RQ3 的衔接

推荐全文表述：配置引起失真；context 让给定目标与容差下的可靠性具有可预测的条件
差异；RQ3 在明确目标、容差及配置域内确定最低必要测量负担。

当前 RQ3 的 estimand 仍为所有更高观测配置的最大平均变化，且包括 duration；这里
研究的是日尺度对固定锚点的超容差概率。两者通过“目标/容差依赖的可靠性”衔接，
并不是把模型剩余误差代入 RQ3，或证明“不可消除误差决定最低负担”。

## 代码、结果与验证

- `scripts/12d_rq2_recovery.R --run`：新的 canonical analysis；旧 recovery 仅以
  `--legacy-run` 保留。新分析不依赖 XGBoost。
- `scripts/utils/rq2_risk_models.R`：统一 spline/ridge、增量 block、概率单调投影。
- `scripts/utils/rq2_conditional_reliability.R`：输入契约、三次 OOS、缓存、汇总和 bootstrap。
- `scripts/13b_plot_fig4.R`：只读取冻结结果，输出 Fig.4 和两张补充图。
- `scripts/run_downstream_server.sh`：已接入 analysis → frozen plotting 顺序。
- `scripts/diagnose_rq2_*.R` 与 `scripts/summarize_rq2_information.R`：可重现的诊断和失败路线。

主结果入口为 `results/rq2/rq2_conditional_reliability.rds`；完整 run 为
`results/rq2/reliability/<rq1_version>/5a621f68840b56126caf8b01f1ec5990/`。
主图为 `results/figures/Fig4_RQ2.png`。旧主图和旧绘图源码已归档在
`results/rq2/information_diagnostics/`，历史 recovery 输入与预测未覆盖。

已通过数值契约测试、全部冻结任务的支持/分组/概率审计、图号注册和历史输入路径测试。
三张实际 PNG 均经过查看，无裁切或丢失图例。Core、RQ1、Fig.3 和 RQ3 均未重算。
方法从 learner-library/early-stopping 搜索简化为固定的小矩阵拟合，并沿用 PSOCK
任务缓存；避免为了后续图形修改重复拟合。
本次本地运行每任务包含三次完整分组划分，耗时中位数 3.08 s，范围 2.29–4.13 s；
完整任务耗时表已写入 run 下的 `task_status.csv`。旧模型计时来自不同运行环境，
因此不将二者比值报告为严格的加速倍数。

本地旧 SDOC 稿保留不动。新增 `manuscript/rq2_conditional_reliability.md` 提供可并入
正文的方法、结果与图注；不把旧稿中已废弃的 sampling/duration 定义继续沿用。
