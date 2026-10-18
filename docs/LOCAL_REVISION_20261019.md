# 2026-10-19 轻量修复与本地执行记录

本轮未重新提取原始 10 秒数据，未重建日级指标、小时基底或天气 Core，未运行生产模型拟合或生产 bootstrap。

## 已修改并执行

| 内容 | 实施与结果 |
| --- | --- |
| 单日 IS | 按完整日期数判定不可用；23/24/25 小时单日均适用。单日 IV、有效 2–6 日数值保留。旧 v2 时长块可确定性迁移，读取端也会屏蔽错误值。 |
| RQ3 冻结结果刷新 | 实际运行 `14_rq3_analysis.R --refresh-frozen-availability`，覆盖为 RQ3 v10；删除 720 条涉及单日 IS 的配对，剩余 128,070 条。36 个单日 IS 状态显式保留为 unavailable。逐键核验其余 R_obs/status 不变。 |
| 任务充分性 | All-54 和 Dynamics 在要求单日 IS 时 unavailable，充分性及 Pareto 决策为 NA。可用指标数量排除不可用 IS，resolved 指标仍定义频率分母。 |
| Fig. 5 / Fig. 6 | 已运行两个原有规范绘图入口，覆盖 PNG、展示表及图件清单；已查看 PNG，未发现新增排版问题。 |
| 连续信号诊断 | 已运行 `10c_export_continuous_diagnostics.R`，写入 7 张 CSV。保留 2,484 个 estimated 和 12 个 unavailable 任务。存储充分统计量重现原信号的最大绝对误差为 8.76e-14。 |
| 固定参考系数传播 | 从同一匹配支持的参与者内充分统计量计算 RMS(Wc beta_r − Wr beta_r)，无重拟合。它仍去除了稳定参与者偏移，不能解释为完整固定截距预测迁移。 |
| 连续预测损失 | 导出 mean_risk 的 MAE/MSE；每个目标内先平均预测损失，再等权汇总目标后求相对训练均值基线的 skill。保留所有切分和 null 信息状态。 |
| 共同参考与预算 | 已运行 `14c_rq3_common_reference.R`，输出 11,664 个完整目标—配置状态、同预算选择、连续损失—名义记录数描述性前沿、相邻差分和支持计数审计。 |

主要输出：

- `results/figures/Fig5_RQ3.png`、`results/figures/Fig6_RQ3.png`。
- `results/continuous_diagnostics/`：逐任务、逐结局、逐领域、逐对比的信号表及连续误差表，附定义和输入哈希。
- `results/rq3/common_reference/`：共同参考差异、预算表、RDS，以及输入/输出哈希和定义。
- `results/diagnostics/rq3_removed_single_day_is_pairs.csv`：剔除的旧配对。
- `results/diagnostics/rq3_availability_refresh_inputs.csv`：保留来源版本和哈希的追加式刷新审计。

共同参考是同 placement/optical 分面内的 10 s × 6 d 配置，沿用原支持特异尺度。现有配对汇总不能证明候选配置使用完全相同的父窗口身份和权重。因此预算结果是冻结汇总值的描述性比较；不能写成受控共同样本下的最优设计。负的相邻改善值原样保留。

## 版本与尚未刷新的范围

Core 基础版本保持 v5；时长可用性修订使用已有的 duration 版本字段 v3。后续完整 RQ1 生产者升级到 v7，阻止复用 v6 配对缓存；RQ2 时长模型要求 v7，上游版本已进入其检查点路径。

本地没有当前 manifest 引用的时长大块和 RQ1 配对大块，仅有汇总和清单。因此本轮**未全量刷新 RQ1 配对、RQ2 时长模型、Fig. 1、Fig. 3 和合并附录**。这些旧产物没有改贴新版本。对应绘图入口现在会拒绝过期的 v6 时长结果。

本轮 RQ3 v10 明确记录原始 RQ1 v6 来源：它通过可验证的单日 IS 删除和冻结投影刷新得到，不声称 RQ1 已完整重跑。日级 inference/reliability 不含 IS，本轮新增诊断保留其实际 v6 来源。Fig. 2、日级连续 MAE 导出可继续读取这些未变更的冻结结果。

补齐已有服务器缓存后，可直接从现有 v5 时长块运行 `10_rq1_analysis.R`，无需先重建日级 Core；随后运行依赖新 RQ1 的分析和绘图。若需要把旧时长块物理迁移为 v3，可使用已有 `CORE_DURATION_ONLY=1` 入口。只有缺失块需要从持久化日级值/小时基底重建，仍无需访问原始 10 秒序列。完整下游重跑应在缓存齐全的机器上进行；本记录不估计未经实测的运行时长。

## 验证

通过的专项检查：`validate_duration_windows`、`validate_core_sampling_hours`、`validate_core_pipeline_contract`、`validate_rq1_primary_duration`、`validate_rq3_support_pareto`、`validate_continuous_diagnostics`、`validate_rq3_common_reference`、`validate_inference_pipeline`、`validate_reliability_contract`、`validate_figure_registry`。另通过两工作进程的可用性 helper 检查、真实结果一致性检查及 `git diff --check`。

本地使用 R 4.5.0、LightLogR 0.10.3。当前沙箱中继承的临时目录不可写，执行进程将 TMPDIR/TMP/TEMP（压缩文件测试还包括 VROOM_TEMP_PATH）指向工作区临时目录。R 提示部分已安装包由较新补丁版本构建及 locale 警告，未导致上述检查或正式导出失败。
