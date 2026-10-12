ERA5 server bundle
==================
将整个目录上传到 Linux ECS 的数据盘，保持 code / models / weather 结构。
活动科学配置是 config.R。脚本自动使用当前 bundle 目录。

准备
----
需要可恢复现有 brms 拟合对象的 R 环境、C++ 工具链，以及以下 R 包：
ncdf4, digest, Rcpp, brms, posterior, rstan, dlnm, FNN, splines。
模型来源的包版本记录在 code/model_environment_versions.csv；它是历史环境记录，
不是自动安装锁文件。原始拟合和服务器预测的数值 QC 仍由 03_run_ecs.R 执行。

在 config.R 中预先填入 CDD 情景的 name/theta0/slope/kappa。
当前 workers=32；根据实际内存、单 worker 峰值和试跑速度调整。

命令（在本目录执行）
--------------------
  bash run_server.sh check
  bash run_server.sh pilot 2>&1 | tee logs/pilot.log
  bash run_server.sh full 2>&1 | tee logs/full.log
  bash run_server.sh verify

check 检查静态文件 SHA256、合成数值回归、十年气象包 MD5、模型文件存在性、
依赖和情景配置。pilot 使用相同科学设置和 2000 个抽样位置，只计算前 300 个格点，
结果写入 results_pilot；它用于端到端检查，不能当成全球结果或代表性速度估计。
full 写入 results。shell 管道若用于自动作业，请先执行 set -o pipefail，保留失败退出码。

续算
----
默认 resume="auto"：仅恢复未完成的同一版本运行；已完成运行不会重复启动。
改变情景、代码、mask、tile 或其他科学设置后，使用 resume="new" 开始新运行。
旧 v1 中途断点不能强行套用 v1.1 引擎。
config_pilot.R 继承 config.R，仅修改格点上限和输出目录。

文件与校验
----------
weather/packed_2010.nc ... packed_2019.nc：原始 0.25° 气象值的稀疏提取，不插值。
weather/pack_index.rds：格点索引、完整格点面积、人口、tile、日期和输入溯源。
weather/packed_YYYY.done.rds：年度完成记录；selected_cells.csv / tiles.csv 便于浏览。
SHA256SUMS.txt：代码、启动脚本、模型、pack_index 的内容校验。
WEATHER_MD5SUMS.txt：十年 NetCDF 的内容校验，可执行 md5sum -c WEATHER_MD5SUMS.txt。
可编辑的 config.R / config_pilot.R 不列入静态 SHA256，实际运行配置会记录在科学指纹内。

输出与取回
----------
results/run_*/global/：全域汇总、元数据、grid_partitions、checkpoints。
下载整个 results 到本地 ERA5_workflow/04_results；不要遗漏 checkpoints。
全球 CDD schema 为 v2：无合格数据的期次输出 NA 和 no_eligible_support 状态。
CDD 单位为 °C·day，面积加总单位为 km²·°C·day；不是电力消耗。
真实模型和 ECS 性能试跑尚未由此次本地预处理完成。
