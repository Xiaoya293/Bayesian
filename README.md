# Bayesian
MCMC   report
使用 R 语言对谢菲尔德 28 个行政区的 FGM 相对风险进行估计。根据二项先验分布和似然函数计算
后验分布，利用贝叶斯 Bradley–Terry 模型的 logit 函数形式建模比较概率。选择 Gibbs sampler with 
Metropolis-Hastings Random Walk 的 MCMC 算法，并通过调整步长参数优化链的混合性。通过后验样本
估计每个区域的风险参数及其 95%可信区间，并识别城市中的高风险区。
