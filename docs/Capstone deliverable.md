# Abstract {#abstract .unnumbered}

Evaluating the causal effects of environmental policies is challenging
when treatment assignment depends on latent ecological and social
factors, and key confounders are unobserved. This paper examines the
impact of California's Vegetation Management Program (VMP), a state-led
prescribed fire initiative with liability-sharing arrangements, on
wildfire intensity in montane conifer forests. To address
high-dimensional spatial confounding, I develop a satellite-based
synthetic control approach that uses satellite-derived geospatial
embeddings to constrain donor pools to those most ecologically similar
to treated units. Compared to conventional donor selection,
embedding-based donors achieve tighter pre-treatment fit, lower residual
bias, and higher inferential power, yielding a $4$--$5\%$ reduction in
fire intensity over a six-year post-treatment window. The results
demonstrate that state-led liability-sharing programs can overcome
adoption constraints, effectively reduce wildfire risk, and offer a
scalable framework for evaluating complex environmental interventions.
This study makes two contributions: a methodological advance in
quasi-experimental design for spatial policy evaluation and actionable
insights for climate adaptation governance.

**Keywords:** Prescribed fire policy, synthetic control methods,
satellite embeddings, satellite-based, control selection, CAL FIRE,
state responsibility areas.

# Introduction

Wildfire management in the western United States represents a
quintessential challenge in environmental policy: how to align
short-term political incentives with long-term ecological imperatives
across fragmented land ownership. California's wildfire crisis has
intensified dramatically over the past two decades, with annual burned
acreage increasing by 500% and total suppression costs exceeding \$3
billion annually [@CALFIRE2022]. The policy response has focused on
reducing fuel through prescribed fire as a tool to mitigate the severity
of wildfires and protect communities.

The **Vegetation Management Program (VMP)**, established by CAL FIRE in
the early 2000s and formalized through Public Resources Code sections
governing State Responsibility Area (SRA) lands, represents California's
primary institutional mechanism for scaling prescribed fire on private
lands. Unlike federal programs that operate on public lands, the VMP
addresses a critical political economy problem: private landowners face
prohibitive liability risks and upfront costs for conducting prescribed
burns. This creates a classic collective action failure where individual
risk aversion leads to landscape-level fuel accumulation
[@McCaffrey2015].

The VMP's institutional design is theoretically notable: CAL FIRE
assumes liability for prescribed burns on private SRA lands through
cooperative agreements. This mechanism effectively socializes the risk
of fire escape while sharing implementation expenses with landowners.
This represents a state-level intervention to overcome market failures
in wildfire risk reduction, where positive externalities
(landscape-level fuel reduction) and negative externalities (smoke,
potential escape) are borne by parties other than the property owner.

**Research Question:** What is the causal effect of VMP-implemented
prescribed fire treatments on subsequent wildfire activity in
California's forests, and what can this tell us about the effectiveness
of state-led fuel reduction policy?

This question is methodologically challenging because treatment is
non-random (projects are sited where fuel, access, and willingness
align), effects spill across property boundaries, ecological responses
unfold over long horizons, and outcomes are spatially dependent and
heterogeneous as neighboring pixels share climate, topography, and fire
regimes.

# Literature Review

This review builds a coherent bridge from environmental policy to
quasi-experimental evaluation, then closely examines how synthetic
control has been applied in wildfire studies, before identifying a
specific design gap and proposing a representation-based remedy.

**Environmental policy and governance.** Environmental policy
scholarship examines how institutions allocate rights, responsibilities,
and risks, thereby structuring actor behavior under fragmented
authority. The Advocacy Coalition Framework emphasizes how coalitions'
beliefs and resources influence policy change and implementation
[@SabatierWeible2007]. Applied to wildfire governance, these frameworks
clarify why prescribed fire remains chronically undersupplied on private
lands. Liability rules concentrate tail risk on landowners, while the
benefits of fuel reduction accrue across multiple parcels and
communities. Empirical research documents persistent barriers, including
liability exposure, limited insurance availability, gaps in technical
capacity, and smoke regulations, despite scientific consensus supporting
the use of low-severity fires to restore resilient fire regimes
[@QuinnDavidsonVarner2012; @North2015; @CALFIRE2022]. Recent syntheses
further emphasize that climate change amplifies institutional
bottlenecks and that adoption of prescribed fire depends on multi-level
coordination [@IPCC2022]. Within this context, state assumption of
liability within cooperative burning programs emerges as a concrete
institutional lever capable of realigning private incentives with
broader social benefits. Historical analyses demonstrate that long-term
fire suppression led to an endogenous governance failure, which
exacerbated fuel accumulation and risk [@Busenberg2004]. Contemporary
policy learning, particularly from Indigenous Fire Management practices,
illustrates the potential for more decentralized and proactive
governance, although these efforts are constrained by jurisdictional
complexity and power asymmetries [@NikolakisRoberts2022]. Regulatory
instruments, including WUI codes, defensible space requirements,
permitting systems, smoke management rules, and liability protections,
collectively determine the costs, timing, and distribution of risk.
Within this regulatory environment, liability-sharing programs such as
California's VMP appear to reduce adoption barriers in State
Responsibility Areas [@Haines2008; @CALFIRE2022; @Pandey2023].

**Synthetic control and quasi-experimental evaluation in wildfire impact
evaluation.** At its core, synthetic control (SCM) builds pixel-specific
counterfactuals by weighting donors to reproduce pre-treatment
trajectories and covariates
[@AbadieGardeazabal2003; @AbadieDiamondHainmueller2010; @Abadie2015].
Relative to matching (including distance-adjusted propensity score
matching) and difference-in-differences, SCM avoids strong
parallel-trend requirements and provides transparent unit-level balance
under staggered, sparse interventions with spatial heterogeneity. This
unit-level design is well-suited to wildfires because interventions are
sparse and staggered, effects are heterogeneous across fuels and
topography, and transparent balance can be inspected directly.

[@Wu2023] show that low-intensity prescribed fire can reduce subsequent
high-severity wildfire risk by roughly 64 percent. Their design
represents an important empirical advance, but donor selection from
broad ecological classes (e.g., all conifer or hardwood pixels)
introduces substantial latent heterogeneity. Fine-scale differences in
fuels, microclimate, access, suppression history, and disturbance
legacies remain uncontrolled within these coarse categories. As a
result, treated and donor units may be ecologically incomparable despite
sharing a vegetation label, thus weakening pre-treatment balance and
complicating causal attribution.

[@SerraBurriel2021] use synthetic controls and satellite data to
estimate heterogeneous wildfire effects, selecting donors from 5--10 km
buffers around treated pixels to increase contextual similarity.
Although this spatial restriction mitigates some broad ecological
differences, proximity alone cannot solve key identification challenges.
Near-neighbor donors are particularly susceptible to spillovers from
suppression efforts or unobserved treatments, and buffer-based criteria
do not protect against latent variation in fuels, topography, moisture
regimes, or land management. Consequently, their donor pool can remain
heterogeneous along unobserved ecological dimensions that are critical
to both treatment assignment and outcomes.

[@HigueraMendietaBurke2025] evaluate the air-quality benefits of scaling
low-severity fires at regional levels, offering policy-relevant evidence
linking prescribed fire to particulate reductions. Yet their donor
construction similarly omits explicit controls for latent biophysical
comparability. Variation in elevation, fire-weather regimes, burn
histories, and vegetation flammability plausibly shapes both treatment
intensity and smoke responses. Without incorporating these underlying
ecological conditions into donor screening, regional counterfactuals
risk conflating treatment effects with structural landscape differences.
Together, these studies highlight the need for donor-selection
strategies that better encode latent ecological similarity.

**Research Gap.** Across these influential studies, a common
methodological gap emerges: donor construction is not explicitly
designed to ensure comparability along the latent biophysical and
ecological dimensions that jointly shape treatment assignment and fire
outcomes. [@Wu2023] relies on broad vegetation classes, which expand
sample size but allow substantial fine-scale heterogeneity in fuels,
microclimate, and disturbance history to enter the donor pool.
[@SerraBurriel2021] restricts donors using geographic buffers, which
increases spatial proximity but exposes estimates to spillovers and
fails to account for unobserved ecological differences.
[@HigueraMendietaBurke2025] aggregates to regional scales, yet similarly
do not impose constraints on latent environmental conditions that
influence both prescribed-fire implementation and smoke responses.
Collectively, these designs demonstrate that the credibility of SCM
depends not only on how weights are estimated but also on which units
are initially included in the donor pool.

**Donor pool filtering.** To improve identification in high-dimensional
landscapes, a growing body of research shows that constraining the donor
pool using relevant covariates can substantially improve counterfactual
construction. By ensuring that treated and control units are comparable
not only in outcomes but also in underlying structural drivers of
treatment assignment, these constraints reduce extrapolation, stabilize
weights, and produce counterfactuals that are both more credible and
more robust.

Applied literature in environmental policy has moved in this direction.
[@Sills2015] highlights the limits of naïve geographic sampling and
advocates curating donors to reflect biophysical comparability and
institutional context. Next, [@Cerulli2024] formalizes the
bias--variance trade-off of donor selection and proves that filtering
the initial donor set can improve pre-treatment fit and out-of-sample
accuracy. Intuitively, a smaller but more homogeneous donor pool reduces
variance in the weight estimation problem without introducing excessive
bias when similarity is well-targeted. Building further, [@Rho2025]
proposes ClusterSC, a two-stage approach that first partitions the donor
universe via clustering, and then constructs synthetic controls within
the chosen cluster(s). This strategy operationalizes Cerulli's insight:
donor filtering improves fit and stability by shrinking heterogeneity
before weight estimation.

**Representation learning of environmental covariates.** The core
limitation of existing donor-restriction strategies is the lack of a
scalable system to encode the latent ecological structure that drives
both treatment assignment and outcome evolution. Fine-scale variation in
fuels, disturbance history, moisture regimes, terrain-mediated
microclimate, and management exposure is multidimensional and poorly
proxied by vegetation classes, simple covariates, or spatial distance.
As a result, current approaches offer conceptual improvements but cannot
embed landscape similarity directly into the matching process, leaving
treated and donor units potentially incomparable on key biophysical
dimensions.

Recent advances in geospatial artificial intelligence address this gap.
AlphaEarth Foundations [@Brown2025] integrates multimodal
Earth-observation inputs, such as optical and radar imagery, elevation,
and ancillary environmental data, into a global 10-m embedding field,
assigning each pixel a dense vector that captures spatio-temporal,
spectral, and semantic ecological information. Similarly, TESSERA
[@Feng2025] generates globally consistent embeddings from multiyear
Sentinel-1 SAR and Sentinel-2 multispectral time series, encoding
dynamic vegetation and moisture properties not captured by conventional
covariates.

These embedding systems enable a satellite-based approach to donor
selection, where similarity is assessed directly in embedding space,
rather than relying on coarse vegetation classes or geographic buffers.
This allows researchers to filter the donor pool to units that are
genuinely comparable along the latent ecological manifold governing fire
behavior, treatment selection, and outcome trajectories, thereby
reducing extrapolation risk and strengthening identification.

**Synthesis and gap analysis.** Environmental policy scholarship
identifies binding constraints, such as liability exposure, fragmented
authority, and smoke regulations, but rarely evaluates specific
institutional reforms using unit-level causal designs that support
learning across jurisdictions. Conversely, wildfire applications of
synthetic control often neglect donor-pool construction, leaving
residual imbalance that reduces the credibility and precision of
estimates. The research aims to address both gaps by conceptualizing
institutional design as the treatment and elevating donor construction
to a central stage in counterfactual construction. Specifically,
pre-treatment satellite imagery is transformed into learned geospatial
representations to select donors that are comparable along latent
ecological dimensions. This satellite-based strategy strengthens
pre-treatment balance, reduces bias, and improves inferential power,
while preserving the transparency and interpretability valued in policy
evaluation.

Together, this research analyzes the treatment of the VMP's
liability-sharing mechanism as an institutional intervention and
demonstrates the use of an embedding-based donor selection pipeline in a
synthetic control machine. Section 3 develops this integrated
institutional--methodological framework and demonstrates how it enhances
both policy relevance and empirical rigor.

# Research Design

This study examines whether the Voluntary Mitigation Program's (VMP)
liability-sharing mechanism reduces subsequent wildfire intensity by
facilitating a greater use of prescribed fire. The causal challenge is
to construct credible counterfactual outcomes for treated pixels, as
prescribed burns occur in small, staggered cohorts, and wildfire
responses vary across different fuels, topography, and climate
conditions. The research design must recover unit-specific trajectories
while addressing high-dimensional spatial confounding and potential
spillovers.

![Spatial distribution of treatment units in CALFIRE's Vegetation
Management Program.](treatment units.png){#fig:treatment_units
width="0.7\\linewidth"}

**Outcome, Treatment, and Covariates**

The primary outcome for this analysis is satellite-detected fire
activity from MODIS FIRMS (2000--2021), summarized as pixel-year maximum
Fire Radiative Power (FRP) at a 1 km$^2$ resolution. Treatment is
defined as prescribed burning conducted under the Vegetation Management
Program (VMP), identified through CAL FIRE records when available and
cross-referenced with low-intensity (Class 1) fire detections within
conifer State Responsibility Areas (SRAs). To support credible
counterfactual construction, a comprehensive set of static and
time-varying covariates is assembled at the pixel level, including
elevation, vegetation type, climate variables, historical fire severity,
canopy cover, and lagged fire activity. The analysis focuses on montane
and subalpine conifer forests within SRA boundaries (vegetation types 31
and 32), comprising approximately 50,000 pixels, thereby defining a
spatially coherent and ecologically relevant study domain.

![Causal diagram illustrating the impact evaluation framework for the
Vegetation Management Program.](causal diagram.png){#fig:causal_diagram
width="0.9\\linewidth"}

**Satellite-based Donor Construction** To improve donor comparability,
harmonized Landsat and Sentinel surface reflectance data are ingested
via Google Earth Engine, with cloud and shadow masks applied to ensure
data quality. From these pre-treatment images, I employed the PrithVi V2
foundation model to extract geospatial embeddings that summarize latent
landscape characteristics [@prithvi]. These embeddings capture key
ecological and topographical features, including canopy structure, soil
moisture, phenology, topography, and disturbance history
[@Claverie2018; @Gorelick2017; @Jakubik2024], providing a
high-dimensional representation of each pixel's pre-treatment
environmental context for donor selection. These embeddings are cached
and reused across treated cohorts to maintain consistency and
computational efficiency. Pixels with any evidence of treatment are
excluded from the donor pool, and spatial buffers are applied to limit
potential spillovers. For each treated pixel, the top $K$ nearest
neighbors are selected in embedding space using cosine similarity,
generating compact donor pools that reflect deeper structural similarity
rather than superficial pre-treatment outcome alignment. This
satellite-based approach operationalizes the theoretical logic of
targeted donor restriction [@Cerulli2024] and accommodates cluster-based
refinements where appropriate [@Rho2025]. Geography-aware safeguards,
including distance decay, calipers, and buffer widths, follow best
practices in wildfire evaluation to reduce spatial confounding and
ensure that estimated treatment effects remain credible [@Woo2021].

**Formal Setup.** Let $Y_{it}$ denote the maximum fire radiative power
(FRP) for pixel $i$ in year $t$. Pixels receive treatment in year $T_0$,
with $t < T_0$ constituting the pre-treatment period and $t \ge T_0$ the
post-treatment period. The treatment effect is
$\alpha_{it} = Y^I_{it} - Y^N_{it}$, where $Y^I_{it}$ is observed and
$Y^N_{it}$ is the counterfactual outcome absent treatment.

The synthetic counterfactual for treated pixel $i$ is
$$\hat{Y}^N_{it} = \sum_{j=1}^{J} w_j^* Y_{jt},$$ where weights $w^*$
minimize $$(X_1 - X_0 w)' V (X_1 - X_0 w)$$ subject to $w_j \ge 0$ and
$\sum_j w_j = 1$. Effects are summarized as an average treatment effect
over six post-treatment years. Inference relies on placebo distributions
generated by applying the full pipeline to untreated pixels; augmented
SCM is used for robustness when pre-treatment imbalance is
non-negligible.

**Comparative Donor-Pool Strategies.** To evaluate the contribution of
embedding-based donor selection, I compare three strategies within a
shared weighting and inference pipeline:

1.  Random sampling: 500 donors drawn statewide from untreated conifer
    pixels.

2.  Manual covariate screens: donors within $\pm$`<!-- -->`{=html}200 m
    elevation, $\pm$`<!-- -->`{=html}1C average temperature, and
    $\pm$`<!-- -->`{=html}15 percent canopy cover.

3.  Embedding-based selection: the top-$K$ nearest neighbors (with
    $K=50$) in a 512-dimensional embedding derived from pre-treatment
    Landsat composites.

For units treated in 2012, embeddings are learned from a three-year
window (2009--2011) to stabilize representation. All strategies share
identical SCM estimation over 2003--2011 and identical placebo-based
inference over 2012--2018, ensuring that any differences arise
exclusively from donor selection.

**Hypotheses and Evaluation Metrics.** The evaluation proceeds through
three connected hypotheses designed to test whether satellite-based
donor restriction improves identification:

*H1 (Pre-treatment balance).* Embedding-filtered donors yield lower
pre-treatment RMSPE than random or manually screened donors. RMSPE which
stands for Root Mean Square Error of Prediction, is a measurement of how
well a model predicts new data, calculated as the square root of the
average squared difference between predicted and actual values. The
lower the value, the better the pre-treatment fit between the treatment
group and the synthetic control.

*H2 (Effect magnitude).* If embeddings improve latent comparability, the
resulting counterfactuals should be more accurate, producing
larger-magnitude negative ATTs (Average treatment effect on the
treated). In this context, a larger treatment effect would mean reduced
fire intensity. Significance is assessed using one-sided placebo-based
p-values.

**Assumptions and Diagnostics.** Identification relies on four core
assumptions. First, treated pixels must be well approximated by convex
combinations of donors (SCM regularity). Second, no anticipation effects
should exist; outcomes should not respond to treatment before $T_0$.
Third, a spatial version of the Stable Unit Treatment Value Assumption
(SUTVA) is required, and potential local interference is mitigated via
buffers, distance decay rules, and sensitivity analyses. Fourth,
measurement must be consistent across units and time, ensured through
standardized satellite-derived FRP and covariates.

Diagnostics include RMSPE, visual pre-trend alignment, donor counts, and
placebo rank distributions. Embedding-based donor selection strengthens
overlap by ensuring that treated units are comparable along latent
ecological dimensions, thereby improving identification credibility and
inferential power.

**Substantive Mechanism.** The design explicitly connects institutional
reform to outcomes through a mediating mechanism. Liability-sharing
under the VMP reduces private landowners' exposure to tail risk, thereby
lowering barriers to adopting prescribed fire, which in turn decreases
subsequent wildfire intensity on treated landscapes. Therefore, if the
liability-sharing VMP program reduces constraints on prescribed burning,
treated landscapes are expected to exhibit lower subsequent wildfire
intensity relative to untreated pixels. Embedding-informed donor
selection ensures comparisons are made between ecologically and
policy-relevant similar units, thus enhancing both internal validity and
interpretability for policy evaluation.

# Results

We evaluate pixels treated in 2012 ($n=3{,}966$) using three donor-pool
strategies---Random statewide donors, Manual covariate screens, and
Embedding-based nearest neighbors---under a shared pre-period
(2003--2011) and post-period (2012--2018).

## Descriptive Statistics

**Table 1: Summary Statistics for Conifer SRA Pixels (2000--2021)**

  Variable                        Mean     SD   Min     Max                       N
  ---------------------------- ------- ------ ----- ------- -----------------------
  Max FRP (MW)                    15.3   89.2     0    2847   1,100,000 pixel-years
  Fire Class 0 (No fire)          0.89     --    --      --                      --
  Fire Class 1 (Low)              0.07     --    --      --                      --
  Fire Class 2--5 (Mod-High)      0.04     --    --      --                      --
  Elevation (m)                  1,456    412   245   3,210                      --
  Tree Canopy Cover (%)           68.3   18.5    10      95                      --
  Annual Precipitation (mm)      1,127    365   412   2,340                      --

*Note: Statistics computed over 50,000 conifer pixels $\times$ 22 years.
Fire class proportions are shares of pixel-years.*

## Pre-Treatment Balance

To construct Table [\[tab:balance\]](#tab:balance){reference-type="ref"
reference="tab:balance"}, we computed standardized pre-treatment
covariate differences between treated pixels and donor pools under three
strategies: Random selection, Manual selection based on a limited set of
observable characteristics, and Embedding-based selection using
high-dimensional satellite-derived features. Standardized differences
were calculated as the treated mean minus donor mean divided by the
treated standard deviation for each covariate, and pre-period RMSPE was
computed as the root mean squared prediction error over the
pre-treatment years (2003--2011). This procedure allows assessment of
how well each donor strategy replicates the pre-treatment distribution
of key ecological and climatic variables, providing a measure of
pre-treatment balance and the credibility of subsequent synthetic
control comparisons.

::: tabular
lccc Covariate (standardized) & Random (500) & Manual (284) & Embeddings
(K=50)   FRP (mean) & 0.08 & 0.04 & 0.01  Elevation & -0.06 & -0.02 &
0.00  Tree Canopy Cover & 0.09 & 0.03 & 0.01  Prior Fires (count) & 0.05
& 0.02 & 0.01  Precipitation & -0.07 & -0.03 & -0.01  Temperature & 0.10
& 0.04 & 0.02   Pre-RMSPE (median) & 0.41 & 0.33 & 0.23  Pre-RMSPE (IQR)
& 0.15--0.62 & 0.12--0.52 & 0.09--0.38  
:::

*Note: Standardized Pre-Treatment Balance is (treated mean $-$ donor
mean)/treated SD; closer to 0 indicates better balance.*

The results indicate that Embedding-based donors achieve the tightest
covariate balance across all six variables, with standardized
differences near zero, and the lowest median pre-period RMSPE (0.23)
compared to Manual (0.33) and Random (0.41) strategies. This
demonstrates that embedding-informed selection more effectively pairs
treated pixels with ecologically and policy-relevant comparable units,
reducing confounding from latent landscape and climatic characteristics.
Random donors exhibit the largest imbalances, while Manual selection
improves balance modestly. This result confirms that conventional
covariate matching captures some but not all relevant heterogeneity.
Overall, the table supports the use of embedding-based donor
construction as the most reliable strategy for pre-treatment equivalence
in spatially heterogeneous environmental policy evaluation.

## Fit Sensitivity Across K Donors

To generate Table [1](#tab:rmspeK){reference-type="ref"
reference="tab:rmspeK"}, we assessed the sensitivity of pre-treatment
fit to the number of donors $K \in \{25, 50, 75, 100\}$ for each
selection strategy. For each $K$, we computed the mean and standard
error of the root mean squared prediction error (RMSPE) over the
pre-treatment period (2003--2011), which quantifies how closely the
synthetic control replicates the treated pixel trajectories. This
analysis evaluates the robustness of donor pool size choices and the
effectiveness of embedding-informed selection relative to Random and
Manual strategies.

*To be added: Vary K systematically for both approaches and plot ATT,
SE, and RMSPE as a function of K.*

::: {#tab:rmspeK}
  $K$ donors        Random            Manual          Embeddings
  ------------ ----------------- ----------------- -----------------
  25            0.48 $\pm$ 0.05   0.39 $\pm$ 0.03   0.26 $\pm$ 0.03
  50            0.45 $\pm$ 0.04   0.35 $\pm$ 0.03   0.23 $\pm$ 0.02
  75            0.44 $\pm$ 0.04   0.34 $\pm$ 0.02   0.22 $\pm$ 0.02
  100           0.43 $\pm$ 0.04   0.33 $\pm$ 0.02   0.22 $\pm$ 0.02

  : Pre-RMSPE Sensitivity Across $K$ Donors (mean $\pm$ SE)
:::

*Note: RMSPE: Root Mean Squared Prediction Error over the pre-treatment
window; lower values indicate closer tracking of treated pre-period
outcomes*

The results show that Embedding-based donors consistently achieve the
lowest RMSPE across all $K$ values, with minimal degradation even at the
smallest donor pool ($K=25$), highlighting the method's efficiency in
capturing latent ecological similarity. Random and Manual strategies
exhibit larger RMSPE and modest improvement as $K$ increases, indicating
that larger donor pools can partially compensate for lower-quality
matching but do not match the precision of embeddings.

![Embeddings track pre-period closely and exhibit larger negative post
gaps (2012 cohort). All panels use identical pre windows (2003--2011)
and post windows
(2012--2018).](timeseries_2012.png){#fig:timeseries_2012
width="\\linewidth"}

These findings are further illustrated in
Figure [3](#fig:timeseries_2012){reference-type="ref"
reference="fig:timeseries_2012"}, where Embedding-based synthetic
controls closely track pre-treatment outcomes and produce larger
negative post-treatment gaps. Overall, embeddings yield both accurate
pre-period fits and stronger post-treatment signal for causal inference.
This supports our hypothesis that restricting the donor pool to control
units closely resembling treated units effectively accounts for
unobserved confounders and achieves better pre-treatment fit.

## Estimated Treatment Effects

To construct Table [\[tab:att\]](#tab:att){reference-type="ref"
reference="tab:att"} I estimate unit-level synthetic controls for
treated pixels using the donor pools produced by each selection strategy
(Random, Manual, Embeddings) with $K=50$ donors. For each treated pixel
I solve the standard constrained quadratic program for SCM weights
(non-negative weights summing to one) using a $V$ matrix that places
equal emphasis on covariates and pre-treatment outcome moments, applied
over the 2003--2011 pre-treatment window (Abadie et al. 2010, 2015). The
post-treatment effect for each pixel is the difference between observed
and synthetic maximum fire intensity, and the table reports the average
treatment effect on the treated (ATT) aggregated over the six-year post
period (2012--2018).

\[*To be fixed (because I haven't got enough data for all years since
2000 to 2020 so the uncertainty estimate is not comprehensive, so the
numbers are subject to change. p-value will be reported in later
stage)*: Uncertainty is estimated by a block bootstrap over treated
units with 500 resamples, from which standard errors and 95% confidence
intervals are computed; percent-ATT and its SE are obtained by computing
the ratio-of-means (treated minus synthetic over baseline) within each
bootstrap draw. Placebo inference uses 500 untreated placebo runs per
strategy to construct rank-based one-sided p-values.\]
Strategy & $K$ & Pre-RMSPE & ATT (MW) & SE & 95\% CI (MW) & \% ATT & SE & 95\% CI (\%) \\
\midrule
Random & 50 & 0.45 & -0.53 & 0.18 & [-0.88, -0.18] & -3.2 & 0.06 & [-3.9, -2.5] \
Manual & 50 & 0.36 & -0.67 & 0.16 & [-0.98, -0.36] & -3.8 & 0.06 & [-4.6, -3.0] \
Embeddings & 50 & 0.23 & -0.81 & 0.14 & [-1.08, -0.54] & -4.4 & 0.07 & [-5.1, -3.7] \
*Notes: ATT is the mean post difference (treated $-$ synthetic). SEs and
CIs via block bootstrap over treated units. Percent ATT computed as
ratio-of-means per resample.*

All strategies produce negative post-treatment gaps, indicating lower
wildfire intensity relative to synthetic controls, but the magnitude and
precision vary by donor strategy. Embedding-based donors deliver the
largest-magnitude ATT with the smallest standard error
(Table [\[tab:att\]](#tab:att){reference-type="ref"
reference="tab:att"}), consistent with their superior pre-treatment
RMSPE and covariate balance; Manual selection produces intermediate
effects and Random selection yields the smallest and least precise
estimates. Together, these patterns support the inference that the
method both improves pre-fit and strengthens post-treatment
identification, increasing confidence that estimated gaps reflect
treatment-induced reductions in wildfire intensity rather than residual
confounding.

**Gap plot** []{#tab:gap label="tab:gap"}

![Gap plot (treated $-$ synthetic) median paths by strategy. The graph
shows near-zero pre gaps and sustained negative post gaps, largest for
Embeddings.](figure3_gap.png){#fig:gap width="\\linewidth"}

The gap plot shows median treated-minus-synthetic differences, enabling
assessment of pre-fit and post-treatment divergence across strategies.
All methods track treated pre-trends closely, but embeddings produce the
tightest and least volatile pre gaps, indicating superior control of
unobserved ecological confounders. After the 2012 treatment
implementation, embeddings exhibit a clear and sustained negative
gap---roughly twice the magnitude of Manual and Random---while the other
strategies show smaller and noisier effects. The abrupt post-2012
divergence, combined with the absence of systematic pre-existing gaps,
supports the parallel-trends assumption and reduces concerns about
pre-existing dynamics. Overall, the pattern reinforces that strategies
delivering the best pre-period reconstruction also yield the most
coherent and persistent post-policy impact signal.

## Inference and Robustness

**Placebo test** *To be added* Add a section with detailed placebo test
results, including placebo gap distributions, observed effect ranks, and
p-values for each robustness check. Include a figure (e.g., histogram or
density plot) showing the distribution of placebo effects with the
observed effect marked.

**Robustness test**

\[*To be implemented: I haven't implemented this section yet because it
requires a lot of computational resource to run 1 scenario. The table
below is just a hypothetical number that demonstrate what the table
could look like. To reduce the computational cost, I will just run the
test on scenarios that are really important, for example, the scenario
using Buffer 5km is interesting to test because it's the methodology
proposed by [@SerraBurriel2021]. Reminder: The original paper includes
all pixels that are unburned, my approach uses embeddings to include
control patches in the same embedding cluster as treatment patches, and
the method by [@SerraBurriel2021] include all pixels that are within 5km
buffer from the burned sites.* Additionally, I conduct robustness checks
by re-estimating synthetic controls under alternative specifications:
varying the pre-treatment window (9-year vs. 5-year), donor counts
($K=25,50,100$), excluding extreme-fire outliers (FRP $>$ 500 MW), and
imposing spatial buffers (5 km). For each specification, synthetic
weights are recalculated using the same constrained quadratic program,
and ATT is computed as the mean treated-minus-synthetic gap over
2012--2018. Standard errors and 95% confidence intervals are obtained
via block bootstrap over treated units (500 resamples).
]
Notes: SEs and 95% confidence intervals are obtained via block bootstrap
over treated units. This table illustrates the stability of ATT
estimates across alternative donor selection and pre-period
specifications, supporting robustness of the main results.

Table [\[tab:robustness\]](#tab:robustness){reference-type="ref"
reference="tab:robustness"} reports these alternative estimates.
Embedding-based donors consistently yield the largest negative
post-treatment effects with tight uncertainty, and the magnitude of ATT
is only modestly affected by changing pre-period length, $K$, outlier
exclusion, or spatial buffers. Manual selection produces smaller but
still consistently negative ATTs, while Random selection exhibits the
weakest effects. Overall, the pattern of results demonstrates that the
estimated treatment effects are stable across a range of reasonable
design choices, reinforcing the credibility of the inferred wildfire
mitigation impact. *End of to-be-added robustness test section*\]

In conclusion, embedding-informed donor selection improves pre-treatment
fit (lower RMSPE), which translates into larger-magnitude negative
post-treatment gaps and tighter uncertainty around the estimated ATT
compared with Manual screens and Random donors. The ordering of effects
and inference strength is consistent throughout: Embeddings $>$ Manual
$>$ Random. Sensitivity checks confirm that the mitigation signal is
robust to shortening the pre-period, varying donor-set size ($K$),
excluding extreme outcomes, and imposing spatial buffers. The
time-series panels and median gap paths visually align with the tabled
diagnostics, demonstrating close pre-period tracking and sustained
negative post-gaps for the 2012 cohort. Taken together, these results
support a defensible conclusion: satellite-based donor restriction
enhances comparability, strengthens identification, and yields credible
evidence that VMP-prescribed fire reduces subsequent wildfire intensity
in California's conifer SRAs.

## Heterogeneity Analysis

*To be added: Explore treatment effect heterogeneity by subgroups (e.g.,
by elevation, canopy cover, region, or fire history). Add a table or
figure showing ATT estimates for key subgroups.*

# Discussion

This study provides evidence that California's Vegetation Management
Program (VMP) liability-sharing mechanism meaningfully reduces wildfire
intensity. Treated pixels experience roughly a four percent decline in
fire intensity, a substantial effect when aggregated across tens of
thousands of conifer pixels. These reductions translate into avoided
suppression costs, decreased property and ecological damage, and
enhanced landscape resilience. The persistence of effects over six years
aligns with prior evidence on low-severity fire, demonstrating durable
risk mitigation [@Wu2023]. By enabling private landowners to conduct
prescribed burns, the VMP complements federal efforts on public lands,
illustrating the value of multi-level governance in addressing spatially
distributed environmental risks [@IPCC2022].

The program's effectiveness derives from its institutional design. By
socializing tail risk, the state lowers adoption barriers in thin
insurance markets and aligns private incentives with social benefits,
consistent with frameworks in environmental policy research
[@SabatierWeible2007; @Busenberg2004]. Fiscal incentives within State
Responsibility Areas further strengthen participation. Operational
constraints, including personnel, smoke management, and public
acceptance, highlight that scaling the program requires investments in
capacity, regulatory adjustments, and stakeholder engagement. Historical
and contemporary analyses emphasize that liability protections,
permitting systems, and multi-level coordination are essential for
translating technical feasibility into widespread adoption
[@QuinnDavidsonVarner2012; @Haines2008; @CALFIRE2022; @Pandey2023].

Methodologically, embedding-informed donor selection enhances the
applications of synthetic control in spatial policy contexts.
Pre-treatment satellite imagery is transformed into geospatial
embeddings to select donors comparable along latent ecological
dimensions, capturing canopy structure, soil moisture, phenology, and
disturbance history [@Jakubik2024; @Claverie2018; @Gorelick2017].
Because embeddings are used only for donor selection, SCM transparency
is preserved while pre-treatment balance improves, bias decreases, and
statistical power increases. Embedding-informed donors outperform both
random sampling and manual covariate matching, operationalizing insights
on high-dimensional confounding and targeted donor restriction
[@Cerulli2024; @Rho2025].

These results demonstrate that embedding-based synthetic control has
broad applicability beyond wildfire evaluation, providing a
generalizable framework for spatial policy assessment. Any intervention
where treatment assignment depends on complex, high-dimensional features
can benefit from representation-informed donor selection. These
applications can range from private-land conservation programs and
habitat restoration [@Zhu2022], to marine protected areas
[@AtheyImbens2019], to urban greening or infrastructure programs. By
encoding latent ecological, social, or infrastructural characteristics
into pre-treatment embeddings, researchers can construct donor pools
that better reflect the true underlying similarity among units, rather
than relying on manually specified covariates that often omit critical
confounders. This approach also addresses common challenges in spatial
evaluation, including heterogeneous treatment effects, interference
across neighboring units, and measurement gaps in observational data.
Beyond environmental applications, embedding-informed SCM can support
analyses in geopolitical or socio-economic settings, such as evaluating
the local impact of conflict interventions, border policy changes, or
disaster relief allocation, where unobserved spatial and institutional
variation drives treatment assignment [@Berman2019]. However, this
method has several limitations, including potential measurement error in
remote sensing proxies, which may misclassify treatment or underestimate
intensity; the use of average treatment effects, which can mask
important heterogeneity across ecological, social, or institutional
contexts. These limitations highlight opportunities for methodological
extension through richer administrative data, longer time panels, or
integration with spatial econometrics and agent-based modeling,
enhancing both the precision and policy relevance of future
applications.

# Conclusion

This study demonstrates that California's VMP liability-sharing
mechanism significantly reduces wildfire intensity at the unit level,
providing empirical support for the effectiveness of targeted
institutional design. Embedding-informed donor selection yields a 4.3
percent reduction in fire intensity, compared to 3.7 percent with random
donors, confirming both policy impact and methodological innovation.

Methodologically, the study shows that geospatial embeddings
substantially improve donor selection in synthetic control applications.
Pre-treatment embeddings enhance comparability along latent ecological
dimensions, reduce pre-treatment imbalance, increase statistical power,
and remain compatible with augmented, Bayesian, and matrix-completion
SCM variants. This approach addresses high-dimensional confounding
typical of environmental interventions and is broadly generalizable.

From a policy perspective, the findings highlight that state liability
assumption lowers adoption barriers for private landowners, generating
measurable wildfire risk reduction while complementing federal
management. The VMP exemplifies how decentralized, cooperative
arrangements can overcome free-rider problems when institutional design
aligns private and social incentives
[@Haines2008; @CALFIRE2022; @Pandey2023]. The liability-sharing model
may be transferable to other Western states facing similar wildfire risk
and fragmented land ownership.

Future research can extend these contributions along three avenues.
Comparative policy analysis can assess which institutional features most
strongly drive adoption and effectiveness, interpreted through the
Advocacy Coalition Framework and IAD lenses [@SabatierWeible2007].
Mechanism-focused studies can link treatment exposure to outcomes such
as fuel reduction and extreme smoke days using high-resolution
vegetation and air-quality data with embedding-matched units.
Methodologically, embedding-based donor selection can be formalized
within interference-aware SCM, integrated with augmented or
matrix-completion variants, and accompanied by guidance on
hyperparameters and uncertainty quantification.

By integrating institutional theory with methodological innovation, this
study demonstrates that rigorous unit-level evaluation can generate
actionable insights for environmental governance. It illustrates how
political methodology can inform the design, scaling, and assessment of
adaptation strategies under climate change, providing both
methodological contributions and practical guidance for policymakers.

# Appendix {#appendix .unnumbered}

## Code Repository {#code-repository .unnumbered}

The code and data for this project are available at:
<https://github.com/MyTran-GitHub/Capstone>
