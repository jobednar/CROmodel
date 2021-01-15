$Title  CRO
$GDXin PARS_IN

Option decimals=6;

Set t "decadal time steps";
$load t

Set dt "annual time steps" /dt1*dt81/;
Set par "parameters for MAC curve"
Set pmac "segments of mac function (for integration)"  /pmac1*pmac50/;
Set tpwl "nodes of piecewise linear functions"  /tpwl1*tpwl17/;
Set ppwl "segments of piecewise linear functions"  /ppwl1*ppwl16/;
$load par

$ontext
dt is the annual time scale used for computing CRO interest costs.
t is the decadal time scale for decision variables which are then mapped on dt.
tpwl is the resolution of piece wise linear functions of variables on dt (tpwl is index of nodes, ppwl is index of segments between nodes).
The set size for t and tpwl should be computed as follows:  set_size=(set_size_dt-1)/step_size + 1.
Example for t: set_size_dt = 81, step_size=10, therefore set_size=9, i.e. t1*t9
Example for tpwl: set_size_dt = 81, step_size=5, therefore set_size=17, i.e. tpwl1*tpwl17
Note that
         (i) step_size(t)/step_size(tpwl) = integer
         (ii) (set_size_dt-1)/step_size = integer
Segments (e.g. ppwl) are set_size - 1.
pmac is the index of segments of the marginal abatment cost (MAC) curve for integration.
$offtext

Scalars
    TPWL_STEP_SIZE         Distance between ord(tpwl) and ord(tpwl)+1 on dt scale                /5/
    PPWL_NR                Number of ppwl periods                                                /16/
    T_STEP_SIZE            Number of years (dt) per decade (t)                                   /10/
    PMAC_NR                Amount of segments of the MAC curve for integration                   /50/
    DT_YEAR_2100           Year 2100 on dt time scale (dt1 = 2020)                               /81/;



Alias(dt,dta);


Parameters
         EM_BASE(t)        Baseline emissions
         EM_BASE_DT(dt)    Baseline emissions mapped on dt time scale
         AB_REL_MAX(t)     Maximum abatement (=PAR_A minus epsilon)
         AB_REL_MIN(t)     Minimum abatement (typically set to some value for t1 and then zero)
         PARS(t,par)       Matrix containing parameters of MAC-curve
         PAR_A(t)          Parameter A of MAC-curve
         PAR_K(t)          Parameter k of MAC-curve
         PAR_L(t)          Parameter L of MAC-curve
         PAR_PI(t)         Parameter P of MAC-curve
         PAR_NU(t)         Parameter nu of MAC-curve
         PAR_B(t)          Parameter b of MAC-curve: PAR_B(t) = exp(PAR_PI(t))
         PAR_C(t)          Parameter c of MAC-curve: PAR_C(t) = 1 over PAR_K(t)
         PARS_BS(par)      Matrix containing parameters of BS-MAC-curve
         AB_REL_LEVEL(t)   Initial abatement value for solver
         EM_NET_LEVEL(t)   Initial net emissions value for solver
         MAC_LEVEL(t)      Initial MAC value for solver;

$load PARS, EM_BASE, AB_REL_MAX, AB_REL_MIN, AB_REL_LEVEL, EM_NET_LEVEL, MAC_LEVEL, PARS_BS


Scalars
         BUDGET                                Cumulative carbon budget for 2100
         R                                     Annual discount rate (market interest rate)
         R_CL                                  Carbon debt interest rate
         PHI_LEVEL                             Initial phi value for solver
         PHI_UP                                Can be defined to help finding the optimal solution by default PHI_UP is undefined
         PHI_LO                                Can be defined to help finding the optimal solution by default PHI_LO = 0
         PAR_A_BS                              Parameter A of backstop MAC-curve
         PAR_L_BS                              Parameter L of backstop MAC-curve
         PAR_B_BS                              Parameter b of backstop MAC-curve
         PAR_C_BS                              Parameter c of backstop MAC-curve
         PAR_NU_BS                             Parameter nu of backstop MAC-curve
         USE_BS                                Binary parameter defining if backstop can be used or not (1 = backstop can be used)

* some parameters relevant for tweaking numerical stability of the optimization procedure
         EPS_ARCTAN                            Defines the slope of arctan-step function (smaller=steeper)     /0.0000001/
         EPS_REPAYTERM                         Needed to avoid devision by zero. Increase this number if solution is unbounded            /0.000001/
         PLH_NR                                Number of years in the planning horizon (this is an upper limit but should be close as possiple to the real number) /100/
         SCALE_NET_NEG_COST                    scale net neg costs so cumulative costs don't become too small or too large (this number is emprically determined)
         BS_STEP_SIZE_LO
;


$load  R, BUDGET, PHI_LEVEL, R_CL, USE_BS, BS_STEP_SIZE_LO, PHI_UP, PHI_LO
$load SCALE_NET_NEG_COST

Variables
* general variables
         z                                               value of the objective function
         em_cum_2100                                     cumulative emissions in year 2100
         em_net(t)                                       net emissions
         em_net_dt(dt)                                   net emissions mapped on dt time scale

*repayment term variables
         intercept_cum_em_net_neg_ppwl(ppwl)             intercepts of segments of PWL function of cumulative net negative emissions
         repay_term_all_dt_ppwl(dt,ppwl)                 repay term matrix including values where PWL segments are out of their domain
         repay_term_trunc_dt_ppwl(dt,ppwl)               repay term matrix where out-of-domain values are set to ~zero
         repay_term_dt(dt)                               repay term (final vector -> zeros are removed)
         step_lower_repay_term_dt_ppwl(dt,ppwl)          lower part of rect-function to control domain of PWL segments
         step_upper_repay_term_dt_ppwl(dt,ppwl)          upper part of rect-function to control domain of PWL segments

* CRO cost base variables
         intercept_cum_em_net_neg_cost_ppwl(ppwl)        intercepts of segments of PWL function of cumulative net negative emission costs
         slope_cum_em_net_neg_cost_ppwl(ppwl)            slopes of segments of PWL function of cumulative net negative emission costs
         cro_cost_base_all_dt_ppwl(dt,ppwl)              CRO cost base matrix including values where PWL segments are out of their domain
         cro_cum_cost_base_trunc_dt_ppwl(dt,ppwl)        CRO cost base matrix where out-of-domain values are set to ~zero
         cro_cum_cost_base_dt(dt)                        Cumulative CRO cost base (final vector -> zeros are removed)
         cro_cost_base_diff_dt(dt)                       CRO cost base (differences from cumulative CRO cost base)
         step_lower_cro_cost_base_dt_ppwl(dt,ppwl)       lower part of rect-function to control domain of PWL segments
         step_upper_cro_cost_base_dt_ppwl(dt,ppwl)       upper part of rect-function to control domain of PWL segments
         contr_cost_dt(dt)                               CRO discounted total annual interest cost
         cum_em_net_neg_cost                             total costs of net negative emissions (should be equal to cum_cro_cost_base)
         cum_cro_cost_base                               total CRO cost base (should be equal to cum_em_net_neg_cost)
         contr_cost_sum                                  total CRO interest costs

;

Positive Variables

* general variables
         phi                                             ratio of total net negative to total net positive emissions
         em_net_neg_dt(dt)                               net negative emissions: negative part of net emissions
         em_net_pos_dt(dt)                               net positive emissions: positive part of net emissions

* MAC variables
         em_ab_rel(t)                                    abatement (as fraction of baseline emissions)
         em_ab_rel_bs(t)                                 abatement from backstop (as fraction of baseline emissions)
         mac_step_size(t)                                segment size of MAC-curve (for integration)
         mac_step_lower(pmac,t)                          nodes of MAC-curve segmentation for integration (beginning of segment)
         mac_step_upper(pmac,t)                          nodes of MAC-curve segmentation for integration (end of segment)
         mac_integr(pmac,t)                              Integrated marginal costs (multiply by baseline emissions to obtain total abatement costs)
         bs_step_size(t)                                 segment size of backstop-MAC-curve (for integration)
         bs_step_lower(pmac,t)                           nodes of backstop-MAC-curve segmentation for integration (beginning of segment)
         bs_step_upper(pmac,t)                           nodes of backstop-MAC-curve segmentation for integration (end of segment)
         bs_integr(pmac,t)                               Integrated backstop marginal costs (multiply by baseline emissions to obtain total backstop costs)
         mac(t)                                          marginal abatement cost
         tot_cost(t)                                     total abatement costs (sum of conventional and backstop abatement)
         tot_cost_dt(dt)                                 total abatement costs mapped on dt time scale
         ab_cost_sum                                     sum of total abatement costs


*repayment term variables
         cum_em_net_pos_dt(dt)                          cumulative net positive emissions
         cum_em_net_neg_dt(dt)                          cumulative net negative emissions
         cum_em_net_neg_tpwl(tpwl)                      nodes of PWL approximation of cumulative net negative emissions
         slope_cum_em_net_neg_ppwl(ppwl)                slopes of PWL approximation of cumulative net negative emissions

* CRO cost base variables
         em_ab_rel_dt(dt)                               abatement (as fraction of baseline emissions) mapped on dt time scale
         em_ab_rel_bs_dt(dt)                            abatement from backstop (as fraction of baseline emissions) mapped on dt time scale
         em_net_neg_cost_dt(dt)                         net negative emission cost (average abatement cost x net negative emissions)
         cum_em_net_neg_cost_dt(dt)                     cumulative net negative emission cost
         cum_em_net_neg_cost_tpwl(tpwl)                 nodes of PWL approximation of cumulative net negative emission cost



;
* map baseline emissions on dt time scale
EM_BASE_DT(dt) = sum(t$( (ord(dt) lt 10*ord(t)+1) and (ord(dt) gt 10*(ord(t)-1)) ), EM_BASE(t) + (ord(dt)-10*(ord(t)-1)-1)*(EM_BASE(t+1)-EM_BASE(t))/T_STEP_SIZE);

* retrieve parameters of MAC curves from parameter matrices
PAR_A(t) = PARS(t,"par1");
PAR_L(t) = PARS(t,"par2");
PAR_K(t) = PARS(t,"par3");
PAR_PI(t) = PARS(t,"par4");
PAR_NU(t) = PARS(t,"par5");
PAR_B(t) = exp(PAR_PI(t));
PAR_C(t) = 1/PAR_K(t);

PAR_A_BS =  PARS_BS("par1");
PAR_L_BS = PARS_BS("par2");
PAR_B_BS =  exp(PARS_BS("par4"));
PAR_C_BS =  1/PARS_BS("par3");
PAR_NU_BS =  PARS_BS("par5");

* define bounds and initial values for some variables

*costs %>% filter(model=="GCAM4") %>% filter(SSP=="SSP5") %>% filter(r_cl==0.04) %>% filter(BS2=="LoCost_HiCap")
*phi.lo = 1.18;
*phi.up = 1.22;
*phi.l = 1.20;

*costs %>% filter(model=="GCAM4") %>% filter(SSP=="SSP1") %>% filter(r_cl==0.01) %>% filter(BS2=="LoCost_HiCap")
*phi.lo = 0.745;
*phi.up = 0.773;
*phi.l = (0.745+0.773)/2;

phi.lo = 0;
phi.l = PHI_LEVEL;

phi.up $ (PHI_UP > 0) = PHI_UP;
phi.lo $ (PHI_LO > 0) = PHI_LO;
phi.l $ (PHI_LO > 0) = (PHI_UP+PHI_LO)/2;

em_ab_rel.l(t) = AB_REL_LEVEL(t);
em_net.l(t) = EM_NET_LEVEL(t);

mac.lo(t) = PAR_B(t)* (1/PAR_NU(t) *( ((PAR_L(t)-PAR_A(t))/(AB_REL_MIN(t)-PAR_A(t)))**PAR_NU(t)-1 ) )**PAR_C(t);
mac.up(t) = PAR_B(t)* (1/PAR_NU(t) *( ((PAR_L(t)-PAR_A(t))/(AB_REL_MAX(t)-PAR_A(t)))**PAR_NU(t)-1 ) )**PAR_C(t);
mac.l(t) = MAC_LEVEL(t);

mac_step_size.up(t) = AB_REL_MAX(t)/PMAC_NR;
mac_step_size.lo(t) = AB_REL_MIN(t)/PMAC_NR;


em_ab_rel_bs.up(t) = ((PAR_L_BS-PAR_A_BS)*(PAR_NU_BS*(mac.up(t)/PAR_B_BS)**(1/PAR_C_BS)+1)**(-1/PAR_NU_BS)+PAR_A_BS);
*em_ab_rel_bs.lo("t1") = ((PAR_L_BS-PAR_A_BS)*(PAR_NU_BS*(mac.up("t1")/PAR_B_BS)**(1/PAR_C_BS)+1)**(-1/PAR_NU_BS)+PAR_A_BS);

bs_step_size.up(t) = em_ab_rel_bs.up(t)/PMAC_NR;
bs_step_size.l(t) = em_ab_rel_bs.up(t)/PMAC_NR;
bs_step_size.lo(t)$(ord(t) gt 1) = BS_STEP_SIZE_LO;
bs_step_size.lo(t)$(ord(t) eq 1) = 0;


*SCALE_NET_NEG_COST = 100;


Equations

$ontext
This section of code describes the standard mitigation model:
$offtext

 emNetMap(dt)                                    Map decadal time steps to annual time steps for net emissions (linear interpolation)

 defEmAbRelBS(t)                                 Define net emissions by subtracting abated fraction from baseline emissions (from conventional abatement and backstop BS)
 defEmAbRelNoBS(t)                               Define net emissions by subtracting abated fraction from baseline emissions (from conventional abatement but no backstop BS)
 emAbRelMin(t)                                   Lower bound for abatement (usually this is zero unless a fixed level of abatement is prescribed)
 emAbRelMax(t)                                   Upper bound for abatement (corresponds to parameter PAR_A minus epsilon from MAC curve -> this is for numerical stability)

 defMac(t)                                       Compute marginal abatement cost (MAC) from net emissions and MAC-curve parameters
 defBS(t)                                        Compute abatement level of optional backstop (BS) from marginal abatement cost and BS-MAC-curve parameters (MACs need to be equal for conventional abatement and BS)

 defMacSTepSize(t)                               Integrate MAC-curve: split abatement into PMAC_NR segments
 defMacStepLower(pmac,t)                         Integrate MAC-curve: compute lower bounds of segments
 defMacStepUpper(pmac,t)                         Integrate MAC-curve: compute upper bounds of segments
 defMacIntegrStep(pmac,t)                        Integrate MAC-curve: Use trapezoidal rule to sum up segments

 defBsStepSize(t)                                Integrate backstop-MAC-curve: split abatement into PMAC_NR segments
 defBsStepLower(pmac,t)                          Integrate backstop-MAC-curve: compute lower bounds of segments
 defBsStepUpper(pmac,t)                          Integrate backstop-MAC-curve: compute upper bounds of segments
 defBsIntegrStep(pmac,t)                         Integrate backstop-MAC-curve: Use trapezoidal rule to sum up segments

 defTotCost(t)                                   Multiply integrated marignal costs by baseline emissions to obtain total costs
 totCostMap(dt)                                  Map decadal time steps to annual time steps for total costs (linear interpolation)

 emSum1                                          Define cumulative emissions in the year 2100 (sum of annual values)
 budgetConstr1                                   Set carbon budget constraint for year 2100
 objective                                       Define objective function as discounted sum of total abatement costs + CRO interest costs
 abCostSum                                       !!!Compute total discounted abatement costs (only for reporting -> should be done in post-processing)
 contrCostSum                                    !!!Compute total discounted CRO interest costs (only for reporting -> should be done in post-processing)

$ontext
The following section contains equations to determine the CRO interest costs:
$offtext

 defEmNetDt1(dt)                                 Define net positive and net negative emissions (both variables are non-negative)
 defEmNetDt2(dt)                                 Define net positive and net negative emissions (both variables are non-negative)
 defPhi                                          Define phi as the fracion of total net negative to total net positive emissions (add 0.00000001 to denominator to avoid division by zero)

 cumEmNetNeg(dt)                                 Define time dependent cumulative net negative emissions
 cumEmNetPos(dt)                                 Define time dependent cumulative net positive emissions
 cumEmNetNegPointsPWL(tpwl)                      Express cumulative net negative emissions as piecewise linear function (PWL) with PPWL_NR segments
 defCumEmNetNegSlopesPWL(ppwl)                   Compute slopes of PWL function
 defCumEmNetNegInterceptsPWL(ppwl)               Compute intercepts of PWL function

 calcRepayTerm1(dt,ppwl)                         Compute end of repayment term. Each PWL segment procudes a different value of which only one is valid
 calcRepayTerm2(dt,ppwl)                         Compute lower part of rect-function for each of the PWL segments
 calcRepayTerm3(dt,ppwl)                         Compute lower part of rect-function for each of the PWL segments
 calcRepayTerm4(dt,ppwl)                         Compute upper part of rect-function for each of the PWL segments
 calcRepayTerm5(dt,ppwl)                         Compute upper part of rect-function for each of the PWL segments
 calcRepayTerm6(dt,ppwl)                         Multiply all ends of repay term by rect-function to set values outside domains to zero
 calcRepayTerm(dt)                               Sum up over all ends of repay term for each dt to get the correct value (non-correct ones are set to zero in previous step)


 emAbRelMap(dt)                                  Map decadal time steps to annual time steps for abatement (linear interpolation)
 emAbRelBsMap(dt)                                Map decadal time steps to annual time steps for backstop abatement (linear interpolation)

 defEmNetNegCost(dt)                             Compute net negative emission costs as fraction of total costs attributable to net negative emissions i.e. average abatement costs times net negative emissions
 cumEmNetNegCost(dt)                             Compute time dependent cumulative net negative emission costs
 defCumEmNegCostPointsPWL(tpwl)                  Define picewise linear (PWL) function of cumualtive net negative costs

 defCumEmNegCostSlopesPWL(ppwl)                  Compute slopes of PWL approximation of cumulative net negative costs
 defCumEmNegCostInterceptsPWL(ppwl)              Compute intercepts of PWL approximation of cumulative net negative costs


 calcCroCostBase1(dt,ppwl)                       Map cumulative net negative costs from dt+repay_term_dt(dt) -> dt using PWL approximation
 calcCroCostBase2(dt,ppwl)                       Compute lower part of rect-function for each of the PWL segments
 calcCroCostBase3(dt,ppwl)                       Compute lower part of rect-function for each of the PWL segments
 calcCroCostBase4(dt,ppwl)                       Compute upper part of rect-function for each of the PWL segments
 calcCroCostBase5(dt,ppwl)                       Compute upper part of rect-function for each of the PWL segments
 calcCroCostBase6(dt,ppwl)                       Multiply all mapped cumulative costs by rect-function to set values outside domains of each PWL segment to zero
 calcCroCostBase(dt)                             For each PWL segment sum up over all values (only one of which is >> 0)
 calcCroCostBaseDiff1(dt)                        Compute difference of mapped cumulative costs between two consecutive time steps to obtain CRO cost base
 calcCroCostBaseDiff2(dt)                        Compute difference of mapped cumulative costs between two consecutive time steps to obtain CRO cost base

 contrCost(dt)                                   Multiply CRO cost base by carbon debt interest rate and sum up and discount over repay term: dt -> repay_term_dt(dt)+dt

 defCumEmNetNegCost                              Compute total net negative costs (these should be equal to total CRO cost base costs)
 defCumCroCostBase                               Compute total CRO cost base costs (these should be equal to total net negative costs)


;


$ontext
This section of code describes the standard mitigation model:
Abatement levels for conventional abatement and an optional backstop (BS) are determined to reach a 2100 carbon budget constraint cost-effectively.
Integration over marginal abatement cost curves for conventional abatement and the BS is used to determine total abatement costs.
In the objective function total discounted abatement costs are minimized.
$offtext

 emNetMap(dt) .. em_net_dt(dt) =E= sum(t$( (ord(dt) lt 10*ord(t)+1) and (ord(dt) gt 10*(ord(t)-1)) ), em_net(t) + (ord(dt)-10*(ord(t)-1)-1)*(em_net(t+1)-em_net(t))/T_STEP_SIZE);

* MAC calculation  + backstop
 defEmAbRelBS(t)$(USE_BS eq 1) .. em_net(t) =E= EM_BASE(t)*(1-em_ab_rel(t)-em_ab_rel_bs(t));
 defEmAbRelNoBS(t)$(USE_BS eq 0) .. em_net(t) =E= EM_BASE(t)*(1-em_ab_rel(t));
 emAbRelMax(t) .. em_ab_rel(t) =L= AB_REL_MAX(t);
 emAbRelMin(t) .. em_ab_rel(t) =G= AB_REL_MIN(t);


 defMac(t) .. mac(t) =E= PAR_B(t)* (1/PAR_NU(t) *( ((PAR_L(t)-PAR_A(t))/(em_ab_rel(t)-PAR_A(t)))**PAR_NU(t)-1 ) )**PAR_C(t);
 defBS(t) .. em_ab_rel_bs(t) =E= (PAR_L_BS-PAR_A_BS)*(PAR_NU_BS*(mac(t)/PAR_B_BS)**(1/PAR_C_BS)+1)**(-1/PAR_NU_BS)+PAR_A_BS;

 defMacStepSize(t) .. mac_step_size(t) =E= em_ab_rel(t)/PMAC_NR;
 defMacStepLower(pmac,t) .. mac_step_lower(pmac,t) =E=PAR_B(t)* (1/PAR_NU(t) *( ((PAR_L(t)-PAR_A(t))/((ord(pmac)-1)*mac_step_size(t)-PAR_A(t)))**PAR_NU(t)-1 ) )**PAR_C(t);
 defMacStepUpper(pmac,t) ..  mac_step_upper(pmac,t) =E=PAR_B(t)* (1/PAR_NU(t) *( ((PAR_L(t)-PAR_A(t))/((ord(pmac))*mac_step_size(t)-PAR_A(t)))**PAR_NU(t)-1 ) )**PAR_C(t);
 defMacIntegrStep(pmac,t) .. mac_integr(pmac,t) =E= (mac_step_lower(pmac,t)+mac_step_upper(pmac,t))/2*mac_step_size(t);

 defBsStepSize(t) .. bs_step_size(t) =E= em_ab_rel_bs(t)/PMAC_NR;
 defBsStepLower(pmac,t) .. bs_step_lower(pmac,t) =E=PAR_B_BS * (1/PAR_NU_BS *( ((PAR_L_BS-PAR_A_BS)/((ord(pmac)-1)*bs_step_size(t)-PAR_A_BS))**PAR_NU_BS-1 ) )**PAR_C_BS;
 defBsStepUpper(pmac,t) ..  bs_step_upper(pmac,t) =E=PAR_B_BS* (1/PAR_NU_BS *( ((PAR_L_BS-PAR_A_BS)/((ord(pmac))*bs_step_size(t)-PAR_A_BS))**PAR_NU_BS-1 ) )**PAR_C_BS;
 defBsIntegrStep(pmac,t) .. bs_integr(pmac,t) =E= (bs_step_lower(pmac,t)+bs_step_upper(pmac,t))/2*bs_step_size(t);

 defTotCost(t) .. tot_cost(t) =E= sum(pmac, mac_integr(pmac,t) + bs_integr(pmac,t))*EM_BASE(t);
 totCostMap(dt) .. tot_cost_dt(dt) =E= sum(t$( (ord(dt) lt 10*ord(t)+1) and (ord(dt) gt 10*(ord(t)-1)) ), tot_cost(t) + (ord(dt)-10*(ord(t)-1)-1)*(tot_cost(t+1)-tot_cost(t))/T_STEP_SIZE);


* budget constraint + objective
 emSum1 .. sum(dt$(ord(dt) le DT_YEAR_2100), em_net_dt(dt)) =E= em_cum_2100;
 budgetConstr1 .. em_cum_2100 =E= BUDGET;

 objective .. sum(dt, tot_cost_dt(dt)  * 1/power(1+R, ord(dt)-1) + contr_cost_dt(dt)) =E= z;
 AbCostSum ..  sum(dt, tot_cost_dt(dt)  * 1/power(1+R, ord(dt)-1)) =E= ab_cost_sum;
 ContrCostSum ..  sum(dt,  contr_cost_dt(dt)) =E= contr_cost_sum;


$ontext
The remaining code is to compute CRO interest costs (contr_cost_dt):
         1. net emissions are disentangled into a net positive and a net negative part.
         2. the repay term (repay_term_dt(dt)) defining the time dependent period over which annual interest payments are done is computed
         3. the CRO cost base upon which the amount of annual interest payments depends, is computed
$offtext


* 1. Compute net positive/net negative emissions (both > 0!)
 defEmNetDt1(dt) .. em_net_pos_dt(dt) * em_net_neg_dt(dt) =E= 0;
 defEmNetDt2(dt) .. em_net_pos_dt(dt) - em_net_neg_dt(dt) =E= em_net_dt(dt);
 defPhi .. phi =E= sum(dt, em_net_neg_dt(dt))/(sum(dt, em_net_pos_dt(dt))+0.00000001);

* 2. Compute repayment term

$ontext
         A. Compute cumulative net positive emissions and net negative emissions.
         B. Express cumulative net negative emissions as piecewise linear function with PPWL_NR segments
                 (i) define nodes of PWL function
                 (ii) using (i) define slopes of PWL function
                 (iii) using (i) and (ii) define intercepts of PWL function

$offtext

  cumEmNetNeg(dt) .. cum_em_net_neg_dt(dt) =E= sum((dta)$(ord(dta) <= ord(dt)), em_net_neg_dt(dta) );
  cumEmNetPos(dt) .. cum_em_net_pos_dt(dt) =E= sum((dta)$(ord(dta) <= ord(dt)), em_net_pos_dt(dta) );

  cumEmNetNegPointsPWL(tpwl) ..  cum_em_net_neg_tpwl(tpwl) =E= sum(dt, cum_em_net_neg_dt(dt)$(ord(dt) eq TPWL_STEP_SIZE*(ord(tpwl)-1)+1 ) ) ;
  defCumEmNetNegSlopesPWL(ppwl) .. slope_cum_em_net_neg_ppwl(ppwl) =E= sum(tpwl$(ord(tpwl) eq ord(ppwl)), (cum_em_net_neg_tpwl(tpwl+1)-cum_em_net_neg_tpwl(tpwl))/TPWL_STEP_SIZE);
  defCumEmNetNegInterceptsPWL(ppwl) .. intercept_cum_em_net_neg_ppwl(ppwl) =E= sum(tpwl$(ord(tpwl) eq ord(ppwl)), cum_em_net_neg_tpwl(tpwl)-slope_cum_em_net_neg_ppwl(ppwl)* (TPWL_STEP_SIZE*(ord(tpwl)-1)+1) );

$ontext
         The repayment term maps cum_em_net_pos_dt(dt)*phi to cum_em_net_neg_dt(dt). This is done as follows:
         The start of the repayment term repay_term_dt(dt) is x1(dt)=ord(dt). The amount of emissions for which interest needs to be paid over the repayment term starting at x1
         is  y(x1) = cum_em_net_pos_dt(dt)*phi.
         y is inserted into the inverse pwl function of cum_em_net_neg_dt to obtain the end x2 of the repayment term at dt.
         However, for each y each of the PWL segments (defined by slope and intercept) produces a different x2, but only one x2 is within the correct domain of its pwl segment.
         Therefore each x2 is multiplied with a rect-function that equals 1 on the interval of the respective PWL segment and 0 on all other segments.

         In equation "calcRepayTerm1" slopes are in the denominator, however, cumulative net negative emissions are flat in the beginning, implying slopes = 0.
         Slopes and intercepts of such flat segments are adjusted such that they map into segments outside the planning horizon
         (i.e. very small slope of 0.001 and and intercept of -1 are used, implying that for y=0 -> x=1000 (years), i.e. this is outside the planning horizon which has standard 81 years (2020 to 2100)
         For longer planning horizons the intercept needs to be further reduced, at the cost of adding a larger error to the rest of the pwl function
         (the error added with the standard setting is O(-10) in slopes and O(-8) in intercepts).

         Rect functions are constructed using two step functions made of arctan, hence, rect functions are continuous.
         At the intersect of two segments the sum of rect functions (see eq. "calcRepayTerm") = 1, i.e. x2 is a weighted average of the two intersecting pwl segments.
         The first and last segment need to be expanded due to this continuity (see eqs. "calcRepayTerm2" and "calcRepayTerm5").

$offtext


  calcRepayTerm1(dt,ppwl) ..  repay_term_all_dt_ppwl(dt,ppwl) =E= (cum_em_net_pos_dt(dt)*phi-(intercept_cum_em_net_neg_ppwl(ppwl))-PLH_NR*EPS_REPAYTERM)/(slope_cum_em_net_neg_ppwl(ppwl)+EPS_REPAYTERM);

  calcRepayTerm2(dt,ppwl)$(ord(ppwl) eq 1) .. step_lower_repay_term_dt_ppwl(dt,ppwl) =E=   (1/pi*(arctan((repay_term_all_dt_ppwl(dt,ppwl)-(TPWL_STEP_SIZE*(ord(ppwl)-1)+0.5))/EPS_ARCTAN)+pi/2));
  calcRepayTerm3(dt,ppwl)$(ord(ppwl) > 1) .. step_lower_repay_term_dt_ppwl(dt,ppwl) =E=   (1/pi*(arctan((repay_term_all_dt_ppwl(dt,ppwl)-(TPWL_STEP_SIZE*(ord(ppwl)-1)+1))/EPS_ARCTAN)+pi/2));

  calcRepayTerm4(dt,ppwl)$(ord(ppwl) < PPWL_NR) .. step_upper_repay_term_dt_ppwl(dt,ppwl) =E=   (1-1/pi*(arctan((repay_term_all_dt_ppwl(dt,ppwl)-(TPWL_STEP_SIZE*(ord(ppwl))+1))/EPS_ARCTAN)+pi/2));
  calcRepayTerm5(dt,ppwl)$(ord(ppwl) eq PPWL_NR) .. step_upper_repay_term_dt_ppwl(dt,ppwl) =E=   (1-1/pi*(arctan((repay_term_all_dt_ppwl(dt,ppwl)-(TPWL_STEP_SIZE*(ord(ppwl))+1.5))/EPS_ARCTAN)+pi/2));

  calcRepayTerm6(dt,ppwl) .. repay_term_trunc_dt_ppwl(dt,ppwl) =E=  step_upper_repay_term_dt_ppwl(dt,ppwl) * step_lower_repay_term_dt_ppwl(dt,ppwl) * repay_term_all_dt_ppwl(dt,ppwl);
  calcRepayTerm(dt) .. repay_term_dt(dt) =E= sum(ppwl, repay_term_trunc_dt_ppwl(dt,ppwl));


* 3. Compute CRO cost base
$ontext
         In the next section CRO interest costs (contr_cost_dt) are computed by summing up discounted annual interest payments between dt and dt+repay_term_dt(dt) for each time step dt.
         Annual interest payments (in the continuous model description this is called "instantaneous interest payments"!) for each time step dt are computed by multiplying the carbon debt
         interest rate R_CL by the "CRO cost base" (cro_cost_base_diff_dt).
         The CRO cost base is derived from mapping net negative emission costs from dt+repay_term_dt(dt) -> dt. Net negative costs (em_net_neg_cost_dt) equal the fraction of total abatement costs
         attributable to net negative emissions (see equation "defEmNetNegCost"), i.e. average abatement costs times net negative emissions.
         For continuous time t, at t the CRO cost base equals em_net_neg_cost_dt(t+repay_term(t)). However, for discrete time steps dt, cumulative costs (see cum_em_net_neg_cost_dt in eq. "cumEmNetNegCost")
         are mapped from dt+repay_term(dt) -> dt to avoid discretization errors. Similar to equations for computing the repay term above, piece wise linear (PWL) functions which are multiplied by
         rect-functions (to restrict PWL function segments to their respective domains) are used to do the mapping.
         The CRO cost base equals the differences between the mapped cumulative costs (cro_cum_cost_base_dt(dt)-cro_cum_cost_base_dt(dt-1) in eq. "calcCroCostBaseDiff1").
         Aggregated costs for net negative emissions should equal the aggregated CRO cost base (see eqs. "defCumCroCostBase" and "defCumEmNetNegCost", which are only computed to manually check).
$offtext

 emAbRelMap(dt) .. em_ab_rel_dt(dt) =E= sum(t$( (ord(dt) lt 10*ord(t)+1) and (ord(dt) gt 10*(ord(t)-1)) ), em_ab_rel(t) + (ord(dt)-10*(ord(t)-1)-1)*(em_ab_rel(t+1)-em_ab_rel(t))/T_STEP_SIZE);
 emAbRelBsMap(dt) .. em_ab_rel_bs_dt(dt) =E= sum(t$( (ord(dt) lt 10*ord(t)+1) and (ord(dt) gt 10*(ord(t)-1)) ), em_ab_rel_bs(t) + (ord(dt)-10*(ord(t)-1)-1)*(em_ab_rel_bs(t+1)-em_ab_rel_bs(t))/T_STEP_SIZE);

 defEmNetNegCost(dt) .. em_net_neg_cost_dt(dt) =E= tot_cost_dt(dt)/((em_ab_rel_dt(dt)+USE_BS*em_ab_rel_bs_dt(dt))*EM_BASE_DT(dt) + 0.00000001 ) * em_net_neg_dt(dt) / SCALE_NET_NEG_COST;
 cumEmNetNegCost(dt) .. cum_em_net_neg_cost_dt(dt) =E= sum((dta)$(ord(dta) <= ord(dt)), em_net_neg_cost_dt(dta) );
 defCumEmNegCostPointsPWL(tpwl) ..  cum_em_net_neg_cost_tpwl(tpwl) =E= sum(dt, cum_em_net_neg_cost_dt(dt)$(ord(dt) eq TPWL_STEP_SIZE*(ord(tpwl)-1)+1 ) ) ;
 defCumEmNegCostSlopesPWL(ppwl) .. slope_cum_em_net_neg_cost_ppwl(ppwl) =E= sum(tpwl$(ord(tpwl) eq ord(ppwl)), (cum_em_net_neg_cost_tpwl(tpwl+1)-cum_em_net_neg_cost_tpwl(tpwl))/TPWL_STEP_SIZE);
 defCumEmNegCostInterceptsPWL(ppwl) .. intercept_cum_em_net_neg_cost_ppwl(ppwl) =E= sum(tpwl$(ord(tpwl) eq ord(ppwl)), cum_em_net_neg_cost_tpwl(tpwl)-slope_cum_em_net_neg_cost_ppwl(ppwl)* (TPWL_STEP_SIZE*(ord(tpwl)-1)+1) );


 calcCroCostBase1(dt,ppwl) ..  cro_cost_base_all_dt_ppwl(dt,ppwl) =E= repay_term_dt(dt)*slope_cum_em_net_neg_cost_ppwl(ppwl) + intercept_cum_em_net_neg_cost_ppwl(ppwl);
 calcCroCostBase2(dt,ppwl)$(ord(ppwl) eq 1) .. step_lower_cro_cost_base_dt_ppwl(dt,ppwl) =E=   (1/pi*(arctan((repay_term_dt(dt)-(TPWL_STEP_SIZE*(ord(ppwl)-1)+0.5))/EPS_ARCTAN)+pi/2));
 calcCroCostBase3(dt,ppwl)$(ord(ppwl) > 1) .. step_lower_cro_cost_base_dt_ppwl(dt,ppwl) =E=   (1/pi*(arctan((repay_term_dt(dt)-(TPWL_STEP_SIZE*(ord(ppwl)-1)+1))/EPS_ARCTAN)+pi/2));
 calcCroCostBase4(dt,ppwl)$(ord(ppwl) < PPWL_NR) .. step_upper_cro_cost_base_dt_ppwl(dt,ppwl) =E=   (1-1/pi*(arctan((repay_term_dt(dt)-(TPWL_STEP_SIZE*(ord(ppwl))+1))/EPS_ARCTAN)+pi/2));
 calcCroCostBase5(dt,ppwl)$(ord(ppwl) eq PPWL_NR) .. step_upper_cro_cost_base_dt_ppwl(dt,ppwl) =E=   (1-1/pi*(arctan((repay_term_dt(dt)-(TPWL_STEP_SIZE*(ord(ppwl))+1.5))/EPS_ARCTAN)+pi/2));
 calcCroCostBase6(dt,ppwl) .. cro_cum_cost_base_trunc_dt_ppwl(dt,ppwl) =E=  step_upper_cro_cost_base_dt_ppwl(dt,ppwl) * step_lower_cro_cost_base_dt_ppwl(dt,ppwl) * cro_cost_base_all_dt_ppwl(dt,ppwl);
 calcCroCostBase(dt) .. cro_cum_cost_base_dt(dt) =E= sum(ppwl, cro_cum_cost_base_trunc_dt_ppwl(dt,ppwl));

 calcCroCostBaseDiff1(dt)$(ord(dt) gt 1) .. cro_cost_base_diff_dt(dt) =E= (cro_cum_cost_base_dt(dt)-cro_cum_cost_base_dt(dt-1))*SCALE_NET_NEG_COST;
 calcCroCostBaseDiff2(dt)$(ord(dt) eq 1) .. cro_cost_base_diff_dt(dt) =E= cro_cum_cost_base_dt(dt)*SCALE_NET_NEG_COST;
 contrCost(dt) .. contr_cost_dt(dt) =E= (R_CL  * cro_cost_base_diff_dt(dt) * ((rPower(1+R, repay_term_dt(dt)-1)-1)/(rPower(1+R, repay_term_dt(dt)-1)*R) - (rPower(1+R, ord(dt)-2)-1)/(rPower(1+R, ord(dt)-2)*R)));


* check sums
 defCumCroCostBase .. cum_cro_cost_base =E= sum(dt, cro_cost_base_diff_dt(dt));
 defCumEmNetNegCost .. cum_em_net_neg_cost =E= sum(dt, em_net_neg_cost_dt(dt))*SCALE_NET_NEG_COST;





Model CRO /all/ ;
Option NLP = CONOPT;
CRO.OptFile = 1;
Solve CRO using NLP minimizing z;


display BUDGET, EM_BASE, AB_REL_LEVEL, EM_NET_LEVEL, MAC_LEVEL, PHI_LEVEL, PAR_A, PAR_L, AB_REL_MAX, AB_REL_MIN, R_CL, USE_BS, SCALE_NET_NEG_COST, PARS_BS, bs_step_size.lo,bs_step_size.up, em_ab_rel_bs.up, mac.up, mac.lo, mac.l

execute_unload 'RESULTS_OUT', em_net, em_ab_rel, em_ab_rel_bs, mac, emSum1, em_net_dt, R_CL, z, tot_cost, phi, cro_cost_base_diff_dt, repay_term_dt, contr_cost_dt, ab_cost_sum, contr_cost_sum
