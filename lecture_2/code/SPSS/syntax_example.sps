* Encoding: UTF-8.

************************************************************************************************************************
* clean data, account for missing values and make new variables.
************************************************************************************************************************

 * if(time_to_med = 999) time_to_med = $SYSMIS.
 * execute.

* alter type family_context(f1).
 
if(year <= 2012) DUM_recent = 0.
if(year > 2012) DUM_recent = 1.
execute.

 * if(grounds_no EQ "unknown") grounds_no=$SYSMIS.
 * execute.
 *  alter type grounds_no(f3).

if(sh_change_precase EQ '') sh_change_precase='999'.
execute.
missing values sh_change_precase('999').
execute.



compute LOG_regdate_to_commencement_mths = LN(regdate_to_commencement_mths).
execute.

compute LOG_dur_mths = LN(dur_mths+.0001).
execute.

if(reg_binary EQ "deregistered") reg_binary_DUM = 0.
if(reg_binary EQ "registered") reg_binary_DUM = 1.
execute.
add value labels reg_binary_DUM 0'deregistered' . 1'registered'.
execute.

if(family_context EQ "no") family_context_DUM = 0.
if(family_context EQ "yes") family_context_DUM = 1.
add value labels family_context_DUM 0'not-family' 1'family' .
execute.

*rel_s461 sh_change_precase disposition buscat
*time_to_med reg_binary_DUM family_context_DUM

COMPUTE test=EXP(LOG_regdate_to_commencement_mths)-regdate_to_commencement_mths.
EXECUTE.


************************************************************************************************************************
* summarize and explore data.
************************************************************************************************************************

FREQUENCIES VARIABLES= year dur_mths LOG_dur_mths dh_no no_pl_affis no_dirs_case_start regdate_to_commencement_mths LOG_regdate_to_commencement_mths 
   /FORMAT=NOTABLE
  /NTILES=10
  /STATISTICS=STDDEV MINIMUM MAXIMUM MEAN MEDIAN SKEWNESS SESKEW KURTOSIS SEKURT
  /ORDER=ANALYSIS.
EXECUTE.




CORRELATIONS
  /VARIABLES=year dur_mths LOG_dur_mths dh_no no_pl_affis no_dirs_case_start regdate_to_commencement_mths LOG_regdate_to_commencement_mths 
  /PRINT=TWOTAIL NOSIG
  /MISSING=PAIRWISE.

NONPAR CORR
  /VARIABLES=year dur_mths LOG_dur_mths dh_no no_pl_affis no_dirs_case_start regdate_to_commencement_mths LOG_regdate_to_commencement_mths 
  /PRINT=SPEARMAN TWOTAIL NOSIG
  /MISSING=PAIRWISE.



T-TEST GROUPS=family_context("no" "yes")
  /MISSING=ANALYSIS
  /VARIABLES=year dur_mths LOG_dur_mths dh_no no_pl_affis no_dirs_case_start regdate_to_commencement_mths LOG_regdate_to_commencement_mths 
       /CRITERIA=CI(.95).
EXECUTE.

T-TEST GROUPS=family_context_DUM(0 1)
  /MISSING=ANALYSIS
  /VARIABLES= year dur_mths LOG_dur_mths dh_no no_pl_affis no_dirs_case_start regdate_to_commencement_mths LOG_regdate_to_commencement_mths 
  /CRITERIA=CI(.95).
EXECUTE.


NPAR TESTS
  /M-W=year dur_mths LOG_dur_mths dh_no no_pl_affis no_dirs_case_start regdate_to_commencement_mths LOG_regdate_to_commencement_mths  BY family_context_DUM(0 1)
  /MISSING ANALYSIS
  /METHOD=EXACT TIMER(5).


T-TEST GROUPS=reg_binary_DUM(0 1)
  /MISSING=ANALYSIS
  /VARIABLES= year dur_mths LOG_dur_mths dh_no no_pl_affis no_dirs_case_start regdate_to_commencement_mths LOG_regdate_to_commencement_mths
  /CRITERIA=CI(.95).
EXECUTE.


EXAMINE  VARIABLES=regdate_to_commencement_mths BY reg_binary_DUM
  /PLOT BOXPLOT STEMLEAF
  /COMPARE GROUP
  /STATISTICS DESCRIPTIVES
  /CINTERVAL 95
  /MISSING LISTWISE
  /NOTOTAL.



NPAR TESTS
  /M-W= year dur_mths LOG_dur_mths dh_no no_pl_affis no_dirs_case_start regdate_to_commencement_mths LOG_regdate_to_commencement_mths BY reg_binary_DUM(0 1)
  /MISSING ANALYSIS
  /METHOD=EXACT TIMER(5).


GRAPH
/SCATTERPLOT(BIVAR)= dur_mths  WITH time_to_med
/MISSING=LISTWISE.
EXECUTE.



CROSSTABS
  /TABLES=  reg_binary_DUM family_context_DUM BY gender_pl
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ PHI ETA CORR RISK
  /CELLS=COUNT
  /COUNT ROUND CELL
  /METHOD=EXACT TIMER(5).


CROSSTABS
  /TABLES=    reg_binary_DUM BY  sh_change_precase  
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ PHI ETA CORR RISK
  /CELLS=COUNT
  /COUNT ROUND CELL
  /METHOD=EXACT TIMER(5).
	



* Chart Builder.
 * GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=duration_mths MISSING=LISTWISE REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE.
 * BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: duration_mths=col(source(s), name("duration_mths"))
  GUIDE: axis(dim(1), label("duration_mths"))
  GUIDE: axis(dim(2), label("Frequency"))
  GUIDE: text.title(label("Simple Histogram of duration_mths"))
  ELEMENT: interval(position(summary.count(bin.rect(duration_mths))), shape.interior(shape.square))
END GPL.

* Chart Builder.
 * GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=regdate_to_commencement_mths codeath MISSING=LISTWISE
    REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE.
 * BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: regdate_to_commencement_mths=col(source(s), name("regdate_to_commencement_mths"))
  DATA: codeath=col(source(s), name("codeath"), unit.category())
  GUIDE: axis(dim(1), label("regdate_to_commencement_mths"))
  GUIDE: axis(dim(2), label("Frequency"))
  GUIDE: legend(aesthetic(aesthetic.color.interior), label("codeath"))
  GUIDE: text.title(label("Stacked Histogram of regdate_to_commencement_mths by codeath"))
  ELEMENT: interval.stack(position(summary.count(bin.rect(regdate_to_commencement_mths))),
    color.interior(codeath), shape.interior(shape.square))
END GPL.

 
 *  CROSSTABS
  /TABLES= DUM_recent codeath BY family_context
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ PHI ETA CORR RISK
  /CELLS=COUNT
  /COUNT ROUND CELL
  /METHOD=EXACT TIMER(5).

 *  CROSSTABS
  /TABLES=DUM_recent codeath dirs_no gender_pl BY family_context
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ PHI ETA CORR RISK
  /CELLS=COUNT
  /COUNT ROUND CELL
  /METHOD=EXACT TIMER(5).


 *  CROSSTABS
  /TABLES=DUM_recent  dirs_no gender_pl BY codeath
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ PHI ETA CORR RISK
  /CELLS=COUNT
  /COUNT ROUND CELL
  /METHOD=EXACT TIMER(5).

************************************************************************************************************************
* a regression model?
************************************************************************************************************************

* same factors should show up as significant (but weird subtle things can block this!).
LOGISTIC REGRESSION VARIABLES codeath
  /METHOD=ENTER     regdate_to_commencement_mths family_context
  /CLASSPLOT
  /PRINT=GOODFIT CI(95)
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(30) CUT(0.5).


* same factors should show up as significant (but weird subtle things can block this!).
 * LOGISTIC REGRESSION VARIABLES codeath
  /METHOD=BSTEP(LR) LOG_regdate_to_commencement_mths family_context DUM_recent
  /CLASSPLOT
  /PRINT=GOODFIT CI(95)
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(30) CUT(0.5).

 * LOGISTIC REGRESSION VARIABLES codeath
  /METHOD=BSTEP(LR) LOG_regdate_to_commencement_mths family_context DUM_recent gender_pl 
  /CLASSPLOT
  /PRINT=GOODFIT CI(95)
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(30) CUT(0.5).



