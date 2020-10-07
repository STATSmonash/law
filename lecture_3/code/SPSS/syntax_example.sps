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

compute LOG_regdate_to_commencement_mths = LN(regdate_to_commencement_mths).
execute.
************************************************************************************************************************
* summarize and explore data.
************************************************************************************************************************

FREQUENCIES VARIABLES= Year time_to_med duration_mths family_context codeath birth2death_m grounds_no regdate_to_commencement_mths LOG_regdate_to_commencement_mths
  /FORMAT=NOTABLE
  /NTILES=10
  /STATISTICS=STDDEV MINIMUM MAXIMUM MEAN MEDIAN SKEWNESS SESKEW KURTOSIS SEKURT
  /ORDER=ANALYSIS.
EXECUTE.


CORRELATIONS
  /VARIABLES=Year time_to_med duration_mths family_context birth2death_m regdate_to_commencement_mths
  /PRINT=TWOTAIL NOSIG
  /MISSING=PAIRWISE.

NONPAR CORR
  /VARIABLES=Year time_to_med duration_mths family_context birth2death_m regdate_to_commencement_mths
  /PRINT=SPEARMAN TWOTAIL NOSIG
  /MISSING=PAIRWISE.




T-TEST GROUPS=family_context(0 1)
  /MISSING=ANALYSIS
  /VARIABLES= duration_mths time_to_med Year birth2death_m grounds_no regdate_to_commencement_mths LOG_regdate_to_commencement_mths
  /CRITERIA=CI(.95).
EXECUTE.


NPAR TESTS
  /M-W= duration_mths time_to_med Year birth2death_m  regdate_to_commencement_mths LOG_regdate_to_commencement_mths BY family_context(0 1)
  /MISSING ANALYSIS
  /METHOD=EXACT TIMER(5).


T-TEST GROUPS=codeath(0 1)
  /MISSING=ANALYSIS
  /VARIABLES= duration_mths time_to_med Year birth2death_m grounds_no regdate_to_commencement_mths LOG_regdate_to_commencement_mths
  /CRITERIA=CI(.95).
EXECUTE.


EXAMINE  VARIABLES=regdate_to_commencement_mths BY codeath
  /PLOT BOXPLOT STEMLEAF
  /COMPARE GROUP
  /STATISTICS DESCRIPTIVES
  /CINTERVAL 95
  /MISSING LISTWISE
  /NOTOTAL.



NPAR TESTS
  /M-W= duration_mths time_to_med Year birth2death_m  regdate_to_commencement_mths BY codeath(0 1)
  /MISSING ANALYSIS
  /METHOD=EXACT TIMER(5).


GRAPH
/SCATTERPLOT(BIVAR)= duration_mths  WITH time_to_med
/MISSING=LISTWISE.
EXECUTE.


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

  CROSSTABS
  /TABLES=  codeath BY gender_pl
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ PHI ETA CORR RISK
  /CELLS=COUNT
  /COUNT ROUND CELL
  /METHOD=EXACT TIMER(5).

 CROSSTABS
  /TABLES= DUM_recent codeath BY family_context
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ PHI ETA CORR RISK
  /CELLS=COUNT
  /COUNT ROUND CELL
  /METHOD=EXACT TIMER(5).

 CROSSTABS
  /TABLES=DUM_recent codeath dirs_no gender_pl BY family_context
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ PHI ETA CORR RISK
  /CELLS=COUNT
  /COUNT ROUND CELL
  /METHOD=EXACT TIMER(5).


 CROSSTABS
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
LOGISTIC REGRESSION VARIABLES codeath
  /METHOD=BSTEP(LR) LOG_regdate_to_commencement_mths family_context DUM_recent
  /CLASSPLOT
  /PRINT=GOODFIT CI(95)
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(30) CUT(0.5).

LOGISTIC REGRESSION VARIABLES codeath
  /METHOD=BSTEP(LR) LOG_regdate_to_commencement_mths family_context DUM_recent gender_pl 
  /CLASSPLOT
  /PRINT=GOODFIT CI(95)
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(30) CUT(0.5).



