FOR /L %%A IN (1,1,50) DO (
  gams investcge.gms --pvalue=0 --base=1 --climate="run%%A" --results_dir="results/"
)


FOR /L %%A IN (1,1,50) DO (
 gams investcge.gms --pvalue_A=33 --base=0 --fin_gsave_A=0.1818 --fin_fbor_A=0.8182 --interest_A=0.02 --repayper_A=20 --pvalue_B=29.2 --fin_fbor_B=1 --interest_B=0.045 --repayper_B=12 --flabel="loans-exports" --loanprod_A=9.588 --climate="run%%A" --results_dir="results/"

)

FOR /L %%A IN (1,1,50) DO (
 gams investcge.gms --pvalue_A=33 --base=0 --fin_gsave_A=0.1818 --fin_fbor_A=0.8182 --interest_A=0.02 --repayper_A=20 --pvalue_B=29.2 --fin_fbor_B=1 --interest_B=0.045 --repayper_B=12 --flabel="loans" --loanprod_A=0 --climate="run%%A" --results_dir="results/"

)


FOR /L %%A IN (1,1,50) DO (
 gams investcge.gms --pvalue_A=0 --base=0 --fin_gsave_A=1 --interest_A=0.02 --repayper_A=20 --flabel="energy-only" --climate="run%%A" --results_dir="results/"

)
