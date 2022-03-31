*Random-effects meta-analysis
gen logOR=log(aor_rr_irr_hr) 
gen logLci=log(lowerci) 
gen logUci=log(upperci) 
gen selogOR=(log(upperci)-log(lowerci))/3.92 

sort Design Year Cases

label variable authoryearstudylocation "First author (year,country)"
label variable sitesex "Tumour location, gender"
label variable ncases "Study size (CRC cases)"

metan logOR selogOR, randomi eform label(namevar= authoryearstudylocation, yearvar= year ) lcols( authoryearstudylocation sitesex ncases) textsize(170) boxsca(70) xlabel(0.5,1.5) notable xsize(10) ysize(8) favours(Statins reduce risk of CRC # Statins increase risk of CRC) astext(50) nowarning boxopt(mcolor(gs12)) diamopt(lcolor(red) lwidth(medium)) pointopt(msymbol(circle)) olineopt(lcolor(red) lpattern(shortdash_dot)) ciopt(lcolor(gs1)) by( design )

*Funnelplot*
metafunnel logOR selogOR, eform by (design) ytitle (Standard error of LogES) xtitle(Effect Size)subtitle(Funnel plot with pseudo 95% confidence limits)

