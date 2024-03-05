// Delete plot1
// graph drop plot1

// Delete plot2
// graph drop plot2

// Delete plot3
// graph drop plot3

// logistic regression NH_BlackP
logit fatal_enc_binary NH_BlackP

margins, at(NH_BlackP = (0(.05)1)) noatleg


marginsplot,  title("Predicted Probability of a LUOF, 2015-2020") ///
			  xtitle("Proportion black in the census tract") ///
              ytitle("Probability of a LUOF") ///
              name(plot1, replace) ///
			  yscale(range(0 0.18)) ///
			  ylabel(0 "0" 0.02 "0.02" 0.04 "0.04" 0.06 "0.06" 0.08 "0.08" 0.10 "0.10" 0.12 "0.12" 0.14 "0.14" 0.16 "0.16" 0.18 "0.18")
			  // title("Predicted Probability of a LUOF, 2015-2020") ///

// logistic regression NH_WhiteP
logit fatal_enc_binary NH_WhiteP

margins, at(NH_WhiteP = (0(.05)1)) noatleg

marginsplot, title("Predicted Probability of a LUOF, 2015-2020") ///
              xtitle("Proportion white in census tract") ///
			  ytitle("Probability of a LUOF") ///
              name(plot2, replace) ///
			  yscale(range(0 0.18)) ///
			  ylabel(0 "0" 0.02 "0.02" 0.04 "0.04" 0.06 "0.06" 0.08 "0.08" 0.10 "0.10" 0.12 "0.12" 0.14 "0.14" 0.16 "0.16" 0.18 "0.18")

// logistic regression Hisp_LatinoP
logit fatal_enc_binary Hisp_LatinoP

margins, at(Hisp_LatinoP = (0(.05)1)) noatleg

marginsplot,  title("Predicted Probability of a LUOF, 2015-2020") ///
              xtitle("Proportion Hispanic/Latino in census tract") ///
			  ytitle("Probability of a LUOF") ///
              name(plot3, replace) ///
			  yscale(range(0 0.18)) ///
			  ylabel(0 "0" 0.02 "0.02" 0.04 "0.04" 0.06 "0.06" 0.08 "0.08" 0.10 "0.10" 0.12 "0.12" 0.14 "0.14" 0.16 "0.16" 0.18 "0.18")

// Combine plots
// graph combine plot1 plot2 plot3, xcommon ycommon cols(3) xsize(12) ysize(4)
