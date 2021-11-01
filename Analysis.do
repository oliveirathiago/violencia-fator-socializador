cd // specify working directory
use // read data: SPLSS_W3.dta, clear

// Variaveis demograficas / de controle

encode Dados_Aluno_18, gen(female)
	recode female 2=0 // variavel sexo
	
label define corlbl 1 "Branca" 2 "Preta" 3 "Parda" 4 "Amarela" 5 "Indigena"
label values cor corlbl

gen brancos = 0
	replace brancos=1 if cor==1 // variavel dummy para cor (1 = branca)
	
gen pretos = 0
	replace pretos = 1 if cor==2 // variavel dummy para cor (1 = preta)
	
replace RENDA_ATUAL=. if RENDA_ATUAL== 97 
	
encode Dados_Escola_1, gen(escola) // variavel escola
xtset escola // variavel nivel-2

// Modelo geral

gen cont_dir1 = 0
replace cont_dir1=1 if P3103>0
gen cont_dir2 = 0
replace cont_dir2=1 if P3104>0
gen cont_dir3 = 0
replace cont_dir3=1 if P3105>0

gen cont_dir = 0
replace cont_dir=1 if cont_dir1==1 | cont_dir2==1 | cont_dir3==1

gen cont_ind_l1 = 0
replace cont_ind_l1 = 1 if P2901 > 0
gen cont_ind_l2 = 0
replace cont_ind_l2 = 1 if P2902 > 0

gen cont_ind_l = 0
replace cont_ind_l = 1 if cont_ind_l1==1 | cont_ind_l2==1

gen cont_ind_v1 = 0
replace cont_ind_v1 = 1 if P2903 > 0
gen cont_ind_v2 = 0
replace cont_ind_v2 = 1 if P2904 > 0

gen cont_ind_v = 0
replace cont_ind_v = 1 if cont_ind_v1==1 | cont_ind_v2==1

// VITIMIZACAO POLICIAL
	gen vit_pol1 = 0
		replace vit_pol1=1 if P3001>0 //dicotomizando pq empiricamente nao faz sentido
							// diferenciar entre uma, poucas e muitas vezes
	gen vit_pol2 = 0
		replace vit_pol2=1 if P3002>0

	gen vit_pol3 = 0
		replace vit_pol3=1 if P3003>0

	gen vit_pol = 0
		replace vit_pol=1 if vit_pol1==1 | vit_pol2==1 | vit_pol3==1

// VITIMIZACAO
	gen vitim1 = 0
		replace vitim1=1 if P2801>0

	gen vitim2 = 0
		replace vitim2=1 if P2802>0

	gen vitim3 = 0
		replace vitim3=1 if P2803>0
		
	gen vitim = 0
		replace vitim=1 if vitim1==1 | vitim2==1 | vitim3==1

	/*
		Modelos de mensuracao: 
	*/
	
	// Confianca (pj e eff - separado ou nao?)
	
	sem (conf -> P3201-P3204 P3401-P3404), var(conf@1) method(mlmv) ///
		nocapslatent latent(conf)
		estat gof, stats(all)
		
	sem (pj -> P3201-P3204) (eff -> P3401-P3404), var(pj@1 eff@1) ///
		method(mlmv) nocapslatent latent(pj eff)
		estat gof, stats(all)
	
	// Legitimidade (dever e alinhamento - separado ou nao?)
	
	sem (leg -> P3301-P3303 P3304 P3305), ///
		var(leg@1) method(mlmv) nocapslatent latent(leg)
		estat gof, stats(all)
		
	sem (dever -> P3301-P3303) (normid -> P3304 P3305), ///
		var(dever@1 normid@1) method(mlmv) nocapslatent latent(dever normid)
		estat gof, stats(all)
		
	// One-factor exp vio
	
	sem (exp_vio -> P2701-P2704 P1001 P1002), ///
		var(exp_vio@1) method(mlmv) nocapslatent latent(exp_vio)
		estat gof, stats(all)
		
	// Five-factor
		
	sem (dever -> P3301-P3303) (normid -> P3304 P3305) ///
		(pj -> P3201-P3204) (eff -> P3401-P3404) ///
		(exp_vio -> P2701-P2704 P1001 P1002), ///
			var(dever@1 normid@1 pj@1 eff@1 exp_vio@1) ///
				method(mlmv) nocapslatent ///
				latent(dever normid pj eff exp_vio)
					estat gof, stats(all)
				
	/*
		Modelo estrutural
	*/
	
	// Sem clustered standard errors
		sem (dever -> P3301-P3303) (normid -> P3304 P3305) ///
		(pj -> P3201-P3204) (eff -> P3401-P3404) ///
		(exp_vio -> P2701-P2704 P1001 P1002) ///
			(dever normid pj eff exp_vio <- female brancos RENDA_ATUAL) ///
			(pj eff <- exp_vio vitim cont_dir cont_ind_l cont_ind_v vit_pol) ///
			(dever normid <- pj eff exp_vio vitim cont_dir cont_ind_l cont_ind_v vit_pol), ///
				var(e.dever@1 e.normid@1 e.pj@1 e.eff@1 e.exp_vio@1) ///
				method(mlmv) nocapslatent ///
				latent(dever normid pj eff exp_vio)

		estat gof, stats(all) // model fit
		
			/*	
				AIC |  36257.840 
				BIC |  36918.050 
			*/			
			
	
	// Com clustered standard errors
		sem (dever -> P3301-P3303) (normid -> P3304 P3305) ///
		(pj -> P3201-P3204) (eff -> P3401-P3404) ///
		(exp_vio -> P2701-P2704 P1001 P1002) ///
			(dever normid pj eff exp_vio <- female brancos RENDA_ATUAL) ///
			(pj eff <- exp_vio vitim cont_dir cont_ind_l cont_ind_v vit_pol) ///
			(dever normid <- pj eff exp_vio vitim cont_dir cont_ind_l cont_ind_v vit_pol), ///
				var(e.dever@1 e.normid@1 e.pj@1 e.eff@1 e.exp_vio@1) ///
				method(mlmv) nocapslatent vce(cluster escola) ///
				latent(dever normid pj eff exp_vio)
	estat ic
	
			/*
				AIC | 36191.84 
				BIC | 36700.75
			*/
	
	estat teffects // indirect effects

	
	/*
	Efeitos indiretos
	
	contindv via PJ sobre dever:  .2890117 * -.7521577
	contindv via EFF sobre dever: .2613641 * -.300925
	contindv via PJ sobre ID:  .8025138 * -.7521577
	contindv via EFF sobre ID: .4934968 *  -.300925
	
	exp via PJ sobre dever: .2890117 *  -.1848631
	exp via PJ sobre id: .8025138 * -.1848631
	
	vitpol via PJ sobre dever: .2890117 * -.8243095
	vitpol via PJ sobre id: .8025138 * -.8243095
	/*
