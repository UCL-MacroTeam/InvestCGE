set year /2007*2067/
*    yt   /2008*2067/
    run  /Run1*Run50/ ;

parameter h_output(*,year,run)   Hydropower output - merged scenarios
          delta_h_output(*,year,run) Changes in hydro output relative to the baseline
          b_output(year,run)     Bui Dam output
          a_base_output(year,run) Akosombo Dam output baseline
          a_bui_output(year, run) Akosombo Dam output with Bui


$call gdxxrw "MB - Hydropower results v3.xlsx" o=hydro_scenarios par=B_Output Rng='Bui!A1:BB64' rdim=1 cdim=1 par=a_base_output Rng='Akosombo_Bui_offline!A1:BB64' rdim=1 cdim=1 par=a_bui_output Rng='Akosombo_Bui_online!A1:BB64' rdim=1 cdim=1
$gdxin hydro_scenarios
$load b_output a_base_output a_bui_output

display b_output, a_base_output, a_bui_output;

h_output("akosombo_baseline",year,run) = a_base_output(year,run) ;
h_output("akosombo_with_bui",year,run) = a_bui_output(year,run) ;
h_output("bui",year,run) = b_output(year,run) ;
h_output("total_with_bui",year,run) = h_output("akosombo_with_bui",year,run) + h_output("bui",year,run) ;

delta_h_output("total",year,run) = (h_output("akosombo_with_bui",year,run)+h_output("bui",year,run))/h_output("akosombo_baseline",year,run)   ;
delta_h_output("akosombo",year,run) = h_output("akosombo_with_bui",year,run)/h_output("akosombo_baseline",year,run)                           ;
delta_h_output("bui",year,run) = h_output("bui",year,run)/h_output("akosombo_baseline",year,run)                                              ;

delta_h_output("akosombo_base",year,run) = h_output("akosombo_baseline",year,run) / h_output("akosombo_baseline","2011",run)                 ;

execute_unload "hydro_output.gdx" h_output, delta_h_output ;

